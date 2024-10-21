module SPC
  ( -- * SPC startup
    SPC,
    startSPC,

    -- * Job functions
    Job (..),
    JobId,
    JobStatus (..),
    JobDoneReason (..),
    jobAdd,
    jobStatus,
    jobWait,
    jobCancel,

    -- * Worker functions
    WorkerName,
    workerAdd,
    workerStop,
  )
where

import Control.Concurrent
  ( ThreadId,
    forkIO,
    killThread,
    threadDelay,
  )
import Control.Exception (SomeException, catch)
import Control.Monad (ap, forM_, forever, liftM, void, when)
import Data.List (partition)
import GenServer
import System.Clock.Seconds (Clock (Monotonic), Seconds, getTime)

-- First some general utility functions.

-- | Retrieve Unix time using a monotonic clock. You cannot use this
-- to measure the actual world time, but you can use it to measure
-- elapsed time.
getSeconds :: IO Seconds
getSeconds = getTime Monotonic

-- | Remove mapping from association list.
removeAssoc :: (Eq k) => k -> [(k, v)] -> [(k, v)]
removeAssoc needle ((k, v) : kvs) =
  if k == needle
    then kvs
    else (k, v) : removeAssoc needle kvs
removeAssoc _ [] = []

-- Then the definition of the glorious SPC.

-- | A job that is to be enqueued in the glorious SPC.
data Job = Job
  { -- | The IO action that comprises the actual action of the job.
    jobAction :: IO (),
    -- | The maximum allowed runtime of the job, counting from when
    -- the job begins executing (not when it is enqueued).
    jobMaxSeconds :: Int
  }

-- | A unique identifier of a job that has been enqueued.
newtype JobId = JobId Int
  deriving (Eq, Ord, Show)

-- | How a job finished.
data JobDoneReason
  = -- | Normal termination.
    Done
  | -- | The job was killed because it ran for too long.
    DoneTimeout
  | -- | The job was explicitly cancelled, or the worker
    -- it was running on was stopped.
    DoneCancelled
  | -- | The job crashed due to an exception.
    DoneCrashed
  deriving (Eq, Ord, Show)

-- | The status of a job.
data JobStatus
  = -- | The job is done and this is why.
    JobDone JobDoneReason
  | -- | The job is still running.
    JobRunning
  | -- | The job is enqueued, but is waiting for an idle worker.
    JobPending
  | -- | A job with this ID is not known to this SPC instance.
    JobUnknown
  deriving (Eq, Ord, Show)

-- | A worker decides its own human-readable name. This is useful for
-- debugging.
type WorkerName = String

data WorkerStatus
  = WorkerIdle
  | WorkerBusy
  deriving (Eq, Ord, Show)

-- | Messages sent to workers. These are sent both by SPC and by
-- processes spawned by the workes.
data WorkerMsg
  = -- | The worker should start executing the job.
    WorkerMsgStartJob Job
  | WorkerMsgJobDone JobDoneReason
  | WorkerMsgCancelJob
  | WorkerMsgJobTimedOut
  | WorkerMsgJobCrashed
  | WorkerMsgShutDown

-- Messages sent to SPC.
data SPCMsg
  = -- | Add the job, and reply with the job ID.
    MsgJobAdd Job (ReplyChan JobId)
  | -- | Cancel the given job.
    MsgJobCancel JobId
  | -- \| Immediately reply the status of the job.
    MsgJobStatus JobId (ReplyChan JobStatus)
  | -- | Reply when the job is done.
    MsgJobWait JobId (ReplyChan (Maybe JobDoneReason))
  | -- | Some time has passed.
    MsgTick
  | MsgWorkerAdd WorkerName Worker
  | MsgDoesWorkerExist WorkerName (ReplyChan Bool)
  | MsgWorkerJobDone WorkerName JobDoneReason
  | MsgWorkerShutDown WorkerName

-- | A handle to the SPC instance.
newtype SPC = SPC (Server SPCMsg)

-- | A handle to a worker.
newtype Worker = Worker (Server WorkerMsg)

-- | The central state. Must be protected from the bourgeoisie.
data SPCState = SPCState
  { spcJobsPending :: [(JobId, Job)],
    spcJobsRunning :: [(JobId, (WorkerName, Seconds))],
    spcJobsDone :: [(JobId, JobDoneReason)],
    spcJobCounter :: JobId,
    spcWaiting :: [(JobId, ReplyChan (Maybe JobDoneReason))],
    spcIdleWorkers :: [(WorkerName, Worker)],
    spcBusyWorkers :: [(WorkerName, Worker)],
    spcWorkerJobs :: [(WorkerName, JobId)]
    -- TODO: you will need to add more fields.
  }

-- | The monad in which the main SPC thread runs. This is a state
-- monad with support for IO.
newtype SPCM a = SPCM (SPCState -> IO (a, SPCState))

instance Functor SPCM where
  fmap = liftM

instance Applicative SPCM where
  pure x = SPCM $ \state -> pure (x, state)
  (<*>) = ap

instance Monad SPCM where
  SPCM m >>= f = SPCM $ \state -> do
    (x, state') <- m state
    let SPCM f' = f x
    f' state'

-- | Retrieve the state.
get :: SPCM SPCState
get = SPCM $ \state -> pure (state, state)

-- | Overwrite the state.
put :: SPCState -> SPCM ()
put state = SPCM $ \_ -> pure ((), state)

-- | Modify the state.
modify :: (SPCState -> SPCState) -> SPCM ()
modify f = do
  state <- get
  put $ f state

-- | Lift an 'IO' action into 'SPCM'.
io :: IO a -> SPCM a
io m = SPCM $ \state -> do
  x <- m
  pure (x, state)

-- | Run the SPCM monad.
runSPCM :: SPCState -> SPCM a -> IO a
runSPCM state (SPCM f) = fst <$> f state

schedule :: SPCM ()
schedule = do
  state <- get
  case (spcJobsRunning state, spcJobsPending state, spcIdleWorkers state) of
    (runningJobs, (jobid, job) : jobs, (wName, Worker workerServer) : workers) -> do
      now <- io getSeconds
      let deadline = now + fromIntegral (jobMaxSeconds job)
      put $
        state
          { spcJobsRunning = (jobid, (wName, deadline)) : runningJobs,
            spcJobsPending = jobs,
            spcIdleWorkers = workers,
            spcBusyWorkers = (wName, Worker workerServer) : spcBusyWorkers state,
            spcWorkerJobs = (wName, jobid) : spcWorkerJobs state
          }
      io $ sendTo workerServer $ WorkerMsgStartJob job
    _ -> pure ()

jobDone :: JobId -> JobDoneReason -> SPCM ()
jobDone jobid reason = do
  state <- get
  case lookup jobid $ spcJobsDone state of
    Just _ -> pure ()
    Nothing -> do
      let (waiting, rest) = partition ((== jobid) . fst) $ spcWaiting state
      forM_ waiting $ \(_, r) -> io $ reply r $ Just reason
      case lookup jobid $ spcJobsRunning state of
        Just (wName, _) -> do
          case lookup wName $ spcBusyWorkers state of
            Just (Worker workerServer) -> do
              put $
                state
                  { spcWaiting = rest,
                    spcJobsDone = (jobid, reason) : spcJobsDone state,
                    spcJobsRunning = removeAssoc jobid $ spcJobsRunning state,
                    spcBusyWorkers = removeAssoc wName $ spcBusyWorkers state,
                    spcWorkerJobs = removeAssoc wName $ spcWorkerJobs state,
                    spcJobsPending = removeAssoc jobid $ spcJobsPending state,
                    spcIdleWorkers = (wName, Worker workerServer) : spcIdleWorkers state
                  }
            Nothing -> do
              put $
                state
                  { spcWaiting = rest,
                    spcJobsDone = (jobid, reason) : spcJobsDone state,
                    spcJobsRunning = removeAssoc jobid $ spcJobsRunning state,
                    spcWorkerJobs = removeAssoc wName $ spcWorkerJobs state,
                    spcJobsPending = removeAssoc jobid $ spcJobsPending state
                  }
        Nothing ->
          put $
            state
              { spcWaiting = rest,
                spcJobsDone = (jobid, reason) : spcJobsDone state,
                spcJobsPending = removeAssoc jobid $ spcJobsPending state,
                spcJobsRunning = removeAssoc jobid $ spcJobsRunning state
              }

workerIsIdle :: WorkerName -> SPCM Bool
workerIsIdle wName = do
  state <- get
  pure $ wName `elem` map fst (spcIdleWorkers state)

workerIsBusy :: WorkerName -> SPCM Bool
workerIsBusy wName = do
  state <- get
  pure $ wName `elem` map fst (spcBusyWorkers state)

checkTimeouts :: SPCM ()
checkTimeouts = do
  state <- get
  now <- io getSeconds
  forM_ (spcJobsRunning state) $ \(_, (wName, deadline)) ->
    when (now >= deadline) $ do
      case lookup wName $ spcBusyWorkers state of
        Just (Worker workerServer) -> do
          io $ sendTo workerServer WorkerMsgJobTimedOut
        Nothing -> pure ()

workerExists :: WorkerName -> SPCM Bool
workerExists wName = do
  busy <- workerIsBusy wName
  idle <- workerIsIdle wName
  pure $ busy || idle

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  checkTimeouts
  schedule
  msg <- io $ receive c
  case msg of
    MsgJobAdd job rsvp -> do
      state <- get
      let JobId jobid = spcJobCounter state
      put $
        state
          { spcJobsPending =
              (spcJobCounter state, job) : spcJobsPending state,
            spcJobCounter = JobId $ succ jobid
          }
      io $ reply rsvp $ JobId jobid
    MsgJobStatus jobid rsvp -> do
      state <- get
      io $ reply rsvp $ case ( lookup jobid $ spcJobsPending state,
                               lookup jobid $ spcJobsRunning state,
                               lookup jobid $ spcJobsDone state
                             ) of
        (Just _, _, _) -> JobPending
        (_, Just _, _) -> JobRunning
        (_, _, Just r) -> JobDone r
        _ -> JobUnknown
    MsgJobWait jobid rsvp -> do
      state <- get
      case lookup jobid $ spcJobsDone state of
        Just reason -> io $ reply rsvp (Just reason)
        Nothing -> do
          put $ state {spcWaiting = (jobid, rsvp) : spcWaiting state}
    MsgWorkerJobDone wName reason -> do
      state <- get
      case lookup wName $ spcWorkerJobs state of
        Just jobid ->
          case lookup jobid $ spcJobsRunning state of
            Just _ -> jobDone jobid reason
            Nothing -> pure ()
        Nothing -> pure ()
    MsgWorkerAdd wName worker -> do
      state <- get
      put $
        state
          { spcIdleWorkers = (wName, worker) : spcIdleWorkers state
          }
    MsgDoesWorkerExist wName rsvp -> do
      exists <- workerExists wName
      io $ reply rsvp exists
    MsgJobCancel jobid -> do
      state <- get
      case lookup jobid $ spcJobsPending state of
        Just _ -> do
          jobDone jobid DoneCancelled
        Nothing -> do
          case lookup jobid $ spcJobsRunning state of
            Just (wName, _) -> do
              case lookup wName $ spcBusyWorkers state of
                Just (Worker workerServer) -> do
                  io $ sendTo workerServer WorkerMsgCancelJob
                  jobDone jobid DoneCancelled
                Nothing -> pure ()
            Nothing -> pure ()
    MsgWorkerShutDown wName -> do
      state <- get
      case lookup wName $ spcWorkerJobs state of
        Just jobid ->
          case lookup jobid $ spcJobsRunning state of
            Just _ -> do
              jobDone jobid DoneCancelled
              modify $ removeWorker wName
            Nothing -> do
              modify $ removeWorker wName
        Nothing -> do
          modify $ removeWorker wName
    MsgTick -> pure ()

removeWorker :: WorkerName -> SPCState -> SPCState
removeWorker wName state =
  state
    { spcIdleWorkers = removeAssoc wName $ spcIdleWorkers state,
      spcBusyWorkers = removeAssoc wName $ spcBusyWorkers state,
      spcWorkerJobs = removeAssoc wName $ spcWorkerJobs state
    }

startSPC :: IO SPC
startSPC = do
  let initial_state =
        SPCState
          { spcJobCounter = JobId 0,
            spcJobsPending = [],
            spcJobsRunning = [],
            spcJobsDone = [],
            spcWaiting = [],
            spcIdleWorkers = [],
            spcBusyWorkers = [],
            spcWorkerJobs = []
          }
  c <- spawn $ \c -> runSPCM initial_state $ forever $ handleMsg c
  void $ spawn $ timer c
  pure $ SPC c
  where
    timer c _ = forever $ do
      threadDelay 1000000 -- 1 second
      sendTo c MsgTick

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC c) job =
  requestReply c $ MsgJobAdd job

-- | Asynchronously query the job status.
jobStatus :: SPC -> JobId -> IO JobStatus
jobStatus (SPC c) jobid =
  requestReply c $ MsgJobStatus jobid

-- | Synchronously block until job is done and return the reason.
jobWait :: SPC -> JobId -> IO (Maybe JobDoneReason)
jobWait (SPC c) jobid =
  requestReply c $ MsgJobWait jobid

-- | Asynchronously cancel a job.
jobCancel :: SPC -> JobId -> IO ()
jobCancel (SPC c) jobid =
  sendTo c $ MsgJobCancel jobid

-- | Add a new worker with this name. Fails with 'Left' if a worker
-- with that name already exists.
workerAdd :: SPC -> WorkerName -> IO (Either String Worker)
workerAdd (SPC channel) wName = do
  exists <- requestReply channel $ MsgDoesWorkerExist wName
  if exists
    then pure $ Left $ "Worker with name '" ++ wName ++ "' already exists."
    else do
      workerServ <- spawn $ \c -> handleWorkerMsg c (SPC channel) wName Nothing
      let worker = Worker workerServ
      sendTo channel $ MsgWorkerAdd wName worker
      pure $ Right worker

handleWorkerMsg :: Chan WorkerMsg -> SPC -> WorkerName -> Maybe ThreadId -> IO ()
handleWorkerMsg c (SPC channel) wName tid = do
  msg <- receive c
  case msg of
    WorkerMsgStartJob job -> do
      t <- forkIO $ do
        let doJob = do
              jobAction job
              send c $ WorkerMsgJobDone Done
            onException :: SomeException -> IO ()
            onException _ =
              send c WorkerMsgJobCrashed
        doJob `catch` onException
      handleWorkerMsg c (SPC channel) wName (Just t)
    WorkerMsgJobDone reason -> do
      sendTo channel $ MsgWorkerJobDone wName reason
      handleWorkerMsg c (SPC channel) wName tid
    WorkerMsgCancelJob -> do
      forM_ tid $ \threadId -> do
        killThread threadId
      sendTo channel $ MsgWorkerJobDone wName DoneCancelled
      handleWorkerMsg c (SPC channel) wName Nothing
    WorkerMsgJobTimedOut -> do
      forM_ tid $ \threadId -> do
        killThread threadId
      sendTo channel $ MsgWorkerJobDone wName DoneTimeout
      handleWorkerMsg c (SPC channel) wName Nothing
    WorkerMsgJobCrashed -> do
      forM_ tid $ \threadId -> do
        killThread threadId
      sendTo channel $ MsgWorkerJobDone wName DoneCrashed
      handleWorkerMsg c (SPC channel) wName Nothing
    WorkerMsgShutDown -> do
      forM_ tid $ \threadId -> do
        killThread threadId
      sendTo channel $ MsgWorkerShutDown wName

-- | Shut down a running worker. No effect if the worker is already
-- terminated.
workerStop :: Worker -> IO ()
workerStop (Worker workerServer) = do
  sendTo workerServer WorkerMsgShutDown
