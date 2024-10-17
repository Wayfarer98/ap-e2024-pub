module SPC
  ( -- * SPC startup
    SPC,
    startSPC,
    Job (..),
    JobDoneReason (..),
    JobStatus (..),
    JobId,
    jobAdd,
    jobStatus,
    jobCancel,
    jobWait,
  )
where

import Control.Concurrent
  ( ThreadId,
    forkIO,
    killThread,
    newChan,
    threadDelay,
  )
import Control.Exception (SomeException, catch)
import Control.Monad (ap, forM_, forever, liftM, void, when)
import Data.Function ((&))
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

data Job = Job
  { jobAction :: IO (),
    jobMaxSeconds :: Int
  }

data JobDoneReason
  = Done
  | DoneTimeout
  | DoneCancelled
  | DoneCrashed
  deriving (Eq, Ord, Show)

data JobStatus
  = JobDone JobDoneReason
  | JobRunning
  | JobPending
  | JobUnknown
  deriving (Eq, Ord, Show)

newtype JobId = JobId Int
  deriving (Eq, Ord, Show)

-- Messages sent to SPC.
data SPCMsg
  = MsgAddJob Job (ReplyChan JobId)
  | MsgGetJobStatus JobId (ReplyChan JobStatus)
  | MsgCancelJob JobId
  | MsgJobWait JobId (ReplyChan (Maybe JobDoneReason))
  | MsgJobDone JobId

-- | A Handle to the SPC instance.
newtype SPC = SPC (Server SPCMsg)

data SPCState = SPCState
  { spcJobs :: [(JobId, Job)],
    spcNextJobId :: JobId,
    spcDoneJobs :: [(JobId, JobDoneReason)],
    spcJobChannels :: [(JobId, ReplyChan (Maybe JobDoneReason))],
    spcRunningJob :: Maybe (JobId, ThreadId),
    spcChannel :: Chan SPCMsg
  }

newtype SPCM a = SPCM (SPCState -> IO (a, SPCState))

instance Functor SPCM where
  fmap = liftM

instance Applicative SPCM where
  pure a = SPCM $ \s -> pure (a, s)
  (<*>) = ap

instance Monad SPCM where
  return = pure
  (>>=) (SPCM m) f = SPCM $ \s -> do
    (a, s') <- m s
    let SPCM m' = f a
    m' s'

get :: SPCM SPCState
get = SPCM $ \s -> pure (s, s)

put :: SPCState -> SPCM ()
put s = SPCM $ \_ -> pure ((), s)

io :: IO a -> SPCM a
io m = SPCM $ \s -> do
  a <- m
  pure (a, s)

runSPCM :: SPCState -> SPCM a -> IO a
runSPCM s (SPCM m) = do
  (a, _) <- m s
  pure a

startSPC :: IO SPC
startSPC = do
  server <- spawn $ \c -> runSPCM (SPCState [] (JobId 0) [] [] Nothing c) (forever $ handleMsg c)
  pure $ SPC server

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg chan = do
  schedule
  msg <- io $ receive chan
  case msg of
    MsgAddJob job rsvp -> do
      state <- get
      let JobId jobId = spcNextJobId state
      put $ state {spcJobs = (spcNextJobId state, job) : spcJobs state, spcNextJobId = JobId $ succ jobId}
      io $ reply rsvp $ JobId jobId
    MsgGetJobStatus jobId rsvp -> do
      state <- get
      case lookup jobId $ spcJobs state of
        Just _ -> io $ reply rsvp JobPending
        Nothing -> case lookup jobId $ spcDoneJobs state of
          Just reason -> io $ reply rsvp $ JobDone reason
          Nothing -> io $ reply rsvp JobUnknown
    MsgCancelJob jobId -> do
      state <- get
      case lookup jobId $ spcJobs state of
        Just _ -> do
          jobDone jobId DoneCancelled
        Nothing -> pure ()
    MsgJobWait jobId rsvp -> do
      state <- get
      case lookup jobId $ spcDoneJobs state of
        Just reason -> io $ reply rsvp $ Just reason
        Nothing -> do
          put $ state {spcJobChannels = (jobId, rsvp) : spcJobChannels state}
    MsgJobDone jobId -> do
      state <- get
      case spcRunningJob state of
        Just (runningJobId, threadId) ->
          when (runningJobId == jobId) $ do
            io $ killThread threadId
            jobDone jobId Done
            put $ state {spcRunningJob = Nothing}
        Nothing -> jobDone jobId Done

jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC server) job = do
  requestReply server (MsgAddJob job)

jobStatus :: SPC -> JobId -> IO JobStatus
jobStatus (SPC server) jobId = do
  requestReply server (MsgGetJobStatus jobId)

jobCancel :: SPC -> JobId -> IO ()
jobCancel (SPC server) jobId = do
  sendTo server (MsgCancelJob jobId)

jobWait :: SPC -> JobId -> IO (Maybe JobDoneReason)
jobWait (SPC c) jobId = do
  requestReply c (MsgJobWait jobId)

jobDone :: JobId -> JobDoneReason -> SPCM ()
jobDone jobId reason = do
  state <- get
  let (waiting, rest) = partition ((== jobId) . fst) $ spcJobChannels state
  forM_ waiting $ \(_, rsvp) -> io $ reply rsvp $ Just reason
  put $ state {spcJobs = removeAssoc jobId $ spcJobs state, spcDoneJobs = (jobId, reason) : spcDoneJobs state, spcJobChannels = rest}

schedule :: SPCM ()
schedule = do
  state <- get
  case spcJobs state of
    [] -> pure ()
    (jobId, job) : rest ->
      case spcRunningJob state of
        Just _ -> pure ()
        Nothing -> do
          t <- io $ forkIO $ do
            jobAction job
            send (spcChannel state) $ MsgJobDone jobId
          put $ state {spcJobs = rest, spcRunningJob = Just (jobId, t)}
