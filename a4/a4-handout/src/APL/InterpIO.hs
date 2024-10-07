module APL.InterpIO (runEvalIO) where

import APL.Monad
import APL.Util
import System.Directory (removeFile)
import System.IO (hFlush, readFile', stdout)

-- Converts a string into a value. Only 'ValInt's and 'ValBool' are supported.
readVal :: String -> Maybe Val
readVal = unserialize

-- 'prompt s' prints 's' to the console and then reads a line from stdin.
prompt :: String -> IO String
prompt s = do
  putStr s
  hFlush stdout
  getLine

-- 'writeDB dbFile s' writes the 'State' 's' to the file 'db'.
writeDB :: FilePath -> State -> IO ()
writeDB db s =
  writeFile db $ serialize s

-- 'readDB db' reads the database stored in 'db'.
readDB :: FilePath -> IO (Either Error State)
readDB db = do
  ms <- readFile' db
  case unserialize ms of
    Just s -> pure $ pure s
    Nothing -> pure $ Left "Invalid DB."

-- 'copyDB db1 db2' copies 'db1' to 'db2'.
copyDB :: FilePath -> FilePath -> IO ()
copyDB db db' = do
  s <- readFile' db
  writeFile db' s

-- Removes all key-value pairs from the database file.
clearDB :: IO ()
clearDB = writeFile dbFile ""

-- The name of the database file.
dbFile :: FilePath
dbFile = "db.txt"

-- Creates a fresh temporary database, passes it to a function returning an
-- IO-computation, executes the computation, deletes the temporary database, and
-- finally returns the result of the computation. The temporary database file is
-- guaranteed fresh and won't have a name conflict with any other files.
withTempDB :: (FilePath -> IO a) -> IO a
withTempDB m = do
  tempDB <- newTempDB -- Create a new temp database file.
  res <- m tempDB -- Run the computation with the new file.
  removeFile tempDB -- Delete the temp database file.
  pure res -- Return the result of the computation.

runEvalIO :: EvalM a -> IO (Either Error a)
runEvalIO evalm = do
  clearDB
  runEvalIO' envEmpty dbFile evalm
  where
    runEvalIO' :: Env -> FilePath -> EvalM a -> IO (Either Error a)
    runEvalIO' _ _ (Pure x) = pure $ pure x
    runEvalIO' r db (Free (ReadOp k)) = runEvalIO' r db $ k r
    runEvalIO' r db (Free (StateGetOp k)) = do
      res <- readDB db
      case res of
        Left e -> pure $ Left e
        Right s -> runEvalIO' r db $ k s
    runEvalIO' r db (Free (StatePutOp s m)) = do
      writeDB db s
      runEvalIO' r db m
    runEvalIO' r db (Free (PrintOp p m)) = do
      putStrLn p
      runEvalIO' r db m
    runEvalIO' _ _ (Free (ErrorOp e)) = pure $ Left e
    runEvalIO' r db (Free (TryCatchOp m1 m2)) = do
      state <- runEvalIO' r db getState
      res <- runEvalIO' r db m1
      case res of
        Left _ ->
          case state of
            Left e -> pure $ Left e
            Right s -> do
              _ <- runEvalIO' r db $ putState s
              runEvalIO' r db m2
        Right x -> pure $ pure x
    runEvalIO' r db (Free (KvGetOp v k)) = do
      res <- runEvalIO' r db getState
      case res of
        Left e -> pure $ Left e
        Right s ->
          case lookup v s of
            Just v' -> runEvalIO' r db $ k v'
            Nothing -> do
              v' <- prompt ("Key not found: " ++ show v ++ ". Enter a value: ")
              case readVal v' of
                Just v'' -> runEvalIO' r db $ k v''
                Nothing -> pure $ Left $ "Invalid value input: " ++ v'
    runEvalIO' r db (Free (KvPutOp v1 v2 m)) = do
      res <- runEvalIO' r db getState
      case res of
        Left e -> pure $ Left e
        Right s -> do
          _ <- runEvalIO' r db $ putState ((v1, v2) : filter ((/= v1) . fst) s)
          runEvalIO' r db m
    runEvalIO' r db (Free (TransactionOp m n)) = do
      withTempDB $ \tempDB -> do
        copyDB db tempDB
        res <- runEvalIO' r tempDB m
        case res of
          Right _ -> do
            copyDB tempDB db
            runEvalIO' r db n
          Left _ -> runEvalIO' r db n
