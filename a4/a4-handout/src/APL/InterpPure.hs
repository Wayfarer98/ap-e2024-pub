module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> ([String], Either Error a)
runEval = runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Either Error a)
    runEval' _ _ (Pure x) = ([], pure x)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (StateGetOp k)) = runEval' r s $ k s
    runEval' r _ (Free (StatePutOp s' m)) = runEval' r s' m
    runEval' r s (Free (PrintOp p m)) =
      let (ps, res) = runEval' r s m
       in (p : ps, res)
    runEval' _ _ (Free (ErrorOp e)) = ([], Left e)
    runEval' r s (Free (TryCatchOp m1 m2)) =
      case runEval' r s m1 of
        (ps, Left _) ->
          let (ps', res') = runEval' r s m2
           in (ps ++ ps', res')
        (ps, Right x) -> (ps, pure x)
    runEval' r s (Free (KvGetOp v k)) =
      case lookup v s of
        Just v' -> runEval' r s $ k v'
        Nothing -> ([], Left $ "Invalid key: " ++ show v)
    runEval' r s (Free (KvPutOp v1 v2 m)) =
      runEval' r ((v1, v2) : filter ((/= v1) . fst) s) m
    runEval' r s (Free (TransactionOp m n)) =
      case runEval' r s (m >> getState) of
        (ps, Right s') ->
          let (ps', res') = runEval' r s' n
           in (ps ++ ps', res')
        (ps, Left _) ->
          let (ps', res') = runEval' r s n
           in (ps ++ ps', res')
