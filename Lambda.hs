{-# LANGUAGE FlexibleContexts #-}
import Lambda.Stmt
import Lambda.Parser
import Lambda.Intrp
import System.Environment
import Control.Monad.Except

strErr :: (MonadError String m, Show e) => Either e a -> m a
strErr = either (throwError . show) return


main = (either (putStrLn) (putStrLn . show)) <=< runExceptT $ do
  fname <- liftIO getArgs
  input <- liftIO $ readFile (head fname)
  prog <- strErr $ lparse (head fname) input
  strErr $ intrp prog









