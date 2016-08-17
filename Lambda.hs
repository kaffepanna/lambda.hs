import Lambda.Stmt
import Lambda.Parser
import Lambda.Intrp
import System.Environment

main = do
    fname <- getArgs
    input <- readFile (head fname)
    case lparse (head fname) input of
      Left msg -> putStrLn $ show msg
      Right p -> case intrp p of
        Left msg -> putStrLn $ show msg
        Right r -> putStrLn $ show r

