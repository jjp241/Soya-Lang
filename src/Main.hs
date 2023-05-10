module Main where
import qualified Interpreter as I 
import qualified TypeChecker as T
import System.Exit
import qualified ParSoya as P
import System.IO
import System.Environment
main :: IO () 
main = do
    args <- getArgs
    case args of 
        [path] -> do
                f <- readFile path
                run f
        _ -> die "died xd"


run :: String -> IO ()
run prog = do
    case P.pProgram $ P.myLexer prog of 
      Left s -> hPutStrLn stderr s
      Right program -> 
        case T.runChecker program of
            Left err -> hPutStrLn stderr err
            Right _ -> I.runInterpreter program
