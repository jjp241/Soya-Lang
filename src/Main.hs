module Main where
import qualified Interpreter as I 
import qualified TypeChecker as T
import qualified ParSoya as P
import System.Exit
import System.IO
import System.Environment

parseAndInterpret :: String -> IO ()
parseAndInterpret prog = do
    case P.pProgram $ P.myLexer prog of 
      Left s -> hPutStrLn stderr s
      Right program -> 
        case T.runChecker program of
            Left err -> hPutStrLn stderr err
            Right _ -> I.runInterpreter program

main :: IO () 
main = do
    args <- getArgs
    if length args == 0 then
      die "Usage: interpreter <path_to_file>"
    else do
      let path = head args
      prog <- readFile path
      parseAndInterpret prog
