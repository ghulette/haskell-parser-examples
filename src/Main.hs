import qualified ParsecParser
import qualified HappyParser
import Eval
import Expr

runEvalWith :: (String -> Expr) -> String -> IO ()
runEvalWith parseExpr input = do
  let ast = parseExpr input
  putStrLn $ "AST: " ++ (show ast)
  putStrLn $ "Source: " ++ (source ast)
  putStrLn $ "Eval: " ++ (show (eval ast))
  
main :: IO ()
main = do
  input <- getContents
  putStrLn "Input:"
  putStrLn input
  putStrLn "Using Parsec:"
  runEvalWith ParsecParser.parseExpr input
  putStrLn "\nUsing Happy:"
  runEvalWith HappyParser.parseExpr input
