import Parser
import Expr

interp :: String -> IO ()
interp ex = do
  let ast = parseExpr ex
  print ast
  print (run ast)

main :: IO ()
main = do
  ex <- getContents
  interp ex
