import ParsecParser
import Eval
import Expr

main :: IO ()
main = do
  ast <- parseExpr `fmap` getContents
  putStrLn (source ast)
  print ast
  print (eval ast)
