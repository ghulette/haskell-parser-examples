import ParsecParser
import Eval

main :: IO ()
main = do
  ast <- parseExpr `fmap` getContents
  print ast
  print (eval ast)
