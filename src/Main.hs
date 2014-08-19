import ParsecParser

main :: IO ()
main = do
  ast <- fmap parseExpr getContents
  print ast
