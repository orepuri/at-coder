import Text.Printf

main :: IO ()
main = do
  n <- readLn :: IO Int
  printf "ABC%03d" n