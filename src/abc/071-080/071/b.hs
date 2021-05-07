import qualified Data.ByteString.Char8 as C
import qualified Data.Set as S

main :: IO ()
main = do
  s <- C.unpack <$> C.getLine
  case solve s of
    Just c -> putStrLn [c]
    Nothing -> putStrLn "None"

solve :: String -> Maybe Char
solve s = go s (S.fromList ['a'..'z']) 
  where
    go [] s = if S.size s == 0 then Nothing else Just $ head $ S.toAscList s
    go (c:cs) s = go cs (S.delete c s)

