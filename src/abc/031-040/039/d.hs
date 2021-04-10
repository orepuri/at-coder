import Control.Monad
import qualified Data.Array.IO as IOA
import qualified Data.Array.Unboxed as UA

main :: IO ()
main = do
  [gh, gw] <- map read . words <$> getLine
  grid <- IOA.newArray ((1, 1), (gw, gh)) '#' :: IO (IOA.IOUArray (Int, Int) Char)
  forM_ [1..gh] $ \h -> do
    s <- getLine
    forM_ [1..gw] $ \w -> do
      IOA.writeArray grid (w, h) (s !! (w - 1))
  shrinkGrid <- IOA.freeze grid :: IO (UA.UArray (Int, Int) Char)
  let neighbors w h = filter (/=(w, h)) [(w', h') | w' <- [w-1,w,w+1], w' > 0, w' <= gw, h' <- [h-1, h, h+1], h' > 0, h' <= gh]
  forM_ [1..gh] $ \h -> do
    forM_ [1..gw] $ \w -> do
      let c = shrinkGrid UA.! (w, h)
      when (c == '.') $ do
        forM_ (neighbors w h) $ \mas -> do
          IOA.writeArray grid mas '.'
  unShrinkGrid <- IOA.freeze grid :: IO (UA.UArray (Int, Int) Char)

  grid' <- IOA.newArray ((1, 1), (gw, gh)) '#' :: IO (IOA.IOUArray (Int, Int) Char)
  forM_ [1..gh] $ \h -> do
    forM_ [1..gw] $ \w -> do
      let c = unShrinkGrid UA.! (w, h)
      IOA.writeArray grid' (w, h) c
  
  
  forM_ [1..gh] $ \h -> do
    forM_ [1..gw] $ \w -> do
      let c = unShrinkGrid UA.! (w, h)
      when (c == '#') $ do
        forM_ (neighbors w h) $ \mas -> do
          IOA.writeArray grid' mas '#'
  shrinkGrid' <- IOA.freeze grid' :: IO (UA.UArray (Int, Int) Char)

  if shrinkGrid == shrinkGrid'
    then do
      putStrLn "possible"
      showGrid gw gh unShrinkGrid
    else putStrLn "impossible"

showGrid :: Int -> Int -> UA.UArray (Int, Int) Char -> IO ()
showGrid w h grid = do
  forM_ [1..h] $ \h' -> do
    forM_ [1..w] $ \w' -> do
      putChar $ grid UA.! (w', h')
    putStrLn ""
