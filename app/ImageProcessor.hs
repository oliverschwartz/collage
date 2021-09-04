-- ImageProcessor.hs

module ImageProcessor where

import Codec.Picture
import Paths_collage
import Data.Word


-- Type aliases. 
type Width = Int
type Height = Int

-- Reads an image from the data/ directory.
getImage :: String -> IO (Maybe (Image PixelRGB8))
getImage filename = do 
    filepath <- getDataFileName filename
    img <- readImage filepath
    let maybeImg = case img of 
                    Left _ -> Nothing
                    Right dImg -> Just $ convertRGB8 dImg
    return maybeImg 

-- Saves to the data/ directory.
saveImage :: String -> Image PixelRGB8 -> IO () 
saveImage filename img = do
    filepath <- getDataFileName filename
    saveJpgImage 100 filepath (ImageRGB8 img)
  
blendPixels :: [PixelRGB8] -> PixelRGB8
blendPixels pixels = 
    let n = fromIntegral (length pixels) :: Float
        toFloat = fromIntegral :: (Integral a => a -> Float)
        f (PixelRGB8 r g b) = (toFloat r, toFloat g, toFloat b)
        pixels' = map f pixels
        rSum = foldr (\(r,_,_) s -> r + s) 0 pixels' 
        gSum = foldr (\(_,g,_) s -> g + s) 0 pixels'
        bSum = foldr (\(_,_,b) s -> b + s) 0 pixels'
        g = \x -> (round $ x / n) :: Word8
    in PixelRGB8 (g rSum) (g gSum) (g bSum) 

getNbors :: Int -> Int -> Width -> Height -> [(Int, Int)]
getNbors x y w h = 
    let n = 1
        nbors = [(x', y') | x' <- [x-n..x+n], y' <- [y-n..y+n]]
        f = filter (\(x', y') -> 0 <= x' && x' < w && 0 <= y' && y' < h)
    in f nbors

blendNbors :: Image PixelRGB8 -> Int -> Int -> PixelRGB8
blendNbors img x y = 
    let (w, h) = (imageWidth img, imageHeight img)
        nbors = getNbors x y w h 
        pixels = map (\(x,y) -> pixelAt img x y) nbors
    in blendPixels pixels

resizeImage :: Image PixelRGB8 -> Width -> Height -> Maybe (Image PixelRGB8)
resizeImage img wNew hNew
    | wNew <= 0 || hNew <= 0 = Nothing
    | otherwise = Just newImg where 
        (w, h) = (imageWidth img, imageHeight img)
        f x y = pixelAt img x' y' where 
            toFloat = fromIntegral :: (Integral a => a -> Float)
            tuplify [a,b,c,d] = (a,b,c,d)
            (wf, wfNew, hf, hfNew) = tuplify $ map toFloat [w, wNew, h, hNew]
            x' = min (w-1) $ round (wf * (fromIntegral x :: Float) / wfNew)
            y' = min (h-1) $ round (hf * (fromIntegral y :: Float) / hfNew)
        newImg = generateImage f wNew hNew

getAndResizeImage :: String -> String -> Width -> Height -> IO ()
getAndResizeImage filename filename' w h = do 
    img <- getImage filename
    go img where 
        go img = 
            case img of 
                Nothing -> return ()
                Just img -> let resized = resizeImage img w h in 
                                case resized of 
                                    Nothing -> return ()
                                    Just img' -> saveImage filename' img'
