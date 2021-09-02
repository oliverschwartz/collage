-- image-processor.hs

module ImageProcessor where

import Codec.Picture
import Paths_collage

-- Type aliases. 
type Width = Int
type Height = Int

getAndResizeImage :: String -> String -> IO ()
getAndResizeImage filename filename' = do 
    mImg <- getRGBImage filename
    putStrLn "getAndResizeImage"
    maybeImg mImg where 
        maybeImg mImg = 
            case mImg of 
                Nothing -> return ()
                Just img -> let resized = resizeImage img 300 600 in 
                                case resized of 
                                    Nothing -> putStrLn "nothing"
                                    Just img' -> saveImage filename' img'

getRGBImage :: String -> IO (Maybe (Image PixelRGB8))
getRGBImage filename = do 
    filepath <- getDataFileName filename
    eImg <- readImage filepath
    let maybeImg = case eImg of 
                    Left _ -> Nothing
                    Right dImg -> Just $ convertRGB8 dImg
    return maybeImg 
   
-- Resize an image by a straight pixel map (no smoothing). 
resizeImage :: Pixel a => Image a -> Width -> Height -> Maybe (Image a)
resizeImage img w' h' 
    | w' <= 0 || h' <= 0 = Nothing
    | otherwise = Just newImg where 
        w = imageWidth img 
        h = imageHeight img
        f x y = pixelAt img x' y' where 
            wf  = fromIntegral w  :: Float
            wf' = fromIntegral w' :: Float
            hf  = fromIntegral h  :: Float
            hf' = fromIntegral h' :: Float
            x' = min (w-1) $ round (wf * (fromIntegral x :: Float) / wf')
            y' = min (h-1) $ round (hf * (fromIntegral y :: Float) / hf')
        newImg = generateImage f w' h'

saveImage :: String -> Image PixelRGB8 -> IO () 
saveImage filename img = do
    filepath <- getDataFileName filename
    saveJpgImage 100 filepath (ImageRGB8 img)
