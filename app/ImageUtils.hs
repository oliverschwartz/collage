-- ImageUtils.hs

module ImageUtils where

import System.Directory
import Codec.Picture
import Data.Word
import Paths_collage

type Width = Int
type Height = Int
type ImageP = Image PixelRGB8
data Coor = Coor (Int, Int) deriving (Eq, Show)
data Dim = Dim (Width, Height) deriving (Eq, Show)
data Split = V | H deriving (Eq, Show)
data Frame = Frame { frameDim :: Dim, frameCoor :: Coor} deriving (Eq, Show)
data Tree = Leaf Frame ImageP | Node Frame [Tree] deriving Eq
data FrameError = NonEmpty | BadSplit deriving (Eq, Show)

getJPGNames :: IO [FilePath]
getJPGNames = do
    filepath <- getDataFileName ""
    let lastFour s = drop (length s - 4) s
        isJpg s = elem (lastFour s) [".jpg", ".jpeg"]
    contents <- getDirectoryContents filepath
    return $ filter isJpg contents

-- Make a new tree with one image.
makeTree :: ImageP -> Width -> Height -> Tree
makeTree img w h = Leaf (Frame (Dim (w, h)) (Coor (0, 0))) img

-- Add an image to the collage. TODO: remove runtime errors.
addImage :: Tree -> ImageP -> Tree
addImage t@(Leaf (Frame (Dim (w, _)) _) _) img' = 
    case splitFrame t (w `div` 2) V img' of 
            Left err -> error $ show err
            Right t' -> t'
addImage (Node fr nested) img'
    | null nested = error "Empty node"
    | otherwise = 
        let hd = addImage (head nested) img'
        in Node fr (hd : tail nested)

-- Given a collage tree, return the stitched image of the collage.
resolveTree :: Tree -> ImageP
resolveTree t = generateImage f w h where
    f x y = case extractPixel x y t of
                Just p -> p
                _ -> error "Extracting pixel broke"
    (w, h) = case t of 
        Node (Frame (Dim (w', h')) _) _ -> (w', h')
        Leaf (Frame (Dim (w', h')) _) _ -> (w', h')

-- Given coordinates in the collage, return the pixel that belongs there.
extractPixel :: Int -> Int -> Tree -> Maybe PixelRGB8
extractPixel x y (Leaf (Frame (Dim (w, h)) (Coor (fx, fy))) img) = 
    case resizeImage img w h of
        Nothing -> Nothing
        Just resized -> 
            if (x < fx || x >= fx + w || y < fy || y >= fy + h) then Nothing
            else Just $ pixelAt resized (x - fx) (y - fy)
extractPixel x y (Node (Frame (Dim (w, h)) (Coor (fx, fy))) nested) = 
    if (x < fx || x >= fx + w || y < fy || y >= fy + h) then Nothing
    else 
        let extractFromNested = map (extractPixel x y) nested
            f a b = case a of { Just _ -> a; _ -> b }
        in foldr f Nothing extractFromNested
        
-- Split a frame two ways (vertically or horizontally).
splitFrame :: Tree -> Int -> Split -> ImageP -> Either FrameError Tree
splitFrame (Leaf fr@(Frame (Dim (w, h)) (Coor (fx, fy))) img) pos spl img'
    | spl == V = 
        if (0 >= pos || pos >= w) then Left BadSplit
        else 
            let lfr = Frame (Dim (pos, h))     (Coor (fx, fy))
                rfr = Frame (Dim (w - pos, h)) (Coor (fx + pos, fy))
                l = Leaf lfr img
                r = Leaf rfr img'
            in Right $ Node fr [l, r]
    | otherwise = 
        if (0 >= pos || pos >= h) then Left BadSplit
        else 
            let bfr = Frame (Dim (w, pos))     (Coor (fx, fy))
                tfr = Frame (Dim (w, h - pos)) (Coor (fx, fy + pos))
                b = Leaf bfr img
                t = Leaf tfr img'
            in Right $ Node fr [b, t]
splitFrame (Node _ _) _ _ _ = Left BadSplit

-- Read image from data directory.
getImage :: String -> IO (Maybe (ImageP))
getImage filename = do 
    filepath <- getDataFileName filename
    img <- readImage filepath
    let maybeImg = case img of 
                    Left _ -> Nothing
                    Right dImg -> Just $ convertRGB8 dImg
    return maybeImg 

-- Save image to data directory.
saveImage :: String -> ImageP -> IO () 
saveImage filename img = do
    filepath <- getDataFileName filename
    saveJpgImage 100 filepath (ImageRGB8 img)

blendPixels :: [PixelRGB8] -> PixelRGB8
blendPixels pixels = 
    let n = fromIntegral (length pixels) :: Float
        toFloat = fromIntegral :: (Integral a => a -> Float)
        tuplify (PixelRGB8 r g b) = (toFloat r, toFloat g, toFloat b)
        pixels' = map tuplify pixels
        rSum = foldr (\(r,_,_) s -> r + s) 0 pixels' 
        gSum = foldr (\(_,g,_) s -> g + s) 0 pixels'
        bSum = foldr (\(_,_,b) s -> b + s) 0 pixels'
        toWord = \x -> (round $ x / n) :: Word8
    in PixelRGB8 (toWord rSum) (toWord gSum) (toWord bSum) 

getNbors :: Int -> Int -> Width -> Height -> [(Int, Int)]
getNbors x y w h = 
    let n = 2
        nbors = [(x', y') | x' <- [x-n..x+n], y' <- [y-n..y+n]]
        f = filter (\(x', y') -> 0 <= x' && x' < w && 0 <= y' && y' < h)
    in f nbors

blendNbors :: ImageP -> Int -> Int -> PixelRGB8
blendNbors img x y = 
    let (w, h) = (imageWidth img, imageHeight img)
        nbors = getNbors x y w h 
        pixels = map (\(x',y') -> pixelAt img x' y') nbors
    in blendPixels pixels

resizeImage :: ImageP -> Width -> Height -> Maybe (ImageP)
resizeImage img wNew hNew
    | wNew <= 0 || hNew <= 0 = Nothing
    | otherwise = Just newImg where 
        (w, h) = (imageWidth img, imageHeight img)
        f x y = pixelAt img x' y' where 
            toFloat = fromIntegral :: (Integral a => a -> Float)
            tuplify [a,b,c,d] = (a,b,c,d)
            tuplify _ = error "Incorrect usage" 
            (wf, wfNew, hf, hfNew) = tuplify $ map toFloat [w, wNew, h, hNew]
            x' = min (w-1) $ round (wf * (fromIntegral x :: Float) / wfNew)
            y' = min (h-1) $ round (hf * (fromIntegral y :: Float) / hfNew)
        newImg = generateImage f wNew hNew

getResizeSaveImage :: String -> String -> Width -> Height -> IO ()
getResizeSaveImage filename filename' w h = do 
    img <- getImage filename
    go img where 
        go img' = 
            case img' of 
                Nothing -> return ()
                Just img'' -> let resized = resizeImage img'' w h in 
                                case resized of 
                                    Nothing -> return ()
                                    Just img''' -> saveImage filename' img'''
