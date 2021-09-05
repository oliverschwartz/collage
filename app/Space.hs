-- Space.hs

module Space where

import ImageProcessor

type Width = Int
type Height = Int
data Coor = Coor (Int, Int) deriving (Eq, Show)
data Dim = Dim (Width, Height) deriving (Eq, Show)
data Split = V | H deriving (Eq, Show)
data Frame = Frame { frameDim :: Dim, frameCoor :: Coor} deriving Eq
data Tree = Leaf Frame ImageP | Node Frame [Tree]
data FrameError = NonEmpty | BadSplit deriving (Eq, Show)

-- Make a new tree with one image.
makeTree :: ImageP -> Width -> Height -> Tree
makeTree img w h = Leaf (Frame (Dim (w, h)) (Coor (0, 0))) img

-- Add an image to the collage.
addImage :: Tree -> ImageP -> Maybe Tree
addImage t@(Leaf (Frame (Dim (w, _)) _) _) img' = 
    case splitFrame t (w `div` 2) V img' of 
            Left err -> Nothing
            Right t' -> Just t'
addImage (Node fr nested) img'
    | null nested = error "Empty node"
    | otherwise = 
        case addImage (head nested) img' of 
            Just hd -> Just $ Node fr (hd : tail nested)
            Nothing -> Nothing
        
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

