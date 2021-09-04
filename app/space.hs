-- Space.hs

module Space where

type Width = Int
type Height = Int
type Coor = (Int, Int)
data Dim = Dim (Width, Height) deriving (Eq, Show)

data Frame = Frame {
    frameDim    :: Dim,
    frameCoor   :: Coor,
    frameNest   :: [Frame]
} deriving (Eq, Show)
data FrameError = NonEmpty | BadSplit deriving (Eq, Show)
data Split = V | H deriving Eq

zeroCoor :: Coor
zeroCoor = (0, 0)

makeFrame :: Width -> Height -> Frame
makeFrame w h = Frame (Dim (w, h)) zeroCoor []

-- Split an empty frame four ways.
splitFrameFour :: Frame -> Int -> Int -> Either FrameError Frame
splitFrameFour (Frame (Dim (w, h)) (fx, fy) nest) xSpl ySpl
    | not . null $ nest = Left NonEmpty 
    | otherwise = 
        if (0 >= xSpl || xSpl >= w || 
            0 >= ySpl || ySpl >= h) then Left BadSplit
        else 
            let bl = Frame (Dim (xSpl, ySpl))         (fx, fy)                []
                br = Frame (Dim (w - xSpl, ySpl))     (fx + xSpl, fy)         []
                tl = Frame (Dim (xSpl, h - ySpl))     (fx, fy + ySpl)         []
                tr = Frame (Dim (w - xSpl, h - ySpl)) (fx + xSpl, fy + ySpl)  []
            in Right $ Frame (Dim (w, h)) (fx, fy) [bl, br, tl, tr]

-- Split an empty frame two ways. 
splitFrameTwo :: Frame -> Int -> Split -> Either FrameError Frame
splitFrameTwo (Frame (Dim (w, h)) (fx, fy) nest) s vOrH
    | not . null $ nest = Left NonEmpty
    | vOrH == V = 
        if (0 >= s || s >= w) then Left BadSplit
        else 
            let l = Frame (Dim (s, h))     (fx, fy)       []
                r = Frame (Dim (w - s, h)) (fx + s, fy)   []
            in Right $ Frame (Dim (w, h)) (fx, fy) [l, r]
    | otherwise = -- vOrH == V
        if (0 >= s || s >= h) then Left BadSplit
        else 
            let b = Frame (Dim (w, s))     (fx, fy)       []
                t = Frame (Dim (w, h - s)) (fx, fy + s)   []
            in Right $ Frame (Dim (w, h)) (fx, fy) [b, t]
