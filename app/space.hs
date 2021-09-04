-- Space.hs

module Space where

type Width = Int
type Height = Int
type Dim = (Width, Height)
type Coor = (Int, Int)

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
makeFrame w h = Frame (w, h) zeroCoor []

-- Split an empty frame four ways.
splitFrameFour :: Frame -> Int -> Int -> Either FrameError Frame
splitFrameFour fr xSpl ySpl
    | not . null $ (frameNest fr) = Left NonEmpty 
    | otherwise = 
        let (w, h) = frameDim fr in 
        if (0 >= xSpl || xSpl >= w || 
            0 >= ySpl || ySpl >= h) then Left BadSplit
        else 
            let (fx, fy) = frameCoor fr
                bl = Frame (xSpl, ySpl)         (fx, fy)                []
                br = Frame (w - xSpl, ySpl)     (fx + xSpl, fy)         []
                tl = Frame (xSpl, h - ySpl)     (fx, fy + ySpl)         []
                tr = Frame (w - xSpl, h - ySpl) (fx + xSpl, fy + ySpl)  []
            in Right $ Frame (fx, fy) (w, h) [bl, br, tl, tr]

-- Split an empty frame two ways. 
splitFrameTwo :: Frame -> Int -> Split -> Either FrameError Frame
splitFrameTwo fr s vOrH
    | not . null $ (frameNest fr) = Left NonEmpty
    | vOrH == V = 
        if (0 >= s || s >= w) then Left BadSplit
        else 
            let l = Frame (s, h)     (fx, fy)       []
                r = Frame (w - s, h) (fx + s, fy)   []
            in Right $ Frame (fx, fy) (w, h) [l, r]
    | otherwise = -- vOrH == V
        if (0 >= s || s >= h) then Left BadSplit
        else 
            let b = Frame (w, s)     (fx, fy)       []
                t = Frame (w, h - s) (fx, fy + s)   []
            in Right $ Frame (fx, fy) (w, h) [b, t]
    where (w, h) = frameDim fr
          (fx, fy) = frameCoor fr
