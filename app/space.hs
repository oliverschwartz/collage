-- space.hs

module Space where


type Width = Int
type Height = Int
type Aspect = (Width, Height)
type Coor = (Int, Int)
data Frame = Frame {
    frameAspect :: Aspect,
    frameCoor   :: Coor,
    frameNest   :: [Frame]
} deriving (Eq, Show)
data FrameError = NonEmpty | BadSplit deriving (Eq, Show)

zeroCoor :: Coor
zeroCoor = (0, 0)

makeFrame :: Width -> Height -> Frame
makeFrame w h = Frame (w, h) zeroCoor []

splitFrame :: Frame -> Int -> Int -> Either FrameError Frame
splitFrame fr xSpl ySpl
    | not . null $ (frameNest fr) = Left NonEmpty 
    | otherwise = 
        let (w, h) = frameAspect fr in 
        if (0 >= xSpl || xSpl >= w || 
            0 >= ySpl || ySpl >= h) then Left BadSplit
        else 
            let (fx, fy) = frameCoor fr
                bl = Frame (xSpl, ySpl)         (fx, fy) []
                br = Frame (w - xSpl, ySpl)     (fx + xSpl, fy) []
                tl = Frame (xSpl, h - ySpl)     (fx, fy + ySpl) []
                tr = Frame (w - xSpl, h - ySpl) (fx + xSpl, fy + ySpl) []
            in Right $ Frame (fx, fy) (w, h) [bl, br, tl, tr]


            
            
