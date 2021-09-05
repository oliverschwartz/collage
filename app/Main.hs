module Main where

import Data.Maybe
import ImageUtils


main :: IO ()
main = do 
    imageNames <- getJPGNames 
    mImgs <- mapM getImage imageNames
    let imgs = catMaybes mImgs
    let t = foldr f (makeTree (head imgs) 100 100) (tail imgs) where
        f img tree = addImage tree img
    let resolved = resolveTree t
    saveImage "singleResolved.jpg" resolved
    -- mapM_ putStrLn imageNames
    

