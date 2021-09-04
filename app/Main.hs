module Main where

import System.Directory
import Paths_collage
import Space
import ImageProcessor

-- Only JPG images for now. 
getImageNames :: IO [FilePath]
getImageNames = do
    filepath <- getDataFileName ""
    let lastFour s = drop (length s - 4) s
        isJpg s = elem (lastFour s) [".jpg", ".jpeg"]
    contents <- getDirectoryContents filepath
    return $ filter isJpg contents

main :: IO ()
main = do 
    let (w, h) = (200, 600)
    getAndResizeImage "download.jpg" "saved.jpg" w h
    imageNames <- getImageNames 
    let n = length imageNames
    mapM_ putStrLn imageNames
    

