module Main where

import Space
import ImageProcessor

main :: IO ()
main = do 
    getAndResizeImage "download.jpg" "saved.jpg"
    putStrLn "done"
    

