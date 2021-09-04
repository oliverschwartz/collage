module Main where

import Space
import ImageProcessor

main :: IO ()
main = do 
    let (w, h) = (200, 400)
    getAndResizeImage "download.jpg" "saved.jpg" w h
    putStrLn "done"
    

