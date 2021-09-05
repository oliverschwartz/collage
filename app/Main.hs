module Main where

import ImageUtils


main :: IO ()
main = do 
    imageNames <- getJPGNames 
    mImg <- getImage $ head imageNames
    case mImg of 
        Nothing -> putStrLn "Failed to read image"
        Just img -> do
            let t = makeTree img 100 100
            let t' = addImage
            let resolved = resolveTree t
            saveImage "singleResolved.jpg" resolved
    -- mapM_ putStrLn imageNames
    

