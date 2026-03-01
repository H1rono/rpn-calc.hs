module Main (main) where

import Control.Monad (forM_)

import Rpn (solve)

main :: IO ()
main = do
    contents <- getContents
    forM_ (lines contents) $ \line -> do
        answer <- solve line
        print answer
