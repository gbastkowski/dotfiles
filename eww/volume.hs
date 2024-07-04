#!/usr/bin/env stack
{- stack script --resolver lts-17.9
  --optimize
  --package alsa-mixer
  --package process
-}

main :: IO ()

main = putStrLn "Hello, World"
