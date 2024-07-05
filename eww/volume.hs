#!/usr/bin/env stack
{- stack script --resolver lts-20.26
  --optimize
  --package alsa-core
  --package alsa-mixer
  --package process
-}

import Sound.ALSA.Mixer

main :: IO ()
main = do
  putStrLn "Hello, World"
  toggleMute

toggleMute :: IO ()
toggleMute =
  withMixer "default" $ \mixer ->
    do Just control <- getControlByName mixer "Master"
       let Just playbackSwitch = playback $ switch control
       Just sw <- getChannel FrontLeft playbackSwitch
       setChannel FrontLeft playbackSwitch $ not sw
