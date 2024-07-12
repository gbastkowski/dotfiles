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
  toggleMute

toggleMute :: IO ()
toggleMute =
  withMixer "default" $ \mixer ->
    do Just control <- getControlByName mixer "Master"
       element <- getElementByName mixer "Master"
       let Just playbackSwitch = playback $ switch control
       Just switchState <- getChannel FrontLeft playbackSwitch
       let newSwitchState = not switchState
       setChannel FrontLeft playbackSwitch newSwitchState
       putStrLn $ "Current switch state: " ++ (if newSwitchState then "On" else "Off")
