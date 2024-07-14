#!/usr/bin/env stack
{- stack script --resolver lts-20.26
  --optimize
  --package alsa-core
  --package alsa-mixer
  --package process
-}

import Sound.ALSA.Mixer
import System.IO (hGetLine, hIsEOF)
import System.Process
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, when)

main :: IO ()
main = do
  -- toggleMute
  getCurrentVolume
  alsactlMonitor


alsactlMonitor :: IO ()
alsactlMonitor = do
  let process = (proc "alsactl" ["monitor"]) { std_out = CreatePipe }
  (_, Just output, _, handle) <- createProcess process

  let readOutput = do
        eof <- hIsEOF output
        if eof
          then putStrLn "EOF reached"
          else do
                line <- hGetLine output
                putStrLn line
                readOutput

  _ <- forkIO readOutput
  waitForProcess handle
  return ()

toggleMute :: IO ()
toggleMute =
  withMixer "default" $ \mixer ->
    do Just control <- getControlByName mixer "Master"
       let Just playbackSwitch = playback $ switch control
       Just switchState <- getChannel FrontLeft playbackSwitch
       let newSwitchState = not switchState
       setChannel FrontLeft playbackSwitch newSwitchState
       putStrLn $ "Current switch state: " ++ (if newSwitchState then "On" else "Off")

getCurrentVolume :: IO ()
getCurrentVolume =
  withMixer "default" $ \mixer ->
    do Just control <- getControlByName mixer "Master"
       let Just playbackVolume = playback $ volume control
       (min, max) <- getRange playbackVolume
       Just vol <- getChannel FrontLeft $ value $ playbackVolume
       putStrLn $ "Current volume: " ++ show vol

