module Lib.Common.OS (isWin, isMac, getOS) where

import           System.Info (os)

win :: String
win = "Windows"

macOS :: String
macOS = "macOS"

isWin :: Bool
isWin = (==) win getOS

isMac :: Bool
isMac = (==) macOS getOS

getOS :: String
getOS = case os of
  "darwin"  -> macOS
  "mingw32" -> win
  osName    -> osName