-- Main.hs

module Main where

import MachineCode (executeMachineCode)

main :: IO ()
main = do
    executeMachineCode "\xc3" -- ret