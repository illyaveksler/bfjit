-- CPSC 312 |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| --
--                                                                                   --
--                                                                                   --
-- PROJECT - SKULLFUCK                                                               --
-- MachineCode.hs                                 CWL HERE - Illya Vekslyer STDNTNUM --
--                                                                                   --
-- ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| --

-- Main.hs

module Main where

-- import Foreign.Ptr
-- import Foreign.Marshal.Array
import MachineCode (printByteString)
import qualified Data.ByteString as BS

removeParenthesesAtEnd :: String -> String
removeParenthesesAtEnd str =
  if lastTwoCharacters str == "()"
    then init (init str)
    else str
  where
    lastTwoCharacters s = drop (length s - 2) s

main :: IO ()
main = do
    let bytes = [0x68, 0x61, 0x73, 0x6B, 0x65, 0x6C, 0x6C] -- "haskell\NUL"
    let bs = BS.pack bytes
    putStrLn (removeParenthesesAtEnd (show (printByteString bs)))