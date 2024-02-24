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
import MachineCode (executeMachineCode)
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
    let parameterBytes = [0x68, 0x61, 0x73, 0x6B, 0x65, 0x6C, 0x6C] -- "haskell\NUL"
    let parameterByteString = BS.pack parameterBytes

    let machineCodeBytes = [0xC3]
    let machineCodeByteString = BS.pack machineCodeBytes
  --  0x00000000004000be:  48 c7 c7 01 00 00 00    mov    $0x1,%rdi
  --  0x00000000004000c5:  48 c7 c0 01 00 00 00    mov    $0x1,%rax
  --  0x00000000004000cc:  0f 05   syscall 
  --  0x00000000004000ce:  c3      ret    
    putStrLn (removeParenthesesAtEnd (show (executeMachineCode machineCodeByteString parameterByteString)))