-- Main.hs

module Main where

-- import Foreign.Ptr
-- import Foreign.Marshal.Array
import MachineCode (printByteString)
import qualified Data.ByteString.Char8 as C

removeParenthesesAtEnd :: String -> String
removeParenthesesAtEnd str =
  if lastTwoCharacters str == "()" 
    then init (init str)
    else str
  where
    lastTwoCharacters s = drop (length s - 2) s

main :: IO ()
main = do
    let cString = C.pack "Hello, World!"
    putStrLn "ByteString:"
    putStrLn (removeParenthesesAtEnd (show (printByteString cString)))
    putStrLn "Done!"