
import Data.Bits

tohexbyte n = [(tohex (shift n (-4))),(tohex (n .&. 15))]

tohex n
    | n == 10 = 'A'
    | n == 11 = 'B'
    | n == 12 = 'C'
    | n == 13 = 'D'
    | n == 14 = 'E'
    | n == 15 = 'F'
    | otherwise = head (show n)