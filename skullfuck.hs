data BFCommand = IncrPtr
               | DecrPtr
               | IncrVal
               | DecrVal
               | OutChar
               | InpChar
               | LoopStt
               | LoopEnd
               | BFInvalid
               deriving (Show, Eq)
               
charToBFC c
    | (c == '>') = IncrPtr
    | (c == '<') = DecrPtr
    | (c == '+') = IncrVal
    | (c == '-') = DecrVal
    | (c == '.') = OutChar
    | (c == ',') = InpChar
    | (c == '[') = LoopStt
    | (c == ']') = LoopEnd
    | otherwise = BFInvalid
               
readBrainfuck [] = []
readBrainfuck str = reverse (readBrainfuckHelper (brainfuckOnly str) [])

brainfuckOnly str = foldr (\x y -> if (elem x "<>+-.,[]") then x:y else y) [] str

readBrainfuckHelper [] out = out
readBrainfuckHelper (hi:ti) []
    | (charToBFC hi) == BFInvalid = readBrainfuckHelper ti []
    | otherwise = readBrainfuckHelper ti [((charToBFC hi),1)]
readBrainfuckHelper (hi:ti) (hr:tr)
    | (charToBFC hi) == BFInvalid = readBrainfuckHelper ti (hr:tr)
    | otherwise = if (fst hr) == (charToBFC hi) 
                    then readBrainfuckHelper ti (((fst hr),((snd hr)+1)):tr) 
                    else readBrainfuckHelper ti (((charToBFC hi),1):(hr:tr))