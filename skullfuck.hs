import Data.Bits
import Data.Char

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

charToBFC :: Char -> BFCommand
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

-- first element of tuple is the ML string, second is byte length
bfcTupleToML :: (BFCommand, Int) -> (String, Int)
bfcTupleToML tuple
    | (fst tuple == IncrPtr) = incrPtrToML (snd tuple)
    | (fst tuple == DecrPtr) = decrPtrToML (snd tuple)
    | (fst tuple == IncrVal) = incrValToML (snd tuple)
    | (fst tuple == DecrVal) = decrValToML (snd tuple)
    | (fst tuple == OutChar) = outCharToML (snd tuple)
    | (fst tuple == InpChar) = inpCharToML (snd tuple)
    | (fst tuple == LoopStt) = ("TODO: LOOPSTT",0)
    | (fst tuple == LoopEnd) = ("TODO: LOOPEND",0)
    | otherwise = ("UNREACHABLE",0)

-- Machine Language Translation adapted from bfjit by tsoding
-- https://github.com/tsoding/bfjit
-- ( https://github.com/tsoding/bfjit/blob/main/bfjit.c )

incrValToML count = ("\x80\x07"++[chr (count .&. 255)],3)

decrValToML count = ("\x80\x2F"++[chr (count .&. 255)],3)

incrPtrToML count = ("\x48\x81\xC7"++(intToUInt32Bytes count),7)

decrPtrToML count = ("\x48\x81\xEF"++(intToUInt32Bytes count),7)

inpCharToML count = (callMultiple ("",0) count inputCall)

outCharToML count = (callMultiple ("",0) count outputCall)





callMultiple :: (String, Int) -> Int -> (String, Int) -> (String, Int)
callMultiple result 0 input = result
callMultiple result count input = 
                callMultiple (((fst result) ++ (fst input)), ((snd result)+(snd input))) (count-1) input

inputCall = (("\x57" ++
              "\x48\xc7\xc0\x01\x00\x00\x00" ++
              "\x48\x89\xfe" ++
              "\x48\xc7\xc7\x01\x00\x00\x00" ++
              "\x48\xc7\xc2\x01\x00\x00\x00" ++
              "\x0f\x05" ++
              "\x5f"), 
             28)

outputCall = (("\x57" ++
               "\x48\xc7\xc0\x00\x00\x00\x00" ++
               "\x48\x89\xfe" ++
               "\x48\xc7\xc7\x00\x00\x00\x00" ++
               "\x48\xc7\xc2\x01\x00\x00\x00" ++
               "\x0f\x05" ++
               "\x5f"),
              28)

intToUInt32Bytes n = [
    (chr (n .&. 255)),
    (chr ((quot n 256) .&. 255)),
    (chr ((quot n 65536) .&. 255)),
    (chr ((quot n 16777216) .&. 255))
    ]





readBrainfuck [] = []
readBrainfuck str = reverse (readBrainfuckHelper (brainfuckOnly str) [] [])

brainfuckOnly str = foldr (\x y -> if (elem x "<>+-.,[]") then x:y else y) [] str

readBrainfuckHelper [] out [] = out         -- FINISHED READING
readBrainfuckHelper [] out (hs:ts) = []     -- ERROR: MISMATCHED LOOPS

readBrainfuckHelper (hi:ti) [] []
    | (charToBFC hi) == BFInvalid = readBrainfuckHelper ti 
                                                        []  
                                                        [] -- SKIP INVALID
    | ((charToBFC hi) == LoopStt) = readBrainfuckHelper ti 
                                                        [((charToBFC hi),0)]
                                                        [0]
    | ((charToBFC hi) == LoopEnd) = []      -- ERROR: MISMATCHED LOOPS
    | otherwise = readBrainfuckHelper ti [((charToBFC hi),1)] []

readBrainfuckHelper (hi:ti) (hr:tr) []
    | (charToBFC hi) == BFInvalid = readBrainfuckHelper ti 
                                                        (hr:tr)
                                                        [] -- SKIP INVALID
    | ((charToBFC hi) == LoopStt) = readBrainfuckHelper ti 
                                                        (((charToBFC hi),(length (hr:tr))):(hr:tr))
                                                        [(length (hr:tr))]
    | ((charToBFC hi) == LoopEnd) = []      -- ERROR: MISMATCHED LOOPS
    | otherwise = if (fst hr) == (charToBFC hi) 
                    then readBrainfuckHelper ti (((fst hr),((snd hr)+1)):tr) [] 
                    else readBrainfuckHelper ti (((charToBFC hi),1):(hr:tr)) []

readBrainfuckHelper (hi:ti) (hr:tr) (hs:ts)
    | (charToBFC hi) == BFInvalid = readBrainfuckHelper ti 
                                                        (hr:tr)
                                                        (hs:ts) -- SKIP INVALID
    | ((charToBFC hi) == LoopStt) = readBrainfuckHelper ti 
                                                        (((charToBFC hi),(length (hr:tr))):(hr:tr))
                                                        ((length (hr:tr)):(hs:ts))
    | ((charToBFC hi) == LoopEnd) = readBrainfuckHelper ti 
                                                        (((charToBFC hi),hs):(hr:tr))
                                                        (ts)
    | otherwise = if (fst hr) == (charToBFC hi) 
                    then readBrainfuckHelper ti (((fst hr),((snd hr)+1)):tr) (hs:ts)
                    else readBrainfuckHelper ti (((charToBFC hi),1):(hr:tr)) (hs:ts)