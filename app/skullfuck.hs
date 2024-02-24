-- CPSC 312 |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| --
--                                                                                   --
--                                                                                   --
-- PROJECT - SKULLFUCK                                                               --
-- skullfuck.hs                                  aurus (e7q7w) - Angus Chow 10099935 --
--                                                                                   --
-- ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| --



import Data.Bits
import Data.Char


----                                                                               ----



----                                                                               ----

-- ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| --
brainfuckToML code = bfCodeStringsCompile 
                        (bfApplyBackpatchHelper 
                            (bfTuplesToStrings 
                                (brainfuckToTuples code)))
-- ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| --



---- DATA DEFINITIONS
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





---- STRING TO INTERMEDIATE TUPLE REPRESENTATION (ITR)
brainfuckToTuples [] = []
brainfuckToTuples str = reverse (brainfuckToTuplesHelper (brainfuckOnly str) [] [])

brainfuckOnly str = foldr (\x y -> if (elem x "<>+-.,[]") then x:y else y) [] str

brainfuckToTuplesHelper [] out [] = out         -- FINISHED READING
brainfuckToTuplesHelper [] out (hs:ts) = []     -- ERROR: MISMATCHED LOOPS

brainfuckToTuplesHelper (hi:ti) [] []
    | (charToBFC hi) == BFInvalid = brainfuckToTuplesHelper ti 
                                                        []  
                                                        []      -- SKIP INVALID
    | ((charToBFC hi) == LoopStt) = brainfuckToTuplesHelper ti 
                                                        [((charToBFC hi),0)]         -- TUPLE REPLACED BY CLOSER
                                                        [0]
    | ((charToBFC hi) == LoopEnd) = []                          -- ERROR: MISMATCHED LOOPS
    | otherwise = brainfuckToTuplesHelper ti [((charToBFC hi),1)] []

brainfuckToTuplesHelper (hi:ti) (hr:tr) []
    | (charToBFC hi) == BFInvalid = brainfuckToTuplesHelper ti 
                                                        (hr:tr)
                                                        []      -- SKIP INVALID
    | ((charToBFC hi) == LoopStt) = brainfuckToTuplesHelper ti 
                                                        (((charToBFC hi),0):(hr:tr)) -- TUPLE REPLACED BY CLOSER
                                                        [(length (hr:tr))]
    | ((charToBFC hi) == LoopEnd) = []                          -- ERROR: MISMATCHED LOOPS
    | otherwise = if (fst hr) == (charToBFC hi) 
                    then brainfuckToTuplesHelper ti (((fst hr),((snd hr)+1)):tr) [] 
                    else brainfuckToTuplesHelper ti (((charToBFC hi),1):(hr:tr)) []

brainfuckToTuplesHelper (hi:ti) (hr:tr) (hs:ts)
    | (charToBFC hi) == BFInvalid = brainfuckToTuplesHelper ti 
                                                        (hr:tr)
                                                        (hs:ts) -- SKIP INVALID
    | ((charToBFC hi) == LoopStt) = brainfuckToTuplesHelper ti 
                                                        (((charToBFC hi),0):(hr:tr)) -- TUPLE REPLACED BY CLOSER
                                                        ((length (hr:tr)):(hs:ts))
    | ((charToBFC hi) == LoopEnd) = brainfuckToTuplesHelper ti 
                                                        (((charToBFC hi),hs):
                                                            (bfTuplesReplaceAt (hr:tr) 
                                                                                hs   -- REPLACE OPENER TUPLE
                                                                               (LoopStt, (length (hr:tr)))))
                                                        (ts)
    | otherwise = if (fst hr) == (charToBFC hi) 
                    then brainfuckToTuplesHelper ti (((fst hr),((snd hr)+1)):tr) 
                                                    (hs:ts)
                    else brainfuckToTuplesHelper ti (((charToBFC hi),1):(hr:tr)) 
                                                    (hs:ts)



---- STRING->ITR HELPERS
-- Replaces an intermediate tuple with another, for use in the middle of list building.
bfTuplesReplaceAt wipList pos val = listReplaceHelper wipList ((length wipList)-1-pos) val

-- replaces a single element of a list at the given position.
listReplaceHelper :: [a] -> Int -> a -> [a]
listReplaceHelper [] _ _ = []
listReplaceHelper (hs:ts) pos val
    | pos < 0  = (hs:ts)
    | pos == 0 = (val:ts)
    | otherwise = hs:(listReplaceHelper ts (pos-1) val)





---- INTERMEDIATE TUPLE TO MACHINE LANGUAGE SNIPPETS (MLS)
-- first element of tuple is the ML string, second is byte length, third is backpatch
--                                                                    index indicator
bfcTupleToML :: (BFCommand, Int) -> (String, Int, Int)
bfcTupleToML tuple
    | (fst tuple == IncrPtr) = incrPtrToML (snd tuple)
    | (fst tuple == DecrPtr) = decrPtrToML (snd tuple)
    | (fst tuple == IncrVal) = incrValToML (snd tuple)
    | (fst tuple == DecrVal) = decrValToML (snd tuple)
    | (fst tuple == OutChar) = outCharToML (snd tuple)
    | (fst tuple == InpChar) = inpCharToML (snd tuple)
    | (fst tuple == LoopStt) = loopSttToML (snd tuple)
    | (fst tuple == LoopEnd) = loopEndToML (snd tuple)
    | otherwise = ("UNREACHABLE",0,-1)

-- Machine Language Translation adapted from bfjit by tsoding
-- https://github.com/tsoding/bfjit
-- ( https://github.com/tsoding/bfjit/blob/main/bfjit.c )

incrValToML count = ("\x80\x07"++[chr (count .&. 255)],3, -1)

decrValToML count = ("\x80\x2F"++[chr (count .&. 255)],3, -1)

incrPtrToML count = ("\x48\x81\xC7"++(intToUInt32Bytes count),7, -1)

decrPtrToML count = ("\x48\x81\xEF"++(intToUInt32Bytes count),7, -1)

inpCharToML count = (callMultiple ("",0,-1) count inputCall)

outCharToML count = (callMultiple ("",0,-1) count outputCall)

loopSttToML index = ((fst loopStartCall), (snd loopStartCall), index)

loopEndToML index = ((fst loopStartCall), (snd loopEndCall), index)



---- TUPLE->MLS HELPERS AND INTERMEDIATE DATA
callMultiple :: (String, Int, Int) -> Int -> (String, Int) -> (String, Int, Int)
callMultiple result 0 input = result
callMultiple result count input = 
                callMultiple (((fst_3 result) ++ (fst input)), 
                              ((snd_3 result)+(snd input)), 
                                trd_3 result) 
                             (count-1) input

intToUInt32Bytes n = [
    (chr (n .&. 255)),
    (chr ((quot n 256) .&. 255)),
    (chr ((quot n 65536) .&. 255)),
    (chr ((quot n 16777216) .&. 255))
    ]

-- read 0, pointer, 1
inputCall = (("\x57" ++
              "\x48\xC7\xC0\x00\x00\x00\x00" ++
              "\x48\x89\xFE" ++
              "\x48\xC7\xC7\x00\x00\x00\x00" ++
              "\x48\xC7\xC2\x01\x00\x00\x00" ++
              "\x0F\x05" ++
              "\x5F"), 
             28)

-- write 1, pointer, 1
outputCall = (("\x57" ++
               "\x48\xC7\xC0\x01\x00\x00\x00" ++
               "\x48\x89\xFE" ++
               "\x48\xC7\xC7\x01\x00\x00\x00" ++
               "\x48\xC7\xC2\x01\x00\x00\x00" ++
               "\x0F\x05" ++
               "\x5F"),
              28)
              
loopStartCall = (("\x8A\x07" ++
                  "\x84\xC0" ++
                  "\x0F\x84"),
                 10)
                 
loopEndCall = (("\x8A\x07" ++
                "\x84\xC0" ++
                "\x0F\x85"),
               10)





---- INTERMEDIATE TUPLE REPRESENTATION TO MACHINE SNIPPET REPRESENTATION (MSR)
bfTuplesToStrings [] = ([],[],[])
bfTuplesToStrings lst = bfTuplesToStringsHelper (map bfcTupleToML lst) 0 0 ([],[],[])

bfTuplesToStringsHelper [] _ _ result = result
bfTuplesToStringsHelper (ht:tt) cpos cindex (code, addrs, backpatches)
    | ((trd_3 ht) < 0) = bfTuplesToStringsHelper tt (cpos + (snd_3 ht)) (cindex+1) 
                                                 ((code ++ [(fst_3 ht)]),
                                                  (addrs ++ [cpos]),
                                                  backpatches)
    | otherwise = bfTuplesToStringsHelper tt (cpos + (snd_3 ht)) (cindex+1)
                                          (
                                           (code ++ [fst_3 ht]),
                                           
                                           (addrs ++ [cpos]),
                                           
                                           (((cpos + (snd_3 ht)),
                                             (trd_3 ht),
                                              cindex
                                             ):backpatches)
                                           )

---- TRIPLET TUPLE ACCESSORS
fst_3 (x, _, _) = x
snd_3 (_, x, _) = x
trd_3 (_, _, x) = x





---- MSR BACKPATCHING
bfApplyBackpatchHelper ([], [], []) = ([], [], [])
bfApplyBackpatchHelper (code, addrs, []) = (code, addrs, [])
bfApplyBackpatchHelper (code, addrs, (hp:tp)) = 
    bfApplyBackpatchHelper (
                            (listApplyAtHelper code (trd_3 hp) 
                                               (\x -> x ++ (intToUInt32Bytes 
                                                            ((addrs!!(snd_3 hp)) -
                                                             (fst_3 hp))))),
                                    --                         ^
                                    -- This index accessing should probably be 
                                    -- addressed at some point since it's not
                                    -- exactly safe but hey. if the code works there
                                    -- should never be a chance for that to fail
                            addrs,
                            tp
                            )



---- MSR BACKPATCHING HELPERS
-- applies a function to a single element of a list at the given position.
listApplyAtHelper :: [a] -> Int -> (a -> a) -> [a]
listApplyAtHelper [] _ _ = []
listApplyAtHelper (hs:ts) pos f
    | pos < 0  = (hs:ts)
    | pos == 0 = ((f hs):ts)
    | otherwise = hs:(listApplyAtHelper ts (pos-1) f)





---- FINAL BYTECODE COMPILATION
bfCodeStringsCompile ([], _, _) = []
bfCodeStringsCompile ((hc:tc), _, _) = hc ++ bfCodeStringsCompile (tc, [], [])