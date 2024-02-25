-- CPSC 312 |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| --
--                                                                                   --
--                                                                                   --
-- PROJECT - SKULLFUCK                                                               --
-- skullfuck.hs                                  aurus (e7q7w) - Angus Chow 10099935 --
--                                                                                   --
-- ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| --


module Skullfuck where
import qualified Data.ByteString.Char8 as BS
import Data.Bits
import Data.Char


----                                                                               ----
sfmain :: IO ()
sfmain = do
    putStrLn ("Enter your Brainfuck code:")
    input <- getLine
    putStrLn ("Enter your file name:")
    fileNameRaw <- getLine
    let fileName = (foldr (\x y -> if (elem x "<>:\"/\\|?*") 
                                    then (y) 
                                    else (x:y)) [] fileNameRaw)
    let machineCode = (brainfuckToML input)
    putStrLn ("Writing to " ++ fileName ++ ".elf :")
    if (machineCode == []) 
        then (putStrLn ("No valid brainfuck code found.")) 
        else do
            BS.writeFile (fileName++".elf") (BS.pack (compileMLtoELF machineCode))
    
----                                                                               ----

-- ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| --
brainfuckToML :: String -> String
brainfuckToML code = (bfCodeStringsCompile 
                        (bfApplyBackpatchHelper 
                            (bfTuplesToStrings 
                                (brainfuckToTuples code)))) ++ "\xC3"
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
brainfuckToTuples :: String -> [(BFCommand, Int)]
brainfuckToTuples [] = []
brainfuckToTuples str = reverse (brainfuckToTuplesHelper (brainfuckOnly str) [] [])

brainfuckOnly :: String -> String
brainfuckOnly str = foldr (\x y -> if (elem x "<>+-.,[]") then x:y else y) [] str

brainfuckToTuplesHelper :: String -> [(BFCommand, Int)] -> [Int] -> [(BFCommand, Int)]
brainfuckToTuplesHelper []    out [] = out         -- FINISHED READING
brainfuckToTuplesHelper []    _   (_:_) = []       -- ERROR: MISMATCHED LOOPS
brainfuckToTuplesHelper (_:_) []  (_:_) = []       -- how did you get here?

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
bfTuplesReplaceAt :: [t] -> Int -> t -> [t]
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

incrValToML :: Int -> (String, Int, Int)
incrValToML count = ("\x80\x07"++[chr (count .&. 255)],3, -1)

decrValToML :: Int -> (String, Int, Int)
decrValToML count = ("\x80\x2F"++[chr (count .&. 255)],3, -1)

incrPtrToML :: Int -> (String, Int, Int)
incrPtrToML count = ("\x48\x81\xC7"++(intToUInt32Bytes count),7, -1)

decrPtrToML :: Int -> (String, Int, Int)
decrPtrToML count = ("\x48\x81\xEF"++(intToUInt32Bytes count),7, -1)

inpCharToML :: Int -> (String, Int, Int)
inpCharToML count = (callMultiple ("",0,-1) count inputCall)

outCharToML :: Int -> (String, Int, Int)
outCharToML count = (callMultiple ("",0,-1) count outputCall)

loopSttToML :: Int -> (String, Int, Int)
loopSttToML index = ((fst loopStartCall), (snd loopStartCall), index)

loopEndToML :: Int -> (String, Int, Int)
loopEndToML index = ((fst loopEndCall), (snd loopEndCall), index)



---- TUPLE->MLS HELPERS AND INTERMEDIATE DATA
callMultiple :: (String, Int, Int) -> Int -> (String, Int) -> (String, Int, Int)
callMultiple result 0 _ = result
callMultiple result count input = 
                callMultiple (((fst_3 result) ++ (fst input)), 
                              ((snd_3 result)+(snd input)), 
                                trd_3 result) 
                             (count-1) input

intToUInt32Bytes :: Int -> String
intToUInt32Bytes n = [
    (chr (n .&. 255)),
    (chr ((shift n (-8)) .&. 255)),
    (chr ((shift n (-16)) .&. 255)),
    (chr ((shift n (-24)) .&. 255))
    ]

-- read 0, pointer, 1
inputCall :: (String, Int)
inputCall = (("\x57" ++
              "\x48\xC7\xC0\x00\x00\x00\x00" ++
              "\x48\x89\xFE" ++
              "\x48\xC7\xC7\x00\x00\x00\x00" ++
              "\x48\xC7\xC2\x01\x00\x00\x00" ++
              "\x0F\x05" ++
              "\x5F"), 
             28)

-- write 1, pointer, 1
outputCall :: (String, Int)
outputCall = (("\x57" ++
               "\x48\xC7\xC0\x01\x00\x00\x00" ++
               "\x48\x89\xFE" ++
               "\x48\xC7\xC7\x01\x00\x00\x00" ++
               "\x48\xC7\xC2\x01\x00\x00\x00" ++
               "\x0F\x05" ++
               "\x5F"),
              28)
              
loopStartCall :: (String, Int)
loopStartCall = (("\x8A\x07" ++
                  "\x84\xC0" ++
                  "\x0F\x84"),
                 10)
                 
loopEndCall :: (String, Int)
loopEndCall = (("\x8A\x07" ++
                "\x84\xC0" ++
                "\x0F\x85"),
               10)





---- INTERMEDIATE TUPLE REPRESENTATION TO MACHINE SNIPPET REPRESENTATION (MSR)
bfTuplesToStrings :: [(BFCommand, Int)] -> ([String], [Int], [(Int, Int, Int)])
bfTuplesToStrings [] = ([],[],[])
bfTuplesToStrings lst = bfTuplesToStringsHelper (map bfcTupleToML lst) 0 0 ([],[],[])

bfTuplesToStringsHelper :: [(String, Int, Int)] -> Int -> Int -> ([String], [Int], [(Int, Int, Int)]) -> ([String], [Int], [(Int, Int, Int)])
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
fst_3 :: (t0, t1, t2) -> t0
fst_3 (x, _, _) = x

snd_3 :: (t0, t1, t2) -> t1
snd_3 (_, x, _) = x

trd_3 :: (t0, t1, t2) -> t2
trd_3 (_, _, x) = x





---- MSR BACKPATCHING
bfApplyBackpatchHelper :: ([String], [Int], [(Int, Int, Int)]) -> ([String], [Int], [(Int, Int, Int)])
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
bfCodeStringsCompile :: ([String], t0, t1) -> String
bfCodeStringsCompile ([], _, _) = []
bfCodeStringsCompile ((hc:tc), _, _) = hc ++ bfCodeStringsCompile (tc, [], [])





---- ELF writing
elfHead1 :: String
elfHead1 =     ("\x7F\x45\x4C\x46"                 ++ "\x02"        ++ "\x01"             ++ "\x01\x00\x00"   ++   "\x00\x00\x00\x00\x00\x00\x00" ++
                "\x02\x00"                         ++ "\x3E\x00"    ++ "\x01\x00\x00\x00" ++                   "\x80\x00\x00\x04\x00\x00\x00\x00" ++
                "\x40\x00\x00\x00\x00\x00\x00\x00")
-- 40 bytes     7F ELF (magic num)                 ++ x64           ++  little-endian     ++ current ELF ver  ++                          padding
--              executable                         ++ AMD x86-64    ++  original ELF ver  ++                       entry at 0x0000 0400 0000 0080
--              program header at 0x40
-- ADD:                                                                                                                     section header offset
-- (8 bytes)
-- SUBSECTION TOTAL 48 BYTES || RUNNING TOTAL  48 BYTES (0x30) || ADDRESS 0x0000 0000 0400 0000 - 0x0000 0000 0400 002f

elfHead2 :: String
elfHead2 =     ("\x00\x00\x00\x00"                 ++ "\x40\x00"    ++ "\x38\x00"         ++ "\x01\x00"       ++ "\x40\x00" ++
                                                                                             "\x02\x00"       ++ "\x01\x00")
-- 12 bytes     processor flags                    ++ 64-bit header ++  56 byte x64 phead ++ 2 sections phead ++ 64-bit header entry
--                                                                              n sections in sect head table ++ section header name section index
-- SUBSECTION TOTAL 16 BYTES || RUNNING TOTAL  64 BYTES (0x40) || ADDRESS 0x0000 0000 0400 0030 - 0x0000 0000 0400 003f



progHead1 :: String
progHead1 =    ("\x01\x00\x00\x00"                 ++ "\x01\x00\x00\x00" ++        "\x00\x00\x00\x00\x00\x00\x00\x00" ++
                "\x00\x00\x00\x04\x00\x00\x00\x00" ++                              "\x00\x00\x00\x04\x00\x00\x00\x00")
-- 32 bytes     loadable segment                   ++ read+exec  segment ++                       zero program offset
--              0x0000 0000 0400 0000 vaddr memory ++                          0x0000 0000 0400 0000 phys addr memory
-- ADD:         program segment size on file       ++                                          program size on memory
-- (8 bytes + 8 bytes)
-- SUBSECTION TOTAL 48 BYTES || RUNNING TOTAL 112 BYTES (0x70) || ADDRESS 0x0000 0000 0400 0040 - 0x0000 0000 0400 006f

progHead2 :: String
progHead2 =    ("\x00\x00\x00\x00\x00\x00\x00\x00"                       ++        "\x00\x00\x00\x00\x00\x00\x00\x00")
-- 16 bytes      program alignment - 0 (p_vaddr = p_offset + p_align)    ++                      padding 0x78 -> 0x7f
-- SUBSECTION TOTAL 16 BYTES || RUNNING TOTAL 128 BYTES (0x7f) || ADDRESS 0x0000 0000 0400 0070 - 0x0000 0000 0400 007f








sectNames :: String
sectNames =    ("\x2Eshstrtab\x00"   ++ "\x2Etext\x00")
-- 16 bytes     .shstrtab (10 bytes) ++ .text (6 bytes)
-- SUBSECTION TOTAL 16 BYTES || SECOND TOTAL 16 BYTES 

sectHStrTab1 :: String
sectHStrTab1 = ("\x00\x00\x00\x00"   ++ "\x03\x00\x00\x00" ++ "\x00\x00\x00\x00\x00\x00\x00\x00" ++              "\x00\x00\x00\x00\x00\x00\x00\x00")
-- 24 bytes     name offset zero     ++ String table       ++ no flags                           ++                          n on-loading (no addr)
-- ADD:         section offset in data
-- (8 bytes)
-- SUBSECTION TOTAL 32 BYTES || THIRD TOTAL 32 BYTES

sectHStrTab2 :: String
sectHStrTab2 = ("\x10\x00\x00\x00\x00\x00\x00\x00"         ++ "\x00\x00\x00\x00"                 ++                              "\x00\x00\x00\x00" ++
                "\x00\x00\x00\x00\x00\x00\x00\x00"         ++ "\x00\x00\x00\x00\x00\x00\x00\x00")
-- 16 bytes     16 byte size                               ++ link - none                        ++                               other info - none
--              no alignment                               ++ not fixed size
-- SUBSECTION TOTAL 32 BYTES || THIRD TOTAL 64 BYTES




sectText1 :: String
sectText1 =    ("\x0a\x00\x00\x00"   ++ "\x01\x00\x00\x00" ++ "\x06\x00\x00\x00\x00\x00\x00\x00" ++              "\x80\x00\x00\x04\x00\x00\x00\x00" ++
                "\x80\x00\x00\x00\x00\x00\x00\x00")
-- 32 bytes     name offset 10       ++ program            ++ occupies memory, executable        ++ virtual memory address at 0x0800 0000 0400 0000
--              offset 0x80
-- SUBSECTION TOTAL 32 BYTES || FOURTH TOTAL 32 BYTES



-- ADD:         size of code section
-- (8 bytes)
-- SUBSECTION TOTAL  8 BYTES || FOURTH TOTAL 40 BYTES



sectText2 :: String
sectText2 =    (                                              "\x00\x00\x00\x00"                 ++                              "\x00\x00\x00\x00" ++
                "\x00\x00\x00\x00\x00\x00\x00\x00"         ++ "\x00\x00\x00\x00\x00\x00\x00\x00")
-- 24 bytes                                                    link - none                        ++                               other info - none
--              no alignment                               ++ not fixed size
-- SUBSECTION TOTAL 24 BYTES || FOURTH TOTAL 64 BYTES



intToUInt64Bytes :: Integer -> String
intToUInt64Bytes n = [
    (chr (fromInteger (n .&. 255))),
    (chr (fromInteger ((shift n ( -8)) .&. 255))),
    (chr (fromInteger ((shift n (-16)) .&. 255))),
    (chr (fromInteger ((shift n (-24)) .&. 255))),
    
    (chr (fromInteger ((shift n (-32)) .&. 255))),
    (chr (fromInteger ((shift n (-40)) .&. 255))),
    (chr (fromInteger ((shift n (-48)) .&. 255))),
    (chr (fromInteger ((shift n (-56)) .&. 255)))
    ]



compileMLtoELF :: String -> String
compileMLtoELF mlstr = compileMLtoELFHelper (padML mlstr) (toInteger (padSize mlstr))

compileMLtoELFHelper :: String -> Integer -> String
compileMLtoELFHelper padprog padsize =  elfHead1 ++
                                        intToUInt64Bytes (padsize + 16 + 128) ++       -- program size + 16 (names section size) + 128 (header size)
                                        elfHead2 ++ 
                                        progHead1 ++
                                        intToUInt64Bytes (padsize) ++                  -- program size
                                        intToUInt64Bytes (padsize) ++                  -- program size
                                        progHead2 ++
                                        padprog ++                                     -- padded mlstr
                                        sectNames ++
                                        sectHStrTab1 ++
                                        intToUInt64Bytes (padsize + 128) ++            -- program size + 128 (header size)
                                        sectHStrTab2 ++
                                        sectText1 ++
                                        intToUInt64Bytes (padsize) ++                  -- program size
                                        sectText2

padML mlstr = mlstr++(listRepeat '\x00' ((padSize mlstr) - (length mlstr)))

listRepeat :: a -> Int -> [a]
listRepeat item 0 = []
listRepeat item n = item:(listRepeat item (n-1))

padSize mlstr
    | (rem (length mlstr) 64 == 0) = ((length mlstr) + 64)
    | otherwise = ((quot (length mlstr) 64) + 1) * 64