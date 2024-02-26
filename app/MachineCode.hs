{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- MachineCode.hs

module MachineCode (executeMachineCode) where

import Foreign.Ptr ( Ptr, FunPtr, castPtrToFunPtr, nullPtr )

import Foreign ( Bits((.|.)), pokeArray, plusPtr )
import Foreign.C.Types ( CInt(..), CSize(..) )
import System.Posix.Types ( COff(..) )
import Data.Foldable (forM_)

foreign import ccall unsafe "sys/mman.h mmap"
    mmap :: Ptr a -> CSize -> CInt -> CInt -> CInt -> COff -> IO (Ptr ())

foreign import ccall unsafe "sys/mman.h munmap"
    munmap :: Ptr a -> CSize -> IO CInt


executeMachineCode :: String -> IO ()
executeMachineCode code = do
    if code == "\xC3"
        then putStrLn "No valid brainfuck code found."
        else do
        let memSize = length code
        codeMPtr <- mmap nullPtr (fromIntegral memSize) (protRead .|. protExec .|. protWrite) (mapPrivate .|. mapAnon) (-1) 0
        if codeMPtr == nullPtr
            then putStrLn "Failed to allocate memory"
            else do
                putStrLn "Memory allocated successfully"
                -- Fill memory with some executable code
                forM_ (zip [0..] code) $ \(i, c) -> do
                    pokeArray (codeMPtr `plusPtr` i) [c]
                putStrLn "Memory copied successfully"
                -- Allocate working memory too
                wmemPtr <- mmap nullPtr (fromIntegral jitMemoryCap) (protRead .|. protExec .|. protWrite) (mapPrivate .|. mapAnon) (-1) 0
                if wmemPtr == nullPtr
                    then putStrLn "Failed to allocate working memory"
                    else do
                        putStrLn "Successfully allocated working memory"
                        putStrLn "Making sure working memory is clear..."
                        forM_ [0..jitMemoryCap] $ \i -> do
                            pokeArray (wmemPtr `plusPtr` i) ['\x00']
                        putStrLn "Done!"
                        -- Cast memory pointer to a function pointer and call the function
                        let funPtr = castPtrToFunPtr codeMPtr
                        putStrLn "Running..."
                        result <- runCode funPtr wmemPtr -- MODIFY HERE: tsoding also passed in a section of allocated memory for the "tape" that the pointer moves along and modifies (essentially the only difference)
                                                         -- this worked for tsoding because the argument passed in was stored in rdi on compile; by passing in a pointer to alloc'd memory, the brainfuck pointer was set
                                                         -- and ready
                        putStrLn $ "\nFunction returned: " ++ show result
                        -- Clean up
                        res_unmap_wmemPtr <- munmap wmemPtr (fromIntegral jitMemoryCap)
                        if res_unmap_wmemPtr /= 0
                            then putStrLn "Failed to unmap working memory"
                            else do 
                                putStrLn "Working memory unmapped successfully"
                                result_unmap <- munmap codeMPtr (fromIntegral memSize)
                                if result_unmap /= 0
                                    then putStrLn "Failed to unmap code memory"
                                    else putStrLn "Code memory unmapped successfully"

-- | C function to call the executable memory
foreign import ccall unsafe "dynamic"
  c_call :: FunPtr (IO Int) -> IO Int

-- | memcpy C function
foreign import ccall unsafe "string.h memcpy"
  memcpy :: Ptr a -> Ptr b -> CSize -> IO ()

foreign import ccall unsafe "wrapper.h runCode"
  runCode :: FunPtr (IO Int) -> Ptr a -> IO Int

-- | Constants for mmap
protRead :: CInt
protRead = 0x1

protWrite :: CInt
protWrite = 0x2

protExec :: CInt
protExec = 0x4

mapPrivate :: CInt
mapPrivate = 0x02

mapAnon :: CInt
mapAnon = 0x20

jitMemoryCap :: Int
jitMemoryCap = (10*1000*1000)
