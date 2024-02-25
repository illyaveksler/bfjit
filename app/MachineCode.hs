
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
    let memSize = length code
    memPtr <- mmap nullPtr (fromIntegral memSize) (protRead .|. protExec .|. protWrite) (mapPrivate .|. mapAnon) (-1) 0
    if memPtr == nullPtr
        then putStrLn "Failed to allocate memory"
        else do
            putStrLn "Memory allocated successfully"
            -- Fill memory with some executable code
            forM_ (zip [0..] code) $ \(i, c) -> do
                pokeArray (memPtr `plusPtr` i) [c]
            putStrLn "Memory copied successfully"
            -- Cast memory pointer to a function pointer and call the function
            let funPtr = castPtrToFunPtr memPtr
            result <- c_call funPtr -- MODIFY HERE: tsoding also passed in a section of allocated memory for the "tape" that the pointer moves along and modifies
            putStrLn $ "Function returned: " ++ show result
            -- Clean up
            result_unmap <- munmap memPtr (fromIntegral memSize)
            if result_unmap /= 0
                then putStrLn "Failed to unmap memory"
                else putStrLn "Memory unmapped successfully"

-- | C function to call the executable memory
foreign import ccall unsafe "dynamic"
  c_call :: FunPtr (IO Int) -> IO Int

-- | memcpy C function
foreign import ccall unsafe "string.h memcpy"
  memcpy :: Ptr a -> Ptr b -> CSize -> IO ()

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
