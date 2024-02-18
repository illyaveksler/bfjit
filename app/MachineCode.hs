-- MachineCode.hs

{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE GHCForeignImportPrim, UnliftedFFITypes, UnboxedTuples #-}

module MachineCode (printByteString) where

import Language.Asm.Inline
import Language.Asm.Inline.QQ
import Data.ByteString
import Data.Word

defineAsmFun "printByteString"
  [asmTy| (bs : ByteString) | |]
  [asm|
  test {bs:len}, {bs:len}
  jz is_zero
  mov $1, %rdi
  mov {bs:ptr}, %rsi
  mov {bs:len}, %rdx
  mov $1, %rax
  syscall
  is_zero:
  |]