-- CPSC 312 |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| --
--                                                                                   --
--                                                                                   --
-- PROJECT - SKULLFUCK                                                               --
-- MachineCode.hs                                 CWL HERE - Illya Vekslyer STDNTNUM --
--                                                                                   --
-- ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| --



-- MachineCode.hs

{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE GHCForeignImportPrim, UnliftedFFITypes, UnboxedTuples #-}

module MachineCode (executeMachineCode) where

import Language.Asm.Inline
import Language.Asm.Inline.QQ
import Data.ByteString
--   lea {parameter:ptr}, %rsi
--  After mov {parameter:len}
defineAsmFun "executeMachineCode"
  [asmTy| (machineCode : ByteString) (parameter : ByteString) | |]
  [asm|
  test {machineCode:len}, {machineCode:len}
  jz invalidMachineCode

  mov {parameter:len}, %rdx
  mov $1, %rdi
  mov {parameter:ptr}, %rsi
  mov $1, %rax
  syscall

  call *{machineCode:ptr}
  invalidMachineCode:
  RET_HASK 
  |]

-- defineAsmFun "printByteString"
--   [asmTy| (bs : ByteString) | |]
--   [asm|
--   test {bs:len}, {bs:len}
--   jz is_zero
--   mov $1, %rdi
--   mov {bs:ptr}, %rsi
--   mov {bs:len}, %rdx
--   mov $1, %rax
--   syscall
--   is_zero:
--   |]