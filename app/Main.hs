-- Main.hs

module Main where

import MachineCode (executeMachineCode)
import SkullfuckOnly (brainfuckToML)

main :: IO ()
main = do
--    c6 44 24 fd 36  movb   $0x36,-0x3(%rsp)
--    c6 44 24 fe 39  movb   $0x39,-0x2(%rsp)
--    c6 44 24 ff 0a  movb   $0xa,-0x1(%rsp)
--    48 c7 c2 03 00 00 00    mov    $0x3,%rdx
--    48 8d 74 24 fd  lea    -0x3(%rsp),%rsi
--    48 c7 c7 01 00 00 00    mov    $0x1,%rdi
--    48 c7 c0 01 00 00 00    mov    $0x1,%rax
--    0f 05   syscall 
--    c3      ret    
--    executeMachineCode "\xc6\x44\x24\xfd\x36\xc6\x44\x24\xfe\x39\xc6\x44\x24\xff\x0a\x48\xc7\xc2\x03\x00\x00\x00\x48\x8d\x74\x24\xfd\x48\xc7\xc7\x01\x00\x00\x00\x48\xc7\xc0\x01\x00\x00\x00\x0f\x05\xc3"
      executeMachineCode (brainfuckToML "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")