# Skullf█ck JIT Compiler (bfjit)

Skullf█ck JIT Compiler, or bfjit, is a just-in-time compiler designed specifically for Brainf█ck code execution on AMD64 Linux machines. This Haskell program provides optimized performance for Brainf█ck programs, allowing for efficient and speedy execution.

## Installation

To use the Skullf█ck JIT Compiler, follow these steps:

1. Clone the repository:
   ```sh
   git clone https://github.com/illyaveksler/bfjit.git
   ```
   
## Usage

The bfjit compiler takes Brainf█ck code as input and generates optimized machine code for execution. To run the compiler with `ghci`, follow these steps:

1. Start `ghci` with the compiled object files:
   ```sh
   ghci wrapper.o
   ```

2. Import the main module:
   ```sh
   :l Main
   ```

3. Run the Skullf█ck JIT Compiler:
   ```sh
   main ""
   ```

## Example Programs

Here are some example Brainf█ck programs that you can try with the Skullf█ck JIT Compiler:

### Hello World

```bf
+[-->-[>>+>-----<<]<--<---]>-.>>>+.>>..+++[.>]<<<<.+++.------.<<-.>>>>+.
```

### Cat Program

```bf
,[.,]
```

Feel free to experiment with these examples using the Skullf█ck JIT Compiler! Run your Brainf█ck programs with `ghci` as described above.
Skullf█ck was built with heavy referencing from [tsoding](https://github.com/tsoding)'s work on https://github.com/tsoding/bfjit.
