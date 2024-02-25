#include "wrapper.h"

typedef struct {
    void (*run)(void *memory);
} CodeWrapper;

int runCode (void *code, void *mem) {
    int result = 0;
    CodeWrapper wrap = {0};
    wrap.run = code;
    wrap.run(mem);
    return result;
}