// WASM import module bindings for Extism kernel functions.
//
// The Extism runtime provides kernel functions in the "extism:host/env" WASM
// module. GHC's `foreign import ccall` compiles to "env" module imports by
// default. This C file bridges the gap: each function uses
// __attribute__((import_module, import_name)) to declare the correct WASM
// import, then exposes a C-linkage wrapper that the Haskell FFI can call.
//
// Vendored from: https://github.com/extism/haskell-pdk/blob/main/src/extism-pdk.c

#include <stdint.h>

#define IMPORT(a, b) __attribute__((import_module(a), import_name(b)))

typedef uint64_t ExtismPointer;

#define DEFINE(name, t, ...)                                                   \
  IMPORT("extism:host/env", #name) extern t _##name(__VA_ARGS__);

DEFINE(input_length, uint64_t)
uint64_t extism_input_length() { return _input_length(); }

DEFINE(length, uint64_t, ExtismPointer)
uint64_t extism_length(ExtismPointer p) { return _length(p); }

DEFINE(alloc, ExtismPointer, uint64_t)
uint64_t extism_alloc(uint64_t n) { return _alloc(n); }

DEFINE(free, void, ExtismPointer)
void extism_free(uint64_t n) { return _free(n); }

DEFINE(input_load_u8, uint8_t, ExtismPointer)
uint8_t extism_input_load_u8(ExtismPointer p) { return _input_load_u8(p); }

DEFINE(input_load_u64, uint64_t, ExtismPointer)
uint64_t extism_input_load_u64(ExtismPointer p) { return _input_load_u64(p); }

DEFINE(output_set, void, ExtismPointer, uint64_t)
void extism_output_set(ExtismPointer p, uint64_t n) {
  return _output_set(p, n);
}

DEFINE(error_set, void, ExtismPointer)
void extism_error_set(ExtismPointer p) { _error_set(p); }

DEFINE(store_u8, void, ExtismPointer, uint8_t)
void extism_store_u8(ExtismPointer p, uint8_t x) { return _store_u8(p, x); }

DEFINE(load_u8, uint8_t, ExtismPointer)
uint8_t extism_load_u8(ExtismPointer p) { return _load_u8(p); }

DEFINE(store_u64, void, ExtismPointer, uint64_t)
void extism_store_u64(ExtismPointer p, uint64_t x) { return _store_u64(p, x); }

DEFINE(load_u64, uint64_t, ExtismPointer)
uint64_t extism_load_u64(ExtismPointer p) { return _load_u64(p); }
