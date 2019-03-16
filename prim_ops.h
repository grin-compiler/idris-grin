#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <stdbool.h>
#include <string.h>

typedef char* string;

// primop effectful

//:: T_Int64  -> T_Unit
void _prim_int_print(int64_t p1);

//:: T_String -> T_Unit
void _prim_string_print(string p1);

//   :: T_String
string _prim_read_string();

//         :: T_Int64 -> T_Unit
void _prim_usleep(int64_t p1);

//          :: T_String -> T_Unit
void _prim_error(string p1);

//   :: T_Int64 -> T_Int64
int64_t _prim_ffi_file_eof(int64_t p1);

// primop pure

// String

//  :: T_String -> T_String -> T_String
string _prim_string_concat(string p1, string p2);

// :: T_String -> T_String
string _prim_string_reverse(string p1);

//      :: T_String -> T_String -> T_Bool
int64_t _prim_string_lt(string p1, string p2);

//      :: T_String -> T_String -> T_Bool
int64_t _prim_string_eq(string p1, string p2);

//     :: T_String -> T_Int64 -- TODO: Change to Char
int64_t _prim_string_head(string p1);

//     :: T_String -> T_String
string _prim_string_tail(string p1);

//     :: T_Int64  -> T_String -> T_String
string _prim_string_cons(int64_t p1, string p2);

//      :: T_String -> T_Int64
int64_t _prim_string_len(string p1);

// Conversion

//      :: T_Int64 -> T_String
string _prim_int_str(int64_t p1);

// :: T_String -> T_Int64
int64_t _prim_str_int(string p1);

//     :: T_Int64 -> T_Float
float _prim_int_float(int64_t p1);

//  :: T_Float -> T_String
string _prim_float_string(float p1);

//     :: T_Char  -> T_Int64
int64_t _prim_char_int(char p1);
