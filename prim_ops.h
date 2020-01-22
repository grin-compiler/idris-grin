#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <stdbool.h>
#include <string.h>


// Float
float           _prim_float_floor(float p1);
float           _prim_float_ceil(float p1);
float           _prim_float_atan2(float p1, float p2);

// Int
int64_t        _prim_uint_div(int64_t p1, int64_t p2);
int64_t        _prim_int_rem(int64_t p1, int64_t p2);
int64_t        _prim_int_shl(int64_t p1, int64_t p2);
int64_t        _prim_int_lshr(int64_t p1, int64_t p2);
int64_t        _prim_int_and(int64_t p1, int64_t p2);

// String
struct string {
    char* data;
    int64_t length;
};

struct string* create_string_len(int64_t l);
struct string* create_string_copy(char *str);

// ASSUMPTION: The buffer has enough memory allocated to store the string
void cstring(char* buffer, struct string* s);

struct string*  _prim_string_concat(struct string* p1, struct string* p2);
struct string*  _prim_string_reverse(struct string* p1);
struct string*  _prim_string_tail(struct string* p1);
struct string*  _prim_string_cons(int64_t p1, struct string* p2);
int64_t         _prim_string_eq(struct string* p1, struct string* p2);
int64_t         _prim_string_head(struct string* p1);
int64_t         _prim_string_len(struct string* p1);
int64_t         _prim_string_lt(struct string* p1, struct string* p2);
int64_t         _prim_string_index(struct string* p1, int64_t p2);

// Conversion
int64_t         _prim_float_int(float p1);
int64_t         _prim_char_int(char p1);
int64_t         _prim_str_int(struct string* p1);
int64_t         _prim_int_int(int64_t p1);
float           _prim_int_float(int64_t p1);
float           _prim_string_float(struct string* p1);
struct string*  _prim_float_string(float p1);
struct string*  _prim_int_str(int64_t p1);
uint64_t        _prim_int_word(int64_t p1);

// System
int64_t         _prim_time();
int64_t         _prim_ffi_file_eof(int64_t p1);
struct string*  _prim_read_string();
void            _prim_usleep(int64_t p1);
void            _prim_crash(struct string* p1);
void            _prim_error(struct string* p1);
void            _prim_int_print(int64_t p1);
void            _prim_string_print(struct string* p1);
