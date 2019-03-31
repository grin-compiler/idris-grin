#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <stdbool.h>
#include <string.h>

struct string {
    char* data;
    int64_t length;
};

struct string* create_string_len(int64_t l);
struct string* create_string_copy(char *str);

// ASSUMPTION: The buffer has enough memory allocated to store the string
void cstring(char* buffer, struct string* s);

void _prim_string_print(struct string* p1);
void _prim_int_print(int64_t p1);
struct string* _prim_read_string();
void _prim_usleep(int64_t p1);
void _prim_error(struct string* p1);
int64_t _prim_ffi_file_eof(int64_t p1);
struct string* _prim_string_concat(struct string* p1, struct string* p2);
struct string* _prim_string_reverse(struct string* p1);
int64_t _prim_string_eq(struct string* p1, struct string* p2);
int64_t _prim_string_head(struct string* p1);
int64_t _prim_string_len(struct string* p1);
struct string* _prim_string_tail(struct string* p1);
struct string* _prim_string_cons(int64_t p1, struct string* p2);
int64_t _prim_string_lt(struct string* p1, struct string* p2);
struct string* _prim_int_str(int64_t p1);
int64_t _prim_str_int(struct string* p1);
float _prim_int_float(int64_t p1);
struct string* _prim_float_string(float p1);
int64_t _prim_char_int(char p1);
