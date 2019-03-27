#include <stdio.h>
#include <stdlib.h>
#include "prim_ops.h"

// Compile with
// clang-7 prim_ops.c test_prim_ops.c -o test_prim_ops

int main() {

    struct string* r = create_string_len(0);
    struct string* s1;
    struct string* s2;
    struct string* s3;
    struct string* s4;
    printf("%d %ld\n\n", (int)r->data, r->length);
    r = create_string_copy("");
    printf("%d %ld\n\n", (int)r->data, r->length);
    _prim_string_print(r);
    r = create_string_copy("1");
    printf("%d %ld\n\n", (int)r->data, r->length);
    _prim_string_print(r);
    r = create_string_copy("12345");
    printf("%d %ld\n\n", (int)r->data, r->length);
    _prim_string_print(r);
    printf("\n");

    s1 = create_string_copy("Hello.");
    s2 = create_string_copy("World");
    s3 = create_string_len(0);
    s4 = create_string_copy("");

    _prim_string_print(_prim_string_concat(s1,s2));
    printf("\n");
    _prim_string_print(_prim_string_concat(s1,s3));
    printf("\n");
    _prim_string_print(_prim_string_concat(s3,s2));
    printf("\n");
    _prim_string_print(_prim_string_concat(s1,s4));
    printf("\n");
    _prim_string_print(_prim_string_concat(s4,s2));
    printf("\n");
    _prim_string_print(_prim_string_concat(s3,s4));
    printf("\n");
    _prim_string_print(_prim_string_concat(s3,s4));
    printf("\n");

    _prim_string_print(_prim_string_reverse(create_string_copy("")));
    printf("\n");
    _prim_string_print(_prim_string_reverse(create_string_copy("a")));
    printf("\n");
    _prim_string_print(_prim_string_reverse(create_string_copy("ab")));
    printf("\n");
    _prim_string_print(_prim_string_reverse(create_string_copy("abc")));
    printf("\n");

    printf("%ld\n", _prim_string_eq(s1,s1));
    printf("%ld\n", _prim_string_eq(s2,s2));
    printf("%ld\n", _prim_string_eq(s3,s3));
    printf("%ld\n", _prim_string_eq(s4,s4));
    printf("%ld\n", _prim_string_eq(s3,s4));
    printf("%ld\n", _prim_string_eq(s1,s2));
    printf("%ld\n", _prim_string_eq(s3,s1));

    printf("%c\n", (char)_prim_string_head(s1));

    _prim_string_print(_prim_string_tail(create_string_copy("a")));
    printf("\n");
    _prim_string_print(_prim_string_tail(create_string_copy("ab")));
    printf("\n");
    _prim_string_print(_prim_string_tail(create_string_copy("abc")));
    printf("\n");

    _prim_string_print(_prim_string_cons(65, s3));
    printf("\n");
    _prim_string_print(_prim_string_cons(65, create_string_copy("b")));
    printf("\n");
    _prim_string_print(_prim_string_cons(65, create_string_copy("bc")));
    printf("\n");

    printf("0 == %ld\n", _prim_string_lt(create_string_copy(""), create_string_copy("")));
    printf("1 == %ld\n", _prim_string_lt(create_string_copy(""), create_string_copy("a")));
    printf("0 == %ld\n", _prim_string_lt(create_string_copy("a"), create_string_copy("a")));
    printf("1 == %ld\n", _prim_string_lt(create_string_copy("a"), create_string_copy("aa")));
    printf("1 == %ld\n", _prim_string_lt(create_string_copy("aa"), create_string_copy("ab")));
    printf("1 == %ld\n", _prim_string_lt(create_string_copy("aa"), create_string_copy("ab")));
    printf("1 == %ld\n", _prim_string_lt(create_string_copy("aaa"), create_string_copy("ab")));
    printf("0 == %ld\n", _prim_string_lt(create_string_copy("aaa"), create_string_copy("")));
    printf("0 == %ld\n", _prim_string_lt(create_string_copy("bbb"), create_string_copy("aaa")));

    _prim_string_print(_prim_int_str(0));
    printf("\n");
    _prim_string_print(_prim_int_str(10));
    printf("\n");
    _prim_string_print(_prim_int_str(-10));
    printf("\n");

    printf("0 == %ld\n", _prim_str_int(create_string_copy("0")));
    printf("10 == %ld\n", _prim_str_int(create_string_copy("10")));
    printf("+10 == %ld\n", _prim_str_int(create_string_copy("+10")));
    printf("-10 == %ld\n", _prim_str_int(create_string_copy("-10")));

    _prim_string_print(_prim_float_string(0.0));
    printf("\n");
    _prim_string_print(_prim_float_string(10.123));
    printf("\n");
    _prim_string_print(_prim_float_string(-10.34));
    printf("\n");

    printf("%d\n", feof(stdin));

    return 0;
}
