#include <stdio.h>
#include <stdlib.h>
#include "prim_ops.h"

int main() {

    // Test _prim_string_concat
    char* s1 = "Hello, ";
    char* s2 = "World!";
    char* res1 = _prim_string_concat(s1,s2);
    printf("%s\n", res1);

    // Test _prim_string_reverse
    char* s3 = "This is a test sentence!";
    char* res2 = _prim_string_reverse(s3);
    printf("%s\n", res2);

    // Test _prim_string_eq
    printf("%d\n", _prim_string_eq(s1,s2));
    printf("%d\n", _prim_string_eq(s2,s1));
    printf("%d\n", _prim_string_eq(s3,s3));

    // Test _prim_string_head
    printf("%d\n", _prim_string_head(s1));

    // Test _prim_string_tail
    printf("%s\n", _prim_string_tail(s3));

    // Test _prim_int_float
    printf("%f\n", _prim_int_float(100));

    // Test _prim_float_string
    printf("%s\n", _prim_float_string(100.0));

    // Test _prim_string_cons
    char *res3 = _prim_string_cons(65, s2);
    printf("%s\n", res3);

    // Test _prim_sting_lt
    printf("%d\n", _prim_string_lt(s1, s2));
    printf("%d\n", _prim_string_lt(s2, s1));
    printf("%d\n", _prim_string_lt(s3, s3));

    // Test _prim_usleep
    printf("Start sleeping 2 secs\n");
    _prim_usleep(2000 * 1000);
    printf("Woken up...\n");

    // Test _prim_ffi_file_eof
    printf("%d\n", _prim_ffi_file_eof(1));

    return 0;
}
