#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <stdbool.h>
#include <string.h>
#include <unistd.h>

typedef char* string;

// primop effectful

//:: T_Int64  -> T_Unit
void _prim_int_print(int64_t p1) {
    printf("%ld", p1);
}

//:: T_String -> T_Unit
void _prim_string_print(string p1) {
    printf("%s", p1);
}

//   :: T_String
string _prim_read_string() {
    char *buffer = NULL;
    int read;
    size_t len;
    read = getline(&buffer, &len, stdin);
    return buffer;
}

//         :: T_Int64 -> T_Unit
void _prim_usleep(int64_t p1) {
    usleep(p1); // p1 microseconds
}

//          :: T_String -> T_Unit
void _prim_error(string p1) {
    printf("%s\n", p1);
    exit(-1);
}

//   :: T_Int64 -> T_Int64
int64_t _prim_ffi_file_eof(int64_t p1) {
    // TODO: Figure out how to map idris file and C files.
    FILE *f;
    switch(p1) {
        case 1: f = stdin; break;
        default: f = stdin; break;
    }
    return feof(f);
}

// primop pure

// String

//  :: T_String -> T_String -> T_String
string _prim_string_concat(string p1, string p2) {
    int l1 = strlen(p1);
    int l2 = strlen(p2);
    char* p3 = malloc((l1 + l2 + 1) * sizeof(char));
    strcpy(p3, p1);
    strcpy(p3 + l1, p2);
    p3[l1 + l2 + 1] = 0;
    return p3;
}

// :: T_String -> T_String
string _prim_string_reverse(string p1) {
    int l = strlen(p1) - 1;
    int i = 0;
    char* ret = malloc((i + 1) * sizeof(char));
    ret[l + 1] = 0; //Close the string
    while(i <= l) {
        ret[i] = p1[l - i];
        i++;
    }
    return ret;
}

//      :: T_String -> T_String -> T_Bool
int64_t _prim_string_lt(string p1, string p2) {
    int i = 0;
    // Try to find a character which is greater in the first string.
    while(p1[i] && p2[i]) {
        if(p1[i] >= p2[i]) {
            return 0;
        }
        i++;
    }
    // Reached the end.
    if(p1[i] == p2[i]) {
        return 0;
    }
    // First one was shorter, and passed the test above, thus it is less than the second one.
    if(p1[i] == 0) {
        return 1;
    }
    // The second one was shorter, thus the first one is bigger.
    return 0;
}

//      :: T_String -> T_String -> T_Bool
int64_t _prim_string_eq(string p1, string p2) {
    int i = 0;
    // Try to find a different element.
    while(p1[i] && p2[i]) {
        if(p1[i] != p2[i]) {
            return 0;
        }
        i++;
    }
    // If both reached end, both are zero, than they are eq, otherwise not.
    return (p1[i] == p2[i]);
}

//     :: T_String -> T_Int64 -- TODO: Change to Char
int64_t _prim_string_head(string p1) {
    return (int64_t)p1[0];
}

//     :: T_String -> T_String
string _prim_string_tail(string p1) {
    return (p1 + 1);
}

//     :: T_Int64  -> T_String -> T_String
string _prim_string_cons(int64_t p1, string p2) {
    int l = strlen(p2);
    char* ret = malloc((l + 2) * sizeof(char));
    int i = 0;
    ret[i] = (char)p1;
    while(p2[i]) {
        ret[i + 1] = p2[i];
        i++;
    }
    ret[i + 1] = 0;
    return ret;
}

//      :: T_String -> T_Int64
int64_t _prim_string_len(string p1) {
    return strlen(p1);
}

// Conversion

//      :: T_Int64 -> T_String
string _prim_int_str(int64_t p1) {
    char* p2 = malloc(32 * sizeof(char));
    sprintf(p2, "%ld", p1);
    return p2;
}

// :: T_String -> T_Int64
int64_t _prim_str_int(string p1) {
    return (int64_t)atoi(p1);
}

//     :: T_Int64 -> T_Float
float _prim_int_float(int64_t p1) {
    return (float)p1;
}

//  :: T_Float -> T_String
string _prim_float_string(float p1) {
    char* ret = malloc(32 * sizeof(char));
    sprintf(ret, "%f", p1);
    return ret;
}

//     :: T_Char  -> T_Int64
int64_t _prim_char_int(char p1) {
    return (int64_t)p1;
}
