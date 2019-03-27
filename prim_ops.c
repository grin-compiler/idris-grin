#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <stdbool.h>
#include <string.h>
#include <unistd.h>
#include "prim_ops.h"



struct string* create_string_len(int64_t l) {
    struct string* r = (struct string*)malloc(sizeof(struct string));
    r->data = (char*)calloc(sizeof(char), l * sizeof(char));
    r->length = l;
    return r;
}

struct string* create_string_copy(char* str) {
    struct string* r = (struct string*)malloc(sizeof(struct string));
    int64_t l = strlen(str);
    r->data = (char*)malloc(l * sizeof(char));
    strncpy(r->data, str, l);
    r->length = l;
    return r;
}

char* cstring(struct string* s){
    char* r = malloc((s->length + 1) * sizeof(char));
    memcpy(r, s->data, s->length);
    r[s->length] = 0;
    return r;
}

void _prim_string_print(struct string* p1){
    int64_t i = 1;
    while (i <= p1->length) {
        printf("%c", p1->data[i-1]);
        i++;
    }
}

void _prim_int_print(int64_t p1) {
    printf("%ld", p1);
}

struct string* _prim_read_string() {
    char *buffer = NULL;
    size_t len = 0;
    size_t read;
    read = getline(&buffer, &len, stdin);
    if (read == -1) {
        return create_string_len(0);
    } else {
        struct string* r = create_string_copy(buffer);
        free(buffer);
        return r;
    }
}

void _prim_usleep(int64_t p1) {
    usleep(p1); // p1 microseconds
}

void _prim_error(struct string* p1) {
    _prim_string_print(p1);
    exit(-1);
}

int64_t _prim_ffi_file_eof(int64_t p1) {
    return feof(stdin);
}

struct string* _prim_string_concat(struct string* p1, struct string* p2) {
    struct string* r = create_string_len(p1->length + p2->length);
    memcpy(r->data, p1->data, p1->length);
    memcpy(r->data + p1->length, p2->data, p2-> length);
    return r;
}

struct string* _prim_string_reverse(struct string* s){
    struct string* r = create_string_len(s->length);
    size_t i = 1;
    while(i <= s->length) {
        r->data[i-1] = s->data[s->length - i];
        i++;
    }
    return r;
}

int64_t _prim_string_eq(struct string* p1, struct string* p2){
    if(p1->length != p2->length) {
        return 0;
    }
    size_t i = 1;
    while(i <= p1->length) {
        if(p1->data[i-1] != p2->data[i-1]) {
            return 0;
        }
        i++;
    }
    return 1;
}

int64_t _prim_string_head(struct string* p1) {
    if (p1->length == 0) {
        printf("_prim_string_head\n");
        exit(-1);
    }
    return (int64_t)p1->data[0];
}

int64_t _prim_string_len(struct string* p1) {
    return p1->length;
}

struct string* _prim_string_tail(struct string* p1){
    if(p1->length == 0) {
        printf("_prim_string_tail\n");
        exit(-1);
    }
    struct string* r = create_string_len(p1->length - 1);
    memcpy(r->data, p1->data + 1, r->length);
    return r;
}

struct string* _prim_string_cons(int64_t p1, struct string* p2){
    struct string* r = create_string_len(p2->length + 1);
    r->data[0] = (char)p1;
    memcpy(r->data+1,p2->data,p2->length);
    return r;
}

int64_t _prim_string_lt(struct string* p1, struct string* p2) {
    if(p1->length == 0 && p2->length == 0) {
        return 0;
    }
    size_t i = 0;
    while (i < p1->length && i < p2->length) {
        if(p1->data[i] != p2->data[i]) {
            break;
        }
        i++;
    }
    return p1->data[i] < p2->data[i];
}

struct string* _prim_int_str(int64_t p1){
    char *tmp = (char*)malloc(256 * sizeof(char));
    sprintf(tmp, "%ld", p1);
    struct string* r = create_string_copy(tmp);
    free(tmp);
    return r;
}

int64_t _prim_str_int(struct string* p1) {
    char *tmp = cstring(p1);
    int64_t r = strtoll(tmp, NULL, 10);
    free(tmp);
    return r;
}

float _prim_int_float(int64_t p1) {
    return (float)p1;
}

struct string* _prim_float_string(float p1) {
    char *tmp = (char*)malloc(256 * sizeof(char));
    sprintf(tmp, "%.13g", p1);
    struct string* r = create_string_copy(tmp);
    free(tmp);
    return r;
}

int64_t _prim_char_int(char p1) {
    return (int64_t)p1;
}
