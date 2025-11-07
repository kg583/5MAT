#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <memory.h>

#define HASH (end - r)
#define V (*r++)

#define SIZE_GUESS 4096

typedef struct {
    char *data;
    size_t size;
    size_t capacity;
} OutputBuffer;

int fourmat(OutputBuffer *w, char *r);

static inline void buffer_init(OutputBuffer *buf, size_t initial_capacity) {
    buf->data = malloc(initial_capacity);
    if (!buf->data) {
        fprintf(stderr, "Failed to allocate output buffer\n");
        exit(1);
    }
    buf->size = 0;
    buf->capacity = initial_capacity;
}

static void buffer_ensure_capacity(OutputBuffer *buf, size_t extra) {
    if (buf->size + extra > buf->capacity) {
        size_t new_capacity = buf->capacity * 2;
        if (new_capacity < buf->size + extra)
            new_capacity = buf->size + extra;
        char *tmp = realloc(buf->data, new_capacity);
        if (!tmp) {
            fprintf(stderr, "Failed to resize output buffer\n");
            free(buf->data);
            exit(1);
        }
        buf->data = tmp;
        buf->capacity = new_capacity;
    }
}

// write a single character to the tape
static inline void buffer_append_char_raw(OutputBuffer *restrict buf, char c) {
    buf->data[buf->size++] = c;
}

static inline void buffer_append_char(OutputBuffer *restrict buf, char c) {
    buffer_ensure_capacity(buf, 1);
    buf->data[buf->size++] = c;
}

// write a string to the tape
static inline void buffer_append(OutputBuffer *restrict buf,
                                 const char *restrict s, size_t len) {
    buffer_ensure_capacity(buf, len);
    memcpy(buf->data + buf->size, s, len);
    buf->size += len;
}

static inline void buffer_append_repeated(OutputBuffer *restrict buf, char c, size_t count) {
    buffer_ensure_capacity(buf, count);
    memset(buf->data + buf->size, c, count);
    buf->size += count;
}

static inline void buffer_free(OutputBuffer *buf) {
    free(buf->data);
    buf->data = NULL;
    buf->size = buf->capacity = 0;
}

char *read_stdin(size_t *out_size) {
    size_t capacity = SIZE_GUESS;
    size_t size = 0;
    char *data = malloc(capacity);
    if (!data) return NULL;

    while (1) {
        if (size == capacity) {
            capacity *= 2;
            char *tmp = realloc(data, capacity);
            if (!tmp) {
                free(data);
                return NULL;
            }
            data = tmp;
        }

        size_t n = fread(data + size, 1, capacity - size, stdin);
        size += n;

        if (n == 0) {
            if (feof(stdin)) break;
            if (ferror(stdin)) {
                free(data);
                return NULL;
            }
        }
    }

    *out_size = size;
    return data;
}

const char *charname(char c) {
    // length-prefixed strings
    switch (c) {
        // Standard characters
        case '\n': return "\x07""Newline";
        case ' ':  return "\x05""Space";

        // Semi-standard characters
        case '\t': return "\x03""Tab";
        case '\f': return "\x04""Page";
        case 0x7f: return "\x06""Rubout";
        case '\r': return "\x06""Return";
        case '\b': return "\x09""Backspace";

        // Other CLISP names
        case '\x01': return "\x03""Soh";
        case '\x02': return "\x03""Stx";
        case '\x03': return "\x03""Etx";
        case '\x04': return "\x03""Eot";
        case '\x05': return "\x03""Enq";
        case '\x06': return "\x03""Ack";
        case '\x07': return "\x04""Bell";
        case '\x0b': return "\x02""Vt";
        case '\x0e': return "\x02""So";
        case '\x0f': return "\x02""Si";
        case '\x10': return "\x03""Dle";
        case '\x11': return "\x03""Dc1";
        case '\x12': return "\x03""Dc2";
        case '\x13': return "\x03""Dc3";
        case '\x14': return "\x03""Dc4";
        case '\x15': return "\x03""Nak";
        case '\x16': return "\x03""Syn";
        case '\x17': return "\x03""Etb";
        case '\x18': return "\x03""Can";
        case '\x19': return "\x02""Em";
        case '\x1a': return "\x03""Sub";
        case '\x1b': return "\x06""Escape";
        case '\x1c': return "\x02""Fs";
        case '\x1d': return "\x02""Gs";
        case '\x1e': return "\x02""Rs";
        case '\x1f': return "\x02""Us";

        default: return NULL;
    }
}

char *start;
char *end;

int fivemat() {
    char *r = strdup("");
    start = r;
    end = r;

    OutputBuffer w;
    buffer_init(&w, SIZE_GUESS);

    while (1) {
        int ret_val = fourmat(&w, r);
        free(r);

        if (ret_val) {
            buffer_free(&w);
            return ret_val;
        }

        r = malloc(w.size + 1);
        buffer_append_char(&w, '\0');
        strcpy(r, w.data);

        start = r;
        end = r + w.size - 1;

        char *print_start = w.data;
        for (size_t i = w.size; i > 0; i--) {
            if (w.data[i - 1] == '\f') {
                print_start = &w.data[i];
                break;
            }
        }

        //printf("%s", print_start);
        printf("%s", w.data);

        buffer_free(&w);
        buffer_init(&w, w.size);
    }
}

int main() {
    int code = fivemat();

    exit(code);
}

