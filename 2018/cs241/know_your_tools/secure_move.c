/**
* Know Your Tools Lab
* CS 241 - Spring 2018
*/

#include "secure_move.h"
#include "utils.h"

const size_t NUM_LINES_HEAD = 5;
const size_t NUM_LINES_TAIL = 5;

const static int NO_FILE_RETURN_CODE = 1;

void print_usage(char *filename) {
    fprintf(stderr, "Usage:\n\n");
    fprintf(stderr, "\t%s: <path> <destination>\n\n", filename);
    fprintf(stderr,
            "%s takes a file located at <path>, copies the file into memory\n",
            filename);
    fprintf(stderr, "then wraps the file to 80 characters maximum and\n");
    fprintf(stderr, "overwrites the file 3 times to prevent recovery.\n");
    fprintf(stderr,
            "Then it writes to the file at <desination> the following\n");
    fprintf(stderr, "\t- The first five lines (after wrapping)\n");
    fprintf(stderr, "\t- A newline\n");
    fprintf(stderr, "\t- The last five lines (after wrapping)\n");
    fprintf(stderr, "\t- A newline\n");
    fprintf(stderr, "\t- The rest of the file\n");
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        print_usage(argv[0]);
        return 1;
    }
    char *file_name = argv[1];
    char *dest = argv[2];
    file *input_file = read_wrap(file_name);
    shred(file_name);
    int path = open(file_name, O_RDWR);
    if(path < 1) {
        perror("");
        exit(NO_FILE_RETURN_CODE);
    }
    close(path);
    int fd = open(dest, O_CREAT | O_WRONLY | O_TRUNC, 0666);
    if (fd < -1) {
        perror("");
        exit(2);
    }
    if (input_file->num_lines > 10) {
        print_head(input_file, fd);
        print_tail(input_file, fd);
    }
    print_entire_file(input_file, fd);
    
    int* temp = malloc(sizeof(int*));
    /** Print the addresses in descending order here **/
    print_argc_addr(&argc);
    print_heap_addr(temp);
    print_string_literal_addr("");
    print_static_variable_addr(&NO_FILE_RETURN_CODE);
    print_main(&main);
    print_strdup_addr(&strdup);
    
    destroy_file(input_file);
    close(fd);
    free(temp);
    temp = NULL;
    return 0;
}
