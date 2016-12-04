#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdbool.h>

void check_mem(int *buffer){
    // check to make sure memory was successfully (re)allocated
    if (buffer == NULL){
        printf("Oops! Ran out of memory.");
        free(buffer);
        exit(1);
    }
}

typedef struct instruction {
    int num_ops;
    int *ops;
    int next_state;
} instruction;

typedef struct state {
    int num_symbols;
    int *symbols;
    instruction *instructions;
    instruction *any_instruction;
    instruction *else_instruction;
} state;


typedef struct complete_config {
    int *right_tape;
    int *left_tape;
    int right_tape_size;
    int left_tape_size;
    int tape_head;
    unsigned int current_state;
} complete_config;

typedef struct machine {
    int num_states;
    state *states;
} machine;

void move_head(complete_config *cc, int move_distance){
    cc->tape_head = cc->tape_head + move_distance;

    bool right_bound_check = (cc->tape_head > (cc->right_tape_size - 1));
    bool left_bound_check = (cc->tape_head < cc->left_tape_size);
    if (right_bound_check | left_bound_check){
        // ran out of memory, need to reallocate
        int *tape = right_bound_check ? cc->right_tape : cc->left_tape;
        int *tape_size = right_bound_check ? &(cc->right_tape_size) : &(cc->left_tape_size);
        while ((*tape_size) < (abs(cc->tape_head) + 1)){
            tape = realloc(tape, 2*(*tape_size)); // double the tape length
            check_mem(tape);
            memset(tape + (*tape_size), 0, (*tape_size)*sizeof(int));
            (*tape_size) = 2*(*tape_size);
        }
    }
}

void write_symbol(complete_config *cc, int symbol){
    if (cc->tape_head > -1){
        // head is on the right side
        cc->right_tape[cc->tape_head] = symbol;
    }
    else {
        // head is on the left side
        cc->left_tape[-cc->tape_head - 1] = symbol;
    }
}

int read_symbol(complete_config *cc){
    if (cc->tape_head > -1){
        // head is on the right side
        return cc->right_tape[cc->tape_head];
    }
    else {
        // head is on the left side
        return cc->left_tape[-cc->tape_head - 1];
    }
}

void do_move(complete_config *cc, instruction *instr){
    for (int i = 0; i < instr->num_ops; i = i + 2){
        // ops goes [move, write symbol, move, write symbol,... ]
        if (instr->ops[i] != 0){
            // need to move the head
            move_head(cc, instr->ops[i]);
        }
        if (instr->ops[i+1] > -1){
            // symbol of less than zero means skip, otherwise write the symbol on the tape
            //cc->tape[cc->tape_head] = instr->ops[i+1];
            write_symbol(cc, instr->ops[i+1]);
        }
    }
}

void print_tape(complete_config *cc){
    int cell;
    for (cell = cc->left_tape_size - 1; cell >= 0; cell++){
        printf("%d ", cc->left_tape[cell]);
    }
    for (cell = 0; cell < cc->right_tape_size; cell++){
        printf("%d ", cc->right_tape[cell]);
    }
    printf("\n");
}

void wait_for_input(){
    getchar();
}

void simulate_turing(machine *m, int print_frequency, bool pause){
    int s; // keeps track of which symbol we're looking for given a configuration
    bool symbol_match;
    bool all_done = false;
    int num_moves = 0;
    complete_config cc;
    cc.right_tape_size = 10;
    cc.left_tape_size = 10;
    cc.tape_head = 0;
    cc.current_state = 0; // 0 is the starting state

    // allocate and initialize the right side of the tape
    cc.right_tape = malloc(cc.right_tape_size*sizeof(int));
    check_mem(cc.right_tape);
    memset(cc.right_tape, 0, cc.right_tape_size*sizeof(int));

    // same thing for left side
    cc.left_tape = malloc(cc.left_tape_size*sizeof(int));
    check_mem(cc.left_tape);
    memset(cc.left_tape, 0, cc.left_tape_size*sizeof(int));

    int current_symbol;

    while (!all_done){
        current_symbol = read_symbol(&cc);
        if ((m->states[cc.current_state].any_instruction != NULL) & (current_symbol != 0)){
            // any instruction is defined and symbol under head is 0 (blank)
            do_move(&cc, m->states[cc.current_state].any_instruction);
            cc.current_state = m->states[cc.current_state].any_instruction->next_state;
        }
        else {
            // first look if we have a symbol for which an op is defined
            symbol_match = false;
            for (s = 0; s < (m->states[cc.current_state].num_symbols); s++){
                if (m->states[cc.current_state].symbols[s] == current_symbol){
                    symbol_match = true;
                    break;
                }
            }
            if (symbol_match){
                // we have a defined op for the read symbol, do the op!
                do_move(&cc, &(m->states[cc.current_state].instructions[s]));
                cc.current_state = m->states[cc.current_state].instructions[s].next_state;
            }
            else if (m->states[cc.current_state].else_instruction != NULL){
                // see if there is an else op defined
                do_move(&cc, m->states[cc.current_state].else_instruction);
                cc.current_state = m->states[cc.current_state].else_instruction->next_state;
            }
            else {
                // read a symbol from the tape that has no defined op
                // for the current state... so just be done
                all_done = true;
            }
        }
        num_moves++;
        if (num_moves % print_frequency == 0){
            print_tape(&cc);
            if (pause){
                wait_for_input();
            }
        }
    }

}

//machine parse_machine(char *machine_str){


int main(){
    machine m = {0};
    instruction x = {0};
    instruction y = {0}
    instruction 
    
    //simulate_turing(program, 1, true);
    return 0;
}

/*
int main(int argc, char *argv[]){
    char c[1000];
    FILE *fptr;

    if ((fptr = fopen("program.txt", "r")) == NULL)
    {
        printf("Error! opening file");
        // Program exits if file pointer returns NULL.
        exit(1);         
    }

    // fd = fileno(f); //if you have a stream (e.g. from fopen), not a file descriptor.
    struct stat buf;
    fstat(fd, &buf);
    int size = buf.st_size;

    // reads text until newline 
    fscanf(fptr,"%[^\n]", c);

    printf("Data from the file:\n%s", c);
    fclose(fptr);
    
    return 0;
}*/
