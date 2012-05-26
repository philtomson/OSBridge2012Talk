/* Brute force approach in C */

#include <stdio.h>
#define NUMS  9
#define OPS   NUMS - 1
#define TARGET 2012

typedef enum {
  CAT=-5, PLUS=-4, MINUS=-3, MULT=-2, DIV=-1 
} operation;

typedef enum {
  FALSE, TRUE
} bool;

char op_to_char( operation op){
  char retchar;
  switch(op) {
    case CAT:
      retchar = 'C';
      break;
    case PLUS:
      retchar = 'P';
      break;
    case MINUS:
      retchar = 'M';
      break;
    case MULT:
      retchar = 'T';
      break;
    default:
      retchar = 'C';
  }; 
  return retchar;
};

void ops_to_str( operation* ops, char* out ){
  int i;
  for(i=0; i < (OPS); i++){
    out[i] = op_to_char(ops[i]);
  }; 
  out[i] = '\0';

}

void print_ops( operation* ops ) {
  char ops_str[OPS+1];
  ops_to_str(ops, ops_str);
  printf("ops: %s\n", (ops_str));
}

int do_cat(operation* ops, int* out){
  int ni; 
  int num = 9;
  int multiplier = 1;
  int out_i = 0;
  operation curr_op;
  int accum = 0;
  for(num = 9; num > 0; num--){
    if( (num-2) >= 0) {
      curr_op = ops[num-2];
      if(curr_op == CAT) {
	accum = accum + num*multiplier;
	multiplier *= 10;
      } else {
	if(accum > 0) {
          out[out_i++] = accum + num*multiplier; 
	} else {
	  out[out_i++] = num;
	}
	out[out_i++] = ops[num-2];
	multiplier = 1;
	accum =0;
      }
    } else {
	if(accum) {
          out[out_i++] = accum + num*multiplier; 
	} else {
	  out[out_i++] = num;
	}
    }  
  }
 return out_i;
}

operation succ(operation op) {
  operation retop;
  switch(op){
    case CAT: 
      retop = PLUS;
      break;
    case PLUS:
      retop = MINUS;
      break;
    case MINUS:
      retop = MULT;
      break;
    case MULT: 
      retop = DIV;
      break;
    case DIV:
      retop = CAT;
      break;
    default:
      retop = CAT;
  }
  return retop;
} 

operation max = DIV; 
operation min = CAT;

void incr(operation* ops, int len){
  int i;
  bool carry = TRUE;
  for(i=0; i<len; i++){
    operation op = ops[i];
    if(carry == 1) {
      if(op == max) {
	carry = TRUE; 
      } else {
	carry = FALSE;
      }
    ops[i] = (succ(op));
    }
  } 
}

bool max_val(operation* ops, int len){
  int i;
  bool retval = TRUE;
  for(i=0; i<len; i++){
    if(ops[i] != max) {
      return FALSE;
    } 
  }
  return retval;
}

void find_all(){
  operation ops[] = { CAT, CAT, CAT, CAT, CAT, CAT, CAT, CAT } ;
  while( ! max_val(ops, 8)){
    if(do_eval(ops) == TARGET){
      printf("We have a winner: ");
      print_ops(ops);
    }
    incr(ops, 8);
  }
}

//returns the length of the combined ops
int do_eval(operation* ops){
  int out[OPS + NUMS];
  int len = do_cat( ops, out);
  int accum  = 0;
  int prevop = PLUS; /* + 0 is identity */
  int i;
  for(i = (len-1); i > -1; i--){
    int op_or_num = out[i];
    if(op_or_num < 0 ){
      /* op */
      prevop = op_or_num;
    } else {
      /* num */
      switch(prevop) {
	case PLUS: 
	  accum += op_or_num;
	  break;
	case MINUS:
	  accum -= op_or_num;
	  break;
	case MULT: 
	  accum *= op_or_num;
	  break;
      };
    }
  }
  return accum;
}

int main() {
  /*
  operation ops[] = { CAT, PLUS, CAT, MINUS, CAT, MULT, CAT, CAT } ;
  char ops_str[OPS+1];
  print_ops(ops) ;
  ops_to_str(ops, ops_str);
  printf("\nops str is: %s\n",ops_str);
  int out[OPS + NUMS];
  int len = do_cat( ops, out);
  printf("%d items in the resulting array\n", len);
  int i;
  for(i = (len-1); i > -1; i--){
    int op_or_num = out[i];
    if(op_or_num < 0 ){
      // op 
      printf("%c ", (op_to_char(op_or_num)));
    } else {
      // num 
      printf("%d ", op_or_num);
    }
  }
  printf("\n");

  do_eval(ops);
  */
  operation ops1[] = { CAT, CAT, CAT, CAT, CAT, CAT, CAT, CAT } ;
  /*
  int num_counts = 20;
  for(i=0; i< num_counts; i++){
    incr(ops1, 8);
    print_ops(ops1);
  }
  */

  find_all();
  


};
