#include <stdio.h>
#include <stdlib.h>

/* Grammar:
 *  S := E $
 *  E := F E1
 *     | ( E ) E1
 *  E1 := [ + - ] F E1
 *      | eps
 *  F := int F1
 *     | ( E )
 *  F1 := [ * / % ] int F1
 *      | eps
 *
 */

#define EOI     0 /* $     */
#define P_OPEN  1 /* (     */
#define P_CLOSE 2 /* )     */
#define OP_MUL  3 /* * / % */
#define OP_ADD  4 /* + -   */
#define INT     5 /* int   */

typedef union {
  int int_val;
  char char_val;
  char *op_val;
} tok_val;

tok_val la_val;
int la_tok;

char
  op_add[] = "add",
  op_sub[] = "sub",
  mul[] = "mul",
  div[] = "div",
  mod[] = "mod";

int look_ahead(tok_val *buffer){
  char b;
  scanf("%[ \n\t]*", &b);
  int res = scanf("%1[-+*/%()]", buffer -> char_val);
  if(res == EOF){
    return EOI;
  } else if(res == 1) {
    char b = buffer -> char_val;
    switch(b){
      case '+':
        buffer -> op_val = add;
        return OP_ADD;
      case '-':
        buffer -> op_val = sub;
        return OP_ADD;
      case '*':
        buffer -> op_val = mul;
        return OP_MUL;
      case '/':
        buffer -> op_val = div;
        return OP_MUL;
      case '%':
        buffer -> op_val = mod;
        return OP_MUL;
      case '(': return P_OPEN;
      case ')': return P_CLOSE;
      default: return -1;
    }
  } else if(res == 0){
    res = scanf("%d", buffer -> int_val);
    if(res != 1) return -1;
    return INT;
  } else {
    return -1;
  }
}

void shift(){
  if(la_tok == EOI) return;
  la_tok = look_ahead(&la_val);
}

int read(int expected){
  if(la_tok == expected){
    shift();
    return 1;
  }
  return 0;
}


typedef struct EXPR {
  int is_node;
  int value;
  char* op;
  struct EXPR left, right;
} expr;

expr* make_expr_leaf(int val){
  expr* tmp = malloc(sizeof(expr));
  if(!tmp) return 0;
  tmp -> value = val;
  tmp -> is_node = 0;
  return tmp;
}

expr* make_expr_node(char* op, expr* left, expr* right){
  expr* tmp = malloc(sizeof(expr));
  if(!tmp) return 0;
  tmp -> is_node = 1;
  tmp -> left = left;
  tmp -> right = right;
  return tmp;
}

void append_leftmost(expr* e, expr* e1){
  if(e1 -> left == 0){
    e1 -> left = e;
  } else {
    append_leftmost(e, e1 -> left);
  }
}


expr* S(){
  expr* e = E();
  if(!e) return 0;
  if(!read(EOI)) return 0;
  return e;
}


expr* E(){
  expr* e;
  switch(la_tok){
    case P_OPEN:
      if(!read(P_OPEN)) return 0;
      e = E();
      if(!read(P_CLOSE)) return 0;
      break;
    case INT:
      e = F();
      break;
    default:
      return 0;
  }
  expr* e2 = E1();
  if(e2 < 0){
    return e;
  } else {
    append_leftmost(e,e2);
    return e2;
  }
}

expr* E1(){
  switch(la_tok){
    case EOI:
      return -1;
    case P_CLOSE:
      return -1;
    case OP_ADD:
      expr *e = make_expr_node(la_val.op_val, 0, 0);
      read(OP_ADD);
      expr *e1 = E1();
      append_leftmost(e,e1);
      return e1;
    case OP_MUL:
      return -1;
    default:
      return 0;
    }
}

expr* F(){
  switch(la_tok){
    case P_OPEN:
      read(P_OPEN);
      expr* e = E();
      read(P_CLOSE);
      return e;
    case INT:
      expr* e = make_expr_leaf(la_val.int_val);
      expr* e1 = F1;
      if(e1 < 0) return e;
      append_leftmost(e,e1);
      return e1;
    default:
      return 0;
  }
}

expr* F1(){
  switch(la_tok){
    case EOI:
      return -1;
    case OP_ADD:
      return -1;
    case OP_MUL:
      expr *e = make_expr_node(la_val.op_val, 0, 0);
      read(OP_MUL);
      expr *e1 = make_expr_leaf(la_val.int_val);
      e -> right = e1;
      read(INT);
      expr *e2 = F1();
      if(e2 < 0){
        return e;
      } else {
        append_leftmost(e,e2);
        return e2;
      }
    default:
      return 0;
  }
}

int main(){
  int buffer;
  int r;
  shift();
  S();
  while((r = look_ahead(&buffer)) > EOI){
    switch(r){
      case P_OPEN: case P_CLOSE: case OP_MUL: case OP_ADD:
        printf("C> %d %c\n", r, (char) buffer);
        break;
      case INT:
        printf("I> %d %d\n", r, buffer);
        break;
    }
  }
}
