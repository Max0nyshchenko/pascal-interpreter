#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum { None, Op, Num, ParOpen, ParClose } TokenType;

typedef struct {
  TokenType type;
  union {
    char op;
    float num;
  };
} Token;

float expr();

char input[100];
Token tokens[100];
int tokens_len = 0;
int tok_i = 0;

void tokenize() {
  memset(tokens, 0, sizeof(tokens));
  tokens_len = 0;
  tok_i = 0;
  int n;
  for (char *p = input; *p; p += n) {
    if (isspace(*p)) {
      n = 1;
      continue;
    }

    if (isdigit(*p)) {
      tokens[tokens_len++] = (Token){.type = Num, .num = strtof(p, &p)};
      n = 0;
    } else {
      TokenType type = (*p == '(') ? ParOpen : (*p == ')') ? ParClose : Op;
      tokens[tokens_len++] = (Token){type, .op = *p};
      n = 1;
    }
  }
}

Token get_curr_tok() { return tokens[tok_i]; }

Token advance() { return tokens[tok_i++]; }

float factor() {
  Token tok = get_curr_tok();
  if (tok.type == Num)
    return advance().num;

  if (tok.type == ParOpen) {
    advance();
    float left = expr();
    return advance(), left;
  }

  exit(1);
}

float term() {
  float left = factor();
  Token tok;

  while (tok = get_curr_tok(), tok.type == Op && strchr("*/", tok.op)) {
    advance();
    float right = term();
    left = tok.op == '*' ? left * right : left / right;
  }

  return left;
}

float expr() {
  float left = term();
  Token tok;

  while (tok = get_curr_tok(), tok.type == Op && strchr("+-", tok.op)) {
    advance();
    float right = term();
    left = tok.op == '+' ? left + right : left - right;
  }

  return left;
}

void print_token(Token t) {
  if (t.type == Op) {
    printf("Op %c", t.op);
  } else if (t.type == Num) {
    printf("Num %f", t.num);
  } else {
    printf("%s", t.type == ParOpen ? "(" : ")");
  }
}

void print_tokens() {
  int i = 0;
  printf("\nTokens:[");
  while (i < tokens_len) {
    printf("\n");
    print_token(tokens[i]);
    printf(",");
    i++;
  }
  printf("\n]");
}

int main() {

  while (1) {
    printf("\ncalc> ");
    fgets(input, sizeof(input), stdin);
    tokenize();
    float res = expr();
    printf("\nresult is: %f", res);
  }

  return 0;
}
