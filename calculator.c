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
  int i = 0;
  int t_i = 0;
  int len = strlen(input);
  while (i < len) {
    char c = input[i];

    if (c == '(') {
      tokens[t_i++].type = ParOpen;
    } else if (c == ')') {
      tokens[t_i++].type = ParClose;
    } else if (strchr("*/-+", c)) {
      tokens[t_i++] = (Token){.type = Op, .op = c};
    } else if (isdigit(c)) {
      char *ptr = &input[i];
      float num = strtof(&input[i], &ptr);
      i += ptr - &input[i];
      tokens[t_i++] = (Token){.type = Num, .num = num};
      continue;
    }

    i++;
  }

  tokens_len = t_i;
  tok_i = 0;
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
    advance();
    return left;
  }

  exit(1);
}

float term() {
  float left = factor();

  Token tok = get_curr_tok();
  while (tok.type == Op && strchr("*/", tok.op)) {
    advance();
    float right = term();
    if (tok.op == '*') {
      left *= right;
    } else {
      left /= right;
    }
    tok = get_curr_tok();
  }

  return left;
}

float expr() {
  float left = term();

  Token tok = get_curr_tok();
  while (tok.type == Op && strchr("+-", tok.op)) {
    advance();
    float right = term();
    if (tok.op == '+') {
      left += right;
    } else {
      left -= right;
    }
    tok = get_curr_tok();
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
