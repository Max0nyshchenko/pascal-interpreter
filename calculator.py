from typing import Union

INTEGER, PLUS, MINUS, MUL, DIV, LPAREN, RPAREN, EOF = (
    "INTEGER",
    "PLUS",
    "MINUS",
    "MUL",
    "DIV",
    "(",
    ")",
    "EOF",
)


class Token(object):
    def __init__(self, type, value):
        # type: (str, Union[float, str, None]) -> None
        self.type = type
        self.value = value

    def __str__(self):
        return "Token({type}, {value})".format(type=self.type, value=repr(self.value))

    def __repr__(self):
        return self.__str__()


class AST(object):
    pass


class Num(AST):
    def __init__(self, token):
        # type: (Token) -> None
        self.token = token
        self.value = token.value


class UnaryOp(AST):
    def __init__(self, op, expr):
        self.token = self.op = op
        self.expr = expr


class BinOp(AST):
    def __init__(self, left, op, right):
        # type: (Union[UnaryOp, BinOp, Num], Token, Union[UnaryOp, BinOp, Num]) -> None
        self.left = left
        self.token = self.op = op
        self.right = right


class Lexer(object):
    def __init__(self, text):
        # type: (str) -> None
        self.text = text
        self.pos = 0
        self.current_char = self.text[self.pos]

    def error(self):
        raise Exception("Invalid character")

    def advance(self):
        self.pos += 1
        if self.pos > len(self.text) - 1:
            self.current_char = None
        else:
            self.current_char = self.text[self.pos]

    def skip_whitespace(self):
        while self.current_char is not None and self.current_char.isspace():
            self.advance()

    def integer(self):
        result = ""
        while self.current_char is not None and self.current_char.isdigit():
            result += self.current_char
            self.advance()
        return int(result)

    def get_next_token(self):
        while self.current_char is not None:
            if self.current_char.isspace():
                self.skip_whitespace()
                continue

            if self.current_char.isdigit():
                return Token(INTEGER, self.integer())

            if self.current_char == "(":
                self.advance()
                return Token(LPAREN, "(")

            if self.current_char == ")":
                self.advance()
                return Token(RPAREN, ")")

            if self.current_char == "*":
                self.advance()
                return Token(MUL, "*")

            if self.current_char == "/":
                self.advance()
                return Token(DIV, "/")

            if self.current_char == "+":
                self.advance()
                return Token(PLUS, "+")

            if self.current_char == "-":
                self.advance()
                return Token(MINUS, "-")

            self.error()

        return Token(EOF, None)


class Parser(object):
    def __init__(self, lexer):
        # type: (Lexer) -> None
        self.lexer = lexer
        self.current_token = self.lexer.get_next_token()

    def error(self):
        raise Exception("Invalid syntax")

    def eat(self, token_type):
        # type: (str) -> None
        if self.current_token.type == token_type:
            self.current_token = self.lexer.get_next_token()
        else:
            self.error()

    def factor(self):
        token = self.current_token

        if token.type in (PLUS, MINUS):
            self.eat(token.type)
            node = UnaryOp(token, self.factor())
            return node
        elif token.type == LPAREN:
            self.eat(LPAREN)
            result = self.expr()
            self.eat(RPAREN)
            return result

        self.eat(INTEGER)
        return Num(token)

    def term(self):
        # type: () -> Union[UnaryOp, BinOp, Num]
        left = self.factor()

        while self.current_token.type in (MUL, DIV):
            tok = self.current_token
            self.eat(self.current_token.type)
            left = BinOp(left=left, op=tok, right=self.factor())

        return left

    def expr(self):
        # type: () -> Union[UnaryOp, BinOp, Num]
        left = self.term()

        while self.current_token.type in (PLUS, MINUS):
            tok = self.current_token
            self.eat(self.current_token.type)
            left = BinOp(left=left, op=tok, right=self.term())

        return left

    def parse(self):
        return self.expr()


class NodeVisitor(object):
    def visit(self, node):
        # type: (Union[UnaryOp, BinOp, Num]) -> Union[float, None]
        method_name = "visit_" + type(node).__name__
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        # type: (Union[UnaryOp, BinOp, Num]) -> None
        raise Exception("No visit_{} method".format(type(node).__name__))


class Interpreter(NodeVisitor):
    def __init__(self, parser):
        # type: (Parser) -> None
        self.parser = parser

    def visit_BinOp(self, node):
        # type: (BinOp) -> Union[float, None]
        ops = {
            PLUS: lambda x, y: x + y,
            MINUS: lambda x, y: x - y,
            MUL: lambda x, y: x * y,
            DIV: lambda x, y: x / y,
        }
        left = self.visit(node.left)
        right = self.visit(node.right)
        if not left or not right:
            raise Exception("BinOp: Invalid value")

        return ops[node.op.type](left, right)

    def visit_UnaryOp(self, node):
        res = self.visit(node.expr)
        if not res:
            raise Exception("UnaryOp: Invalid value")

        return +res if node.op.type == PLUS else -res

    def visit_Num(self, node):
        # type: (Num) -> Union[float,None]
        return float(node.value) if node.value else None

    def interpret(self):
        tree = self.parser.parse()
        return self.visit(tree)


def main():
    while True:
        try:
            text = raw_input("calc> ")
        except EOFError:
            break
        if not text:
            continue
        lexer = Lexer(text)
        parser = Parser(lexer)
        interpreter = Interpreter(parser)
        result = interpreter.interpret()
        print(result)


if __name__ == "__main__":
    main()
