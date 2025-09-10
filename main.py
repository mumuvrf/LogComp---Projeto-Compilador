import sys
from abc import ABC, abstractmethod

class Node(ABC):
    def __init__(self, value: int | str, children):
        self.value = value
        self.children = children

    @abstractmethod
    def evaluate(self):
        pass

class IntVal(Node):
    def __init__(self, value: int | str):
        super().__init__(value, [])

    def evaluate(self):
        return self.value
    
class UnOp(Node):
    def __init__(self, value: int | str, child: Node):
        super().__init__(value, [child])

    def evaluate(self):
        if(self.value == 'PLUS'):
            return self.children[0].evaluate()
        elif(self.value == 'MINUS'):
            return -self.children[0].evaluate()
    
class BinOp(Node):
    def __init__(self, value: int | str, left: Node, right: Node):
        super().__init__(value, [left, right])

    def evaluate(self):
        if(self.value == 'PLUS'):
            return self.children[0].evaluate() + self.children[1].evaluate()
        elif(self.value == 'MINUS'):
            return self.children[0].evaluate() - self.children[1].evaluate()
        elif(self.value == 'MULT'):
            return self.children[0].evaluate() * self.children[1].evaluate()  
        elif(self.value == 'DIV'):
            return self.children[0].evaluate() // self.children[1].evaluate()

class Token:
    def __init__(self, kind: str, value: int | str):
        self.kind = kind
        self.value = value

class Lexer:
    def __init__(self, source: str):
        self.source = source
        self.position = 0
        self.next = -1

    def selectNext(self):
        if(self.position == len(self.source)):
            self.next = Token('EOF', '')
            return
        char = self.source[self.position]
        while char == ' ' and self.position < len(self.source):
            self.position += 1
            if(self.position < len(self.source)):
                char = self.source[self.position]
            else:
                self.next = Token('EOF', '')
                return
        if(char.isdigit()):
            number = ""
            while(char.isdigit()):
                number += char
                self.position += 1
                if(self.position == len(self.source)):
                    break
                else:
                    char = self.source[self.position]
            self.next = Token('INT', int(number))
        else:
            if(char == '+'):
                self.next = Token('PLUS', '+')
            elif(char == '-'):
                self.next = Token('MINUS', '-')
            elif(char == '*'):
                self.next = Token('MULT', '*')
            elif(char == '/'):
                self.next = Token('DIV', '/')
            elif(char == '('):
                self.next = Token('OPEN_PAR', '(')
            elif(char == ')'):
                self.next = Token('CLOSE_PAR', ')')
            else:
                raise Exception(f"Invalid character found at position {self.position}.")
            self.position += 1

class Parser:
    def __init__(self):
        self.lex: Lexer

    def parseFactor(self):
        if self.lex.next.kind == "INT":
            node = IntVal(self.lex.next.value)
            self.lex.selectNext()
            return node
        elif self.lex.next.kind in ("PLUS", "MINUS"):
            operation = self.lex.next.kind
            self.lex.selectNext()
            return UnOp(operation, self.parseFactor())
        elif self.lex.next.kind == "OPEN_PAR":
            self.lex.selectNext()
            node = self.parseExpression()
            if self.lex.next.kind != "CLOSE_PAR":
                raise Exception("Syntax Error: ')' expected")
            self.lex.selectNext()
            return node
        else:
            raise Exception("Syntax Error: invalid factor")

    def parseTerm(self):
        node = self.parseFactor()
        if(self.lex.next.kind == 'INT'):
            raise Exception('Syntax Error: There must be an operation between two numbers.')
        while self.lex.next.kind in ("MULT", "DIV"):
            operation = self.lex.next.kind
            self.lex.selectNext()
            right = self.parseFactor()
            node = BinOp(operation, node, right)
        return node

    def parseExpression(self):
        node = self.parseTerm()
        while self.lex.next.kind in ("PLUS", "MINUS"):
            operation = self.lex.next.kind
            self.lex.selectNext()
            right = self.parseTerm()
            node = BinOp(operation, node, right)
        return node

    def run(self, code: str):
        self.lex = Lexer(code)
        self.lex.selectNext()
        if self.lex.next.kind == "EOF":
            raise Exception("Syntax Error: empty expression")
        root = self.parseExpression()
        if self.lex.next.kind != "EOF":
            raise Exception("Syntax Error: unexpected token after expression")
        print(root.evaluate())

if __name__ == '__main__':
    parser = Parser()
    if(len(sys.argv) == 1):
        raise Exception('Syntax Error: Empty expression.')
    else:
        parser.run(sys.argv[1])