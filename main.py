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
        if(self.value == '+'):
            return self.children[0].evaluate()
        elif(self.value == '-'):
            return -self.children[0].evaluate()
    
class BinOp(Node):
    def __init__(self, value: int | str, children: list[Node]):
        super().__init__(value, children)

    def evaluate(self):
        if(self.value == '+'):
            return self.children[0].evaluate() + self.children[1].evaluate()
        elif(self.value == '-'):
            return self.children[0].evaluate() - self.children[1].evaluate()
        elif(self.value == '*'):
            return self.children[0].evaluate() * self.children[1].evaluate()  
        elif(self.value == '/'):
            return self.children[0].evaluate() / self.children[1].evaluate()

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
        self.lex : Lexer

    def parseFactor(self):
        if(self.lex.next.kind == 'INT'):
            return self.lex.next.value
        else:
            if(self.lex.next.kind == 'PLUS'):
                self.lex.selectNext()
                return self.parseFactor()
            elif(self.lex.next.kind == 'MINUS'):
                self.lex.selectNext()
                return -self.parseFactor()
            elif(self.lex.next.kind == 'OPEN_PAR'):
                self.lex.selectNext()
                resultado = self.parseExpression()
                if(self.lex.next.kind != 'CLOSE_PAR'):
                    raise Exception('Syntax Error: CLOSE_PAR Token not found.')
                else:
                    return resultado
            else:
                raise Exception('Syntax Error: Number not found after operation signal.')


    def parseTerm(self):
        resultado = self.parseFactor()
        self.lex.selectNext()
        if(self.lex.next.kind == 'INT'):
            raise Exception('Syntax Error: There must be an operation between two numbers.')
        while((self.lex.next.kind == 'MULT') or (self.lex.next.kind == 'DIV')):
            operation = self.lex.next.kind
            self.lex.selectNext()
            if(self.lex.next.kind == 'MULT' or self.lex.next.kind == 'DIV'):
                raise Exception('Syntax Error: Invalid operation.')
            else:
                if(operation == 'MULT'):
                    resultado *= self.parseFactor()
                elif(operation == 'DIV'):
                    resultado //= self.parseFactor()
                self.lex.selectNext()
        return resultado

    def parseExpression(self):
        resultado = self.parseTerm()
        if(self.lex.next.kind == 'INT'):
            raise Exception('Syntax Error: There must be an operation between two numbers.')
        while((self.lex.next.kind == 'PLUS' or self.lex.next.kind == 'MINUS')):
            operation = self.lex.next.kind
            self.lex.selectNext()
            if(operation == 'MINUS'):
                resultado -= self.parseTerm()
            elif(operation == 'PLUS'):
                resultado += self.parseTerm()
        return resultado

    def run(self, code: str):
        self.lex = Lexer(code)
        self.lex.selectNext()
        if(self.lex.next.kind == 'EOF'):
            raise Exception('Syntax Error: Empty expression.')
        resultado = self.parseExpression()
        if(self.lex.next.kind == 'CLOSE_PAR'):
            raise Exception('Syntax Error: CLOSE_PAR Token found without previous OPEN_PAR Token.')
        print(resultado)

if __name__ == '__main__':
    parser = Parser()
    if(len(sys.argv) == 1):
        raise Exception('Syntax Error: Empty expression.')
    else:
        parser.run(sys.argv[1])