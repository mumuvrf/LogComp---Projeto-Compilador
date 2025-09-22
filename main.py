import sys
import re
from abc import ABC, abstractmethod

class Prepro:
    def filter(self, code):
        filtered_code = re.sub(r"//.*", "", code)
        return filtered_code

class SymbolTable:
    def __init__(self):
        self.table = {}

    def getTableValue(self, name: str):
        if name in self.table.keys():
            return self.table[name].value
        else:
            raise Exception('Syntax error: Invalid variable name.')
    
    def setTableValue(self, name: str, value: int):
        self.table[name] = Variable(value)
        pass

class Variable:
    def __init__(self, value: int):
        self.value = value

class Node(ABC):
    def __init__(self, value: int | str, children):
        self.value = value
        self.children = children

    @abstractmethod
    def evaluate(self, st: SymbolTable):
        pass

class IntVal(Node):
    def __init__(self, value: int | str):
        super().__init__(value, [])

    def evaluate(self, st: SymbolTable):
        return self.value
    
class UnOp(Node):
    def __init__(self, value: int | str, child: Node):
        super().__init__(value, [child])

    def evaluate(self, st: SymbolTable):
        if(self.value == 'PLUS'):
            return self.children[0].evaluate(st)
        elif(self.value == 'MINUS'):
            return -self.children[0].evaluate(st)
    
class BinOp(Node):
    def __init__(self, value: int | str, left: Node, right: Node):
        super().__init__(value, [left, right])

    def evaluate(self, st: SymbolTable):
        if(self.value == 'PLUS'):
            return self.children[0].evaluate(st) + self.children[1].evaluate(st)
        elif(self.value == 'MINUS'):
            return self.children[0].evaluate(st) - self.children[1].evaluate(st)
        elif(self.value == 'MULT'):
            return self.children[0].evaluate(st) * self.children[1].evaluate(st)  
        elif(self.value == 'DIV'):
            return self.children[0].evaluate(st) // self.children[1].evaluate(st)
        
class Identifier(Node):
    def __init__(self, value: int | str):
        super().__init__(value, [])

    def evaluate(self, st: SymbolTable):
        return st.getTableValue(self.value)
    
class Print(Node):
    def __init__(self, value: int | str, child: Node):
        super().__init__(value, [child])

    def evaluate(self, st: SymbolTable):
        print(self.children[0].evaluate(st))
        pass

class Assignment(Node):
    def __init__(self, var_name: str, var_value: Node):
        super().__init__(var_name, [var_value])

    def evaluate(self, st: SymbolTable):
        st.setTableValue(self.value, self.children[0].evaluate(st))
        pass

class Block(Node):
    def __init__(self, value: int | str, children = list):
        super().__init__(value, children)

    def evaluate(self, st):
        for child in self.children:
            child.evaluate(st)
        pass

class NoOp(Node):
    def __init__(self):
        super().__init__('', [])

    def evaluate(self, st):
        pass

class Token:
    def __init__(self, kind: str, value: int | str):
        self.kind = kind
        self.value = value

class Lexer:
    def __init__(self, source: str):
        self.source = source
        self.position = 0
        self.next = Token('START', '')

    def selectNext(self):
        if(self.position == len(self.source)):
            self.next = Token('EOF', '')
            return
        
        char = self.source[self.position]
        while (char == ' ' or char == '\n') and self.position < len(self.source):
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
        elif(char in '+-*/(){}=;!'):
            sign_label = {
                '+': 'PLUS',
                '-': 'MINUS',
                '*': 'MULT',
                '/': 'DIV',
                '(': 'OPEN_PAR',
                ')': 'CLOSE_PAR',
                '{': 'OPEN_BRA',
                '}': 'CLOSE_BRA',
                '=': 'ASSIGN',
                ';': 'END',
                '!': 'NOT'
            }
            self.next = Token(sign_label[char], char)
            self.position += 1
        elif(char == '&'):
            self.position += 1
            char = self.source[self.position]
            if(char != '&'):
                raise Exception(f"Lexical Error: Expected '&' at position {self.position}.")
            else:
                self.next = Token('AND', '&&')
                self.position += 1
        elif(char == '|'):
            self.position += 1
            char = self.source[self.position]
            if(char != '|'):
                raise Exception(f"Lexical Error: Expected '|' at position {self.position}.")
            else:
                self.next = Token('OR', '||')
                self.position += 1
        elif(re.match(r'[a-zA-Z]', char)):
            word = ""
            while(re.match(r'[a-zA-Z0-9_]', char)):
                word += char
                self.position += 1
                if(self.position == len(self.source)):
                    break
                else:
                    char = self.source[self.position]
            if(word == "log"):
                self.next = Token('PRINT', word)
            elif(word == 'if'):
                self.next = Token('IF', word)
            elif(word == 'else'):
                self.next = Token('ELSE', word)
            elif(word == 'while'):
                self.next = Token('WHILE', word)
            elif(word == 'readline'):
                self.next = Token('READ', word)
            else:
                self.next = Token('IDEN', word)
        else:
            print(char)
            raise Exception(f"Invalid character found at position {self.position}.")

class Parser:
    def __init__(self):
        self.lex: Lexer

    def parseFactor(self):
        if self.lex.next.kind == "INT":
            node = IntVal(self.lex.next.value)
            self.lex.selectNext()
            return node
        elif self.lex.next.kind == "IDEN":
            node = Identifier(self.lex.next.value)
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
    
    def parseStatement(self):
        if(self.lex.next.kind == 'IDEN'):
            var_name = self.lex.next.value
            self.lex.selectNext()

            if(self.lex.next.kind != 'ASSIGN'):
                raise Exception('Syntax error: Expected assignment.')
            self.lex.selectNext()

            var_value = self.parseExpression()
            if(self.lex.next.kind != 'END'):
                raise Exception('Syntax error: Expected ; token at the end of statement.')
            self.lex.selectNext()

            node = Assignment(var_name, var_value)
        
        elif(self.lex.next.kind == 'PRINT'):
            self.lex.selectNext()
            if(self.lex.next.kind != 'OPEN_PAR'):
                raise Exception('Syntax error: Expected OPEN_PAR token.')
            self.lex.selectNext()

            value = self.parseExpression()

            # print(self.lex.next.kind)

            if(self.lex.next.kind != 'CLOSE_PAR'):
                raise Exception('Syntax error: Expected CLOSE_PAR token.')
            self.lex.selectNext()
            
            if(self.lex.next.kind != 'END'):
                raise Exception('Syntax error: Expected ; token at the end of statement.')
            self.lex.selectNext()

            node = Print('print', value)

        elif(self.lex.next.kind == 'END'):
            node = NoOp()
            self.lex.selectNext()

        else:
            raise Exception('Syntax error: Invalid statement.')

        return node

    
    def parseProgram(self):
        statements = []
        while(self.lex.next.kind != 'EOF'):
            ast = self.parseStatement()
            statements.append(ast)
        node = Block('main', statements)
        return node

    def run(self, code: str):
        prepro = Prepro()
        filtered_code = prepro.filter(code)
        self.lex = Lexer(filtered_code)
        self.lex.selectNext()
        if self.lex.next.kind == "EOF":
            raise Exception("Syntax Error: Empty expression")
        ast = self.parseProgram()
        if self.lex.next.kind != "EOF":
            raise Exception("Syntax Error: Unexpected token after expression")
        return ast

if __name__ == '__main__':
    parser = Parser()
    if(len(sys.argv) == 1):
        raise Exception('Syntax Error: Empty expression.')
    else:
        with open(sys.argv[1], "r") as file:
            code = file.read()
        ast = parser.run(code)
        st = SymbolTable()
        ast.evaluate(st)