import sys
import re
from abc import ABC, abstractmethod

class Prepro:
    def filter(self, code):
        filtered_code = re.sub(r"//.*", "", code)
        return filtered_code
    
class Variable:
    def __init__(self, value: int | bool | str, type: str):
        self.value = value
        self.type = type

class SymbolTable:
    def __init__(self):
        self.table = {}

    def getTableValue(self, name: str):
        if name in self.table.keys():
            return self.table[name]
        else:
            raise Exception('Syntax error: Invalid variable name.')
    
    def setTableValue(self, name: str, var: Variable):
        if(name in self.table.keys()):
            if(self.table[name].type != var.type):
                raise Exception(f'Semantic error: Cannot assign value of type {self.table[name].type} to variable of type {var.type}')
            self.table[name] = var
        else:
            raise Exception('Semantic error: Variable not declared.')

    def createVariable(
            self, 
            name: str, 
            type_: str, 
            value: int | bool | str | None = None
        ):
        if value is None:
            if type_ == 'number':
                value = 0
            elif type_ == 'boolean':
                value = False
            elif type_ == 'string':
                value = ""
            else:
                raise Exception(f"Semantic error: Unknown type '{type_}'.")
        if name in self.table:
            raise Exception('Semantic error: Variable already declared.')
        self.table[name] = Variable(value, type_)
        return

class Node(ABC):
    def __init__(self, value: int | str, children):
        self.value = value
        self.children = children

    @abstractmethod
    def evaluate(self, st: SymbolTable):
        pass

class IntVal(Node):
    def __init__(self, value: int):
        super().__init__(value, [])

    def evaluate(self, st: SymbolTable):
        return Variable(self.value, 'number')
    
class BoolVal(Node):
    def __init__(self, value: bool):
        super().__init__(value, [])

    def evaluate(self, st: SymbolTable):
        return Variable(self.value, 'boolean')
    
class StringVal(Node):
    def __init__(self, value: str):
        super().__init__(value, [])

    def evaluate(self, st: SymbolTable):
        return Variable(self.value, 'string')
    
class Identifier(Node):
    def __init__(self, value: int | str):
        super().__init__(value, [])

    def evaluate(self, st: SymbolTable):
        return st.getTableValue(self.value)
    
class VarDec(Node):
    def __init__(
            self, 
            value: int | str, 
            variable: Identifier, 
            expression: Node | None = None
        ):
        super().__init__(value, [variable, expression])
    
    def evaluate(self, st: SymbolTable):
        variable = self.children[0]
        expression = self.children[1]

        if(expression is None):
            st.createVariable(variable.value, self.value)
        else:
            expr_value = expression.evaluate(st)
            if expr_value.type != self.value:
                raise Exception(f'Type error: Cannot initialize variable of type {self.value} with value of type {expr_value.type}.')
            st.createVariable(variable.value, self.value, expr_value.value)
    
class UnOp(Node):
    def __init__(self, value: int | str, child: Node):
        super().__init__(value, [child])

    def evaluate(self, st: SymbolTable):
        child = self.children[0].evaluate(st)
        if(self.value == 'PLUS'):
            if child.type != 'number':
                raise Exception('Type error: Unary + requires number.')
            return Variable(child.value, 'number')
        elif(self.value == 'MINUS'):
            if child.type != 'number':
                raise Exception('Type error: Unary - requires number.')
            return Variable(-child.value, 'number')
        elif(self.value == 'NOT'):
            if child.type != 'boolean':
                raise Exception('Type error: Unary ! requires boolean.')
            return Variable(not(child.value), 'boolean')
    
class BinOp(Node):
    def __init__(self, value: int | str, left: Node, right: Node):
        super().__init__(value, [left, right])

    def evaluate(self, st: SymbolTable):
        left = self.children[0].evaluate(st)
        right = self.children[1].evaluate(st)
        operation = self.value

        def var_to_str(var):
            if var.type == 'boolean':
                return 'true' if var.value else 'false'
            return str(var.value)
        
        if operation == 'PLUS':
            if left.type == 'number' and right.type == 'number':
                return Variable(left.value + right.value, 'number')
            if left.type == 'string' or right.type == 'string':
                left_s = left.value if left.type == 'string' else var_to_str(left)
                right_s = right.value if right.type == 'string' else var_to_str(right)
                return Variable(left_s + right_s, 'string')
            raise Exception('Type error: PLUS requires both operands to be numbers or at least one string for concatenation.')
        
        elif operation == 'MINUS':
            if left.type == 'number' and right.type == 'number':
                return Variable(left.value - right.value, 'number')
            raise Exception('Type error: MINUS requires number operands.')
        
        elif operation == 'MULT':
            if left.type == 'number' and right.type == 'number':
                return Variable(left.value * right.value, 'number')
            raise Exception('Type error: MULT requires number operands.')
        
        elif operation == 'DIV':
            if left.type == 'number' and right.type == 'number':
                return Variable(left.value // right.value, 'number')
            raise Exception('Type error: DIV requires number operands.')
        
        elif operation == 'GREATER':
            # números ou strings (lexicográfico)
            if left.type == 'number' and right.type == 'number':
                return Variable(left.value > right.value, 'boolean')
            if left.type == 'string' and right.type == 'string':
                return Variable(left.value > right.value, 'boolean')
            raise Exception('Type error: GREATER requires both operands to be numbers or both strings.')
        
        elif operation == 'LESS':
            if left.type == 'number' and right.type == 'number':
                return Variable(left.value < right.value, 'boolean')
            if left.type == 'string' and right.type == 'string':
                return Variable(left.value < right.value, 'boolean')
            raise Exception('Type error: LESS requires both operands to be numbers or both strings.')
        
        elif operation == 'EQUAL':
            if left.type != right.type:
                raise Exception('Type error: EQUAL requires both operands to have same type.')
            return Variable(left.value == right.value, 'boolean')
       
        elif operation == 'AND':
            if left.type == 'boolean' and right.type == 'boolean':
                return Variable(left.value and right.value, 'boolean')
            raise Exception('Type error: AND requires boolean operands.')
        
        elif operation == 'OR':
            if left.type == 'boolean' and right.type == 'boolean':
                return Variable(left.value or right.value, 'boolean')
            raise Exception('Type error: OR requires boolean operands.')
    
class Print(Node):
    def __init__(self, value: int | str, child: Node):
        super().__init__(value, [child])

    def evaluate(self, st: SymbolTable):
        var = self.children[0].evaluate(st)
        if var.type == 'boolean':
            print('true' if var.value else 'false')
        else:
            print(var.value)

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

class Read(Node):
    def __init__(self):
        super().__init__("READ", [])
    
    def evaluate(self, st):
        value = int(input())
        return Variable(value, 'number')
    
class If(Node):
    def __init__(self, condition, expected, alternative):
        super().__init__('IF', [condition, expected, alternative])

    def evaluate(self, st):
        if self.children[0].evaluate(st).type != 'boolean':
            raise Exception('Type error: IF condition must be boolean.')
        
        if(self.children[0].evaluate(st).value):
            self.children[1].evaluate(st)
        else:
            self.children[2].evaluate(st)

class While(Node):
    def __init__(self, condition, execution):
        super().__init__('IF', [condition, execution])

    def evaluate(self, st):
        if self.children[0].evaluate(st).type != 'boolean':
            raise Exception('Type error: WHILE condition must be boolean.')
        while(self.children[0].evaluate(st).value):
            self.children[1].evaluate(st)

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
            if(self.position >= len(self.source)):
                self.next = Token('EOF', '')
                return
            char = self.source[self.position]
            
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
        elif(char == '"'):
            # capturar conteúdo entre aspas (não inclui as aspas)
            word = ""
            self.position += 1  # pular abertura
            while self.position < len(self.source) and self.source[self.position] != '"':
                word += self.source[self.position]
                self.position += 1
            if self.position == len(self.source):
                self.next = Token('EOF', '')
                return
            else:
                # agora self.source[self.position] é a aspas de fechamento
                self.next = Token('STR', word)
                self.position += 1  # pular fechamento
        elif(char in '+-*/(){};!><:'):
            sign_label = {
                '+': 'PLUS',
                '-': 'MINUS',
                '*': 'MULT',
                '/': 'DIV',
                '(': 'OPEN_PAR',
                ')': 'CLOSE_PAR',
                '{': 'OPEN_BRA',
                '}': 'CLOSE_BRA',
                ';': 'END',
                '!': 'NOT',
                '>': 'GREATER',
                '<': 'LESS',
                ':': 'COLON'
            }
            self.next = Token(sign_label[char], char)
            self.position += 1
        elif(char == '='):
            self.position += 1
            if(self.source[self.position] == '='):
                self.next = Token('EQUAL', '==')
                self.position += 1
                if(self.source[self.position] == '='):
                    self.position += 1
            else:
                self.next = Token('ASSIGN', '=')
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
            elif(word == 'let'):
                self.next = Token('VAR', word)
            elif(word in ['true', 'false']):
                self.next = Token('BOOL', word)
            elif(word in ['string', 'number', 'boolean']):
                self.next = Token('TYPE', word)
            else:
                self.next = Token('IDEN', word)
        else:
            raise Exception(f"Invalid character found at position {self.position}.")

class Parser:
    def __init__(self):
        self.lex: Lexer

    def parseRelExpr(self):
        node = self.parseExpression()
        while self.lex.next.kind in ("GREATER", "LESS", "EQUAL"):
            operation = self.lex.next.kind
            self.lex.selectNext()
            right = self.parseExpression()
            node = BinOp(operation, node, right)
        return node
    
    def parseBoolTerm(self):
        node = self.parseRelExpr()
        while self.lex.next.kind == "AND":
            operation = self.lex.next.kind
            self.lex.selectNext()
            right = self.parseRelExpr()
            node = BinOp(operation, node, right)
        return node
    
    def parseBoolExpr(self):
        node = self.parseBoolTerm()
        while self.lex.next.kind == "OR":
            operation = self.lex.next.kind
            self.lex.selectNext()
            right = self.parseBoolTerm()
            node = BinOp(operation, node, right)
        return node

    def parseFactor(self):
        if self.lex.next.kind == "INT":
            node = IntVal(self.lex.next.value)
            self.lex.selectNext()
            return node
        elif self.lex.next.kind == "STR":
            node = StringVal(self.lex.next.value)
            self.lex.selectNext()
            return node
        elif self.lex.next.kind == "BOOL":
            val = True if self.lex.next.value == 'true' else False
            node = BoolVal(val)
            self.lex.selectNext()
            return node
        elif self.lex.next.kind == "IDEN":
            node = Identifier(self.lex.next.value)
            self.lex.selectNext()
            return node
        elif self.lex.next.kind in ("PLUS", "MINUS", "NOT"):
            operation = self.lex.next.kind
            self.lex.selectNext()
            return UnOp(operation, self.parseFactor())
        elif self.lex.next.kind == "OPEN_PAR":
            self.lex.selectNext()
            node = self.parseBoolExpr()
            if self.lex.next.kind != "CLOSE_PAR":
                raise Exception("Syntax Error: Expected ')' after expression.")
            self.lex.selectNext()
            return node
        elif self.lex.next.kind == "READ":
            self.lex.selectNext()
            if self.lex.next.kind != "OPEN_PAR":
                raise Exception("Syntax Error: Expected '(' after READ token.")
            
            self.lex.selectNext()
            if self.lex.next.kind != "CLOSE_PAR":
                raise Exception("Syntax Error: Expected ')' after expression.")
            
            self.lex.selectNext()
            return Read()
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

            var_value = self.parseBoolExpr()
            if(self.lex.next.kind != 'END'):
                print(self.lex.next.kind)
                raise Exception('Syntax error: Expected ; token at the end of statement.')
            self.lex.selectNext()

            node = Assignment(var_name, var_value)
        
        elif(self.lex.next.kind == 'PRINT'):
            self.lex.selectNext()
            if(self.lex.next.kind != 'OPEN_PAR'):
                raise Exception('Syntax error: Expected OPEN_PAR token.')
            self.lex.selectNext()

            value = self.parseBoolExpr()

            if(self.lex.next.kind != 'CLOSE_PAR'):
                raise Exception('Syntax error: Expected CLOSE_PAR token.')
            self.lex.selectNext()
            
            if(self.lex.next.kind != 'END'):
                raise Exception('Syntax error: Expected ; token at the end of statement.')
            self.lex.selectNext()

            node = Print('print', value)

        elif(self.lex.next.kind == 'IF'):
            self.lex.selectNext()
            if(self.lex.next.kind != 'OPEN_PAR'):
                raise Exception('Syntax error: Expected OPEN_PAR token on IF statement.')
            
            self.lex.selectNext()
            condition = self.parseBoolExpr()

            if(self.lex.next.kind != 'CLOSE_PAR'):
                raise Exception('Syntax error: Expected CLOSE_PAR token.')
            
            self.lex.selectNext()
            expected = self.parseStatement()

            alternative = NoOp()
            if(self.lex.next.kind == 'ELSE'):
                self.lex.selectNext()
                alternative = self.parseStatement()
            node = If(condition, expected, alternative)

        elif(self.lex.next.kind == 'WHILE'):
            self.lex.selectNext()
            if(self.lex.next.kind != 'OPEN_PAR'):
                raise Exception('Syntax error: Expected OPEN_PAR token on WHILE statement.')
            
            self.lex.selectNext()
            condition = self.parseBoolExpr()

            if(self.lex.next.kind != 'CLOSE_PAR'):
                raise Exception('Syntax error: Expected CLOSE_PAR token.')
            
            self.lex.selectNext()
            execution = self.parseStatement()

            node = While(condition, execution)

        elif self.lex.next.kind == 'VAR':
            self.lex.selectNext()

            if self.lex.next.kind == 'IDEN':
                ident = Identifier(self.lex.next.value)
                self.lex.selectNext()

                if self.lex.next.kind != 'COLON':
                    raise Exception("Syntax error: Expected ':' after identifier in variable declaration.")
                self.lex.selectNext()

                if self.lex.next.kind != 'TYPE':
                    raise Exception("Syntax error: Expected type after ':' in variable declaration.")
                type_token = self.lex.next.value
                self.lex.selectNext()
            else:
                raise Exception('Syntax error: Expected type or identifier after let.')

            initializer = None
            if self.lex.next.kind == 'ASSIGN':
                self.lex.selectNext()
                initializer = self.parseBoolExpr()

            if self.lex.next.kind != 'END':
                raise Exception('Syntax error: Expected ; token at the end of statement.')
            self.lex.selectNext()

            node = VarDec(type_token, ident, initializer)

        elif(self.lex.next.kind == 'END'):
            node = NoOp()
            self.lex.selectNext()

        else:
            node = self.parseBlock()

        return node

    def parseBlock(self):
        statements = []

        if(self.lex.next.kind != 'OPEN_BRA'):
            raise Exception("Syntax Error: Invalid statement.")
        self.lex.selectNext()

        while(self.lex.next.kind != 'CLOSE_BRA' and self.lex.next.kind != 'EOF'):
            ast = self.parseStatement()
            statements.append(ast)

        if(self.lex.next.kind != 'CLOSE_BRA'):
            raise Exception("Syntax Error: Expected '}' at the end of Block.")
        self.lex.selectNext()

        node = Block('block', statements)
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