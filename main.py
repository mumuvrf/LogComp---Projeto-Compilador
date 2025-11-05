import sys
import re
from abc import ABC, abstractmethod

class Prepro:
    def filter(self, code):
        filtered_code = re.sub(r"//.*", "", code)
        return filtered_code

class ReturnValue:
    def __init__(self, variable):
        self.variable = variable

class Variable:
    def __init__(self, value: int | bool | str | object, type: str, is_function: bool = False, decl_env = None):
        self.value = value
        self.type = type
        self.is_function = is_function  # sinaliza se é função
        self.decl_env = decl_env        # referencia para a SymbolTable onde foi declarada (útil para chamadas)

class SymbolTable:
    def __init__(self, parent = None):
        self.table = {}
        self.parent = parent

    def getTableValue(self, name: str):
        # busca recursiva ascendendo pais
        if name in self.table.keys():
            return self.table[name]
        elif self.parent is not None:
            return self.parent.getTableValue(name)
        else:
            raise Exception(f"[Semantic] Syntax error: Invalid variable name.")

    def setTableValue(self, name: str, var: Variable):
        # procura a tabela onde a variável foi declarada e atualiza lá
        if name in self.table.keys():
            # não permitir atribuir a uma função
            if self.table[name].is_function:
                raise Exception("[Semantic] Semantic error: Cannot assign to a function name.")
            if(self.table[name].type != var.type):
                raise Exception(f"[Semantic] Semantic error: Cannot assign value of type {var.type} to variable of type {self.table[name].type}")
            self.table[name] = var
            return
        elif self.parent is not None:
            self.parent.setTableValue(name, var)
            return
        else:
            raise Exception("[Semantic] Semantic error: Variable not declared.")

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
            elif type_ == 'void':
                value = None
            else:
                raise Exception(f"[Semantic] Semantic error: Unknown type '{type_}'.")
        if name in self.table:
            raise Exception("[Semantic] Semantic error: Variable already declared.")
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
                raise Exception(f"[Semantic] Type error: Cannot initialize variable of type {self.value} with value of type {expr_value.type}.")
            st.createVariable(variable.value, self.value, expr_value.value)

class UnOp(Node):
    def __init__(self, value: int | str, child: Node):
        super().__init__(value, [child])

    def evaluate(self, st: SymbolTable):
        child = self.children[0].evaluate(st)
        if(self.value == 'PLUS'):
            if child.type != 'number':
                raise Exception('[Semantic] Type error: Unary + requires number.')
            return Variable(child.value, 'number')
        elif(self.value == 'MINUS'):
            if child.type != 'number':
                raise Exception('[Semantic] Type error: Unary - requires number.')
            return Variable(-child.value, 'number')
        elif(self.value == 'NOT'):
            if child.type != 'boolean':
                raise Exception('[Semantic] Type error: Unary ! requires boolean.')
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
            raise Exception('[Semantic] Type error: PLUS requires both operands to be numbers or at least one string for concatenation.')

        elif operation == 'MINUS':
            if left.type == 'number' and right.type == 'number':
                return Variable(left.value - right.value, 'number')
            raise Exception('[Semantic] Type error: MINUS requires number operands.')

        elif operation == 'MULT':
            if left.type == 'number' and right.type == 'number':
                return Variable(left.value * right.value, 'number')
            raise Exception('[Semantic] Type error: MULT requires number operands.')

        elif operation == 'DIV':
            if left.type == 'number' and right.type == 'number':
                return Variable(left.value // right.value, 'number')
            raise Exception('[Semantic] Type error: DIV requires number operands.')

        elif operation == 'GREATER':
            # números ou strings (lexicográfico)
            if left.type == 'number' and right.type == 'number':
                return Variable(left.value > right.value, 'boolean')
            if left.type == 'string' and right.type == 'string':
                return Variable(left.value > right.value, 'boolean')
            raise Exception('[Semantic] Type error: GREATER requires both operands to be numbers or both strings.')

        elif operation == 'LESS':
            if left.type == 'number' and right.type == 'number':
                return Variable(left.value < right.value, 'boolean')
            if left.type == 'string' and right.type == 'string':
                return Variable(left.value < right.value, 'boolean')
            raise Exception('[Semantic] Type error: LESS requires both operands to be numbers or both strings.')

        elif operation == 'EQUAL':
            if left.type != right.type:
                raise Exception('[Semantic] Type error: EQUAL requires both operands to have same type.')
            return Variable(left.value == right.value, 'boolean')

        elif operation == 'AND':
            if left.type == 'boolean' and right.type == 'boolean':
                return Variable(left.value and right.value, 'boolean')
            raise Exception('[Semantic] Type error: AND requires boolean operands.')

        elif operation == 'OR':
            if left.type == 'boolean' and right.type == 'boolean':
                return Variable(left.value or right.value, 'boolean')
            raise Exception('[Semantic] Type error: OR requires boolean operands.')

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

    def evaluate(self, st: SymbolTable):
        # Executa statements; se encontrar um ReturnValue, propaga-o pra fora
        for child in self.children:
            # se o filho for um Block interno, criar nova SymbolTable encadeada
            if isinstance(child, Block):
                new_st = SymbolTable(parent=st)
                result = child.evaluate(new_st)
            else:
                result = child.evaluate(st)

            if isinstance(result, ReturnValue):
                return result
        pass

class Read(Node):
    def __init__(self):
        super().__init__("READ", [])

    def evaluate(self, st: SymbolTable):
        value = int(input())
        return Variable(value, 'number')

class If(Node):
    def __init__(self, condition, expected, alternative):
        super().__init__('IF', [condition, expected, alternative])

    def evaluate(self, st: SymbolTable):
        cond = self.children[0].evaluate(st)
        if cond.type != 'boolean':
            raise Exception('[Semantic] Type error: IF condition must be boolean.')

        if(cond.value):
            result = self.children[1].evaluate(st)
            if isinstance(result, ReturnValue):
                return result
        else:
            result = self.children[2].evaluate(st)
            if isinstance(result, ReturnValue):
                return result

class While(Node):
    def __init__(self, condition, execution):
        super().__init__('WHILE', [condition, execution])

    def evaluate(self, st: SymbolTable):
        cond = self.children[0].evaluate(st)
        if cond.type != 'boolean':
            raise Exception('[Semantic] Type error: WHILE condition must be boolean.')
        while(self.children[0].evaluate(st).value):
            result = self.children[1].evaluate(st)
            if isinstance(result, ReturnValue):
                return result

class NoOp(Node):
    def __init__(self):
        super().__init__('', [])

    def evaluate(self, st: SymbolTable):
        pass

class Return(Node):
    def __init__(self, child: Node):
        super().__init__('RETURN', [child])

    def evaluate(self, st: SymbolTable):
        val = self.children[0].evaluate(st)
        return ReturnValue(val)

class FuncDec(Node):
    def __init__(self, return_type: str, name: Identifier, params: list, body: Block):
        # value = return type; children = [Identifier, param1 VarDec, param2 VarDec, ..., Block]
        children = [name] + params + [body]
        super().__init__(return_type, children)

    def evaluate(self, st: SymbolTable):
        # registra a função na tabela atual (deve ser a primeira SymbolTable conforme roteiro)
        func_name = self.children[0].value
        # Armazenamos o próprio nó FuncDec como value na Variable, marcando is_function e salvando o env
        if func_name in st.table:
            raise Exception('[Semantic] Semantic error: Function already declared.')
        v = Variable(self, self.value, is_function=True, decl_env=st)
        st.table[func_name] = v
        return

class FuncCall(Node):
    def __init__(self, name: str, args: list):
        super().__init__(name, args)  # value = function name, children = exprs

    def evaluate(self, st: SymbolTable):
        # verificar existência da função
        try:
            func_var = st.getTableValue(self.value)
        except Exception:
            raise Exception('[Semantic] Semantic error: Function not declared.')

        if not func_var.is_function:
            raise Exception('[Semantic] Semantic error: Identifier is not a function.')

        func_dec: FuncDec = func_var.value
        # parâmetros declarados na função: são children[1:-1] do FuncDec (1..n)
        declared_params = func_dec.children[1:-1]
        if len(declared_params) != len(self.children):
            raise Exception('[Semantic] Semantic error: Incorrect number of arguments in function call.')

        # criar nova SymbolTable encadeada; usar o ambiente onde a função foi declarada como parent
        parent_env = func_var.decl_env if func_var.decl_env is not None else st
        new_st = SymbolTable(parent=parent_env)

        # declarar parâmetros na nova tabela e atribuir valores avaliados
        for i, param_node in enumerate(declared_params):
            # param_node é VarDec com value = type e child[0] = Identifier
            param_name = param_node.children[0].value
            param_type = param_node.value
            arg_value = self.children[i].evaluate(st)
            if arg_value.type != param_type:
                raise Exception('[Semantic] Semantic error: Argument type mismatch in function call.')
            # criar variável local do parâmetro
            new_st.createVariable(param_name, param_type, arg_value.value)

        # executar o corpo da função (último filho)
        body: Block = func_dec.children[-1]
        result = body.evaluate(new_st)
        if isinstance(result, ReturnValue):
            # verificar tipo de retorno
            ret_var = result.variable
            if func_dec.value == 'void':
                raise Exception('[Semantic] Semantic error: Function declared void cannot return a value.')
            if ret_var.type != func_dec.value:
                raise Exception('[Semantic] Semantic error: Function return type mismatch.')
            return ret_var
        else:
            # sem return explícito
            if func_dec.value == 'void':
                return Variable(None, 'void')
            else:
                raise Exception('[Semantic] Semantic error: Function did not return a value.')

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
        while (char == ' ' or char == '\n' or char == '\t' or char == '\r') and self.position < len(self.source):
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
                raise Exception("[Lexer] Lexical Error: Unterminated string literal.")
            else:
                # agora self.source[self.position] é a aspas de fechamento
                self.next = Token('STR', word)
                self.position += 1  # pular fechamento
        elif(char in '+-*/(){};!><:,' ):
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
                ':': 'COLON',
                ',': 'COMMA'
            }
            self.next = Token(sign_label[char], char)
            self.position += 1
        elif(char == '='):
            self.position += 1
            if(self.position < len(self.source) and self.source[self.position] == '='):
                self.next = Token('EQUAL', '==')
                self.position += 1
                if(self.position < len(self.source) and self.source[self.position] == '='):
                    self.position += 1
            else:
                self.next = Token('ASSIGN', '=')
        elif(char == '&'):
            self.position += 1
            if self.position >= len(self.source):
                raise Exception("[Lexer] Lexical Error: Expected '&' after '&'.")
            char = self.source[self.position]
            if(char != '&'):
                raise Exception(f"[Lexer] Lexical Error: Expected '&' at position {self.position}.")
            else:
                self.next = Token('AND', '&&')
                self.position += 1
        elif(char == '|'):
            self.position += 1
            if self.position >= len(self.source):
                raise Exception("[Lexer] Lexical Error: Expected '|' after '|'.")
            char = self.source[self.position]
            if(char != '|'):
                raise Exception(f"[Lexer] Lexical Error: Expected '|' at position {self.position}.")
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
            elif(word == 'function'):
                self.next = Token('FUNC', word)
            elif(word == 'return'):
                self.next = Token('RETURN', word)
            elif(word in ['true', 'false']):
                self.next = Token('BOOL', word)
            elif(word in ['string', 'number', 'boolean', 'void']):
                self.next = Token('TYPE', word)
            else:
                self.next = Token('IDEN', word)
        else:
            raise Exception(f"[Lexer] Invalid character found at position {self.position}.")

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
            # pode ser identifier ou chamada de função
            name = self.lex.next.value
            self.lex.selectNext()
            if self.lex.next.kind == 'OPEN_PAR':
                # chamada de função em expressão
                self.lex.selectNext()
                args = []
                if self.lex.next.kind != 'CLOSE_PAR':
                    args.append(self.parseBoolExpr())
                    while self.lex.next.kind == 'COMMA':
                        self.lex.selectNext()
                        args.append(self.parseBoolExpr())
                if self.lex.next.kind != 'CLOSE_PAR':
                    raise Exception("[Parser] Syntax Error: Expected ')' after function call arguments.")
                self.lex.selectNext()
                return FuncCall(name, args)
            else:
                return Identifier(name)
        elif self.lex.next.kind in ("PLUS", "MINUS", "NOT"):
            operation = self.lex.next.kind
            self.lex.selectNext()
            # após operador unário, deve vir um fator; se não vier, lançamos erro de parser
            if self.lex.next.kind in ('END', 'CLOSE_PAR', 'CLOSE_BRA', 'EOF'):
                raise Exception("[Parser] Syntax Error: Missing expression after unary operator.")
            return UnOp(operation, self.parseFactor())
        elif self.lex.next.kind == "OPEN_PAR":
            self.lex.selectNext()
            node = self.parseBoolExpr()
            if self.lex.next.kind != "CLOSE_PAR":
                raise Exception("[Parser] Syntax Error: Expected ')' after expression.")
            self.lex.selectNext()
            return node
        elif self.lex.next.kind == "READ":
            self.lex.selectNext()
            if self.lex.next.kind != "OPEN_PAR":
                raise Exception("[Parser] Syntax Error: Expected '(' after READ token.")
            self.lex.selectNext()
            if self.lex.next.kind != "CLOSE_PAR":
                raise Exception("[Parser] Syntax Error: Expected ')' after expression.")
            self.lex.selectNext()
            return Read()
        else:
            raise Exception("[Parser] Syntax Error: invalid factor")

    def parseTerm(self):
        node = self.parseFactor()
        if(self.lex.next.kind == 'INT'):
            raise Exception('[Parser] Syntax Error: There must be an operation between two numbers.')
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
            # se após + ou - vier um token ilegal (por exemplo ; ou } ou EOF), devemos reportar erro de parser específico
            if self.lex.next.kind in ('END', 'CLOSE_PAR', 'CLOSE_BRA', 'EOF'):
                raise Exception("[Parser] Syntax Error: Missing expression after binary operator.")
            right = self.parseTerm()
            node = BinOp(operation, node, right)
        return node

    def parseStatement(self):
        if(self.lex.next.kind == 'IDEN'):
            # pode ser atribuição ou chamada de função quando vem OPEN_PAR
            var_name = self.lex.next.value
            self.lex.selectNext()

            if(self.lex.next.kind == 'ASSIGN'):
                self.lex.selectNext()
                # se após '=' vier token terminador, erro parser
                if self.lex.next.kind in ('END', 'CLOSE_BRA', 'CLOSE_PAR', 'EOF'):
                    raise Exception("[Parser] Syntax Error: Missing expression after assignment operator.")
                var_value = self.parseBoolExpr()
                if(self.lex.next.kind != 'END'):
                    raise Exception('[Parser] Syntax error: Expected ; token at the end of statement.')
                self.lex.selectNext()
                node = Assignment(var_name, var_value)

            elif self.lex.next.kind == 'OPEN_PAR':
                # chamada de função como statement
                self.lex.selectNext()
                args = []
                if self.lex.next.kind != 'CLOSE_PAR':
                    args.append(self.parseBoolExpr())
                    while self.lex.next.kind == 'COMMA':
                        self.lex.selectNext()
                        args.append(self.parseBoolExpr())
                if self.lex.next.kind != 'CLOSE_PAR':
                    raise Exception('[Parser] Syntax error: Expected ) after function call.')
                self.lex.selectNext()
                if self.lex.next.kind != 'END':
                    raise Exception('[Parser] Syntax error: Expected ; token at the end of statement.')
                self.lex.selectNext()
                node = FuncCall(var_name, args)

            else:
                raise Exception('[Parser] Syntax error: Expected assignment or function call.')

        elif(self.lex.next.kind == 'PRINT'):
            self.lex.selectNext()
            if(self.lex.next.kind != 'OPEN_PAR'):
                raise Exception('[Parser] Syntax error: Expected OPEN_PAR token.')
            self.lex.selectNext()

            value = self.parseBoolExpr()

            if(self.lex.next.kind != 'CLOSE_PAR'):
                raise Exception('[Parser] Syntax error: Expected CLOSE_PAR token.')
            self.lex.selectNext()

            if(self.lex.next.kind != 'END'):
                raise Exception('[Parser] Syntax error: Expected ; token at the end of statement.')
            self.lex.selectNext()

            node = Print('print', value)

        elif(self.lex.next.kind == 'IF'):
            self.lex.selectNext()
            if(self.lex.next.kind != 'OPEN_PAR'):
                raise Exception('[Parser] Syntax error: Expected OPEN_PAR token on IF statement.')

            self.lex.selectNext()
            condition = self.parseBoolExpr()

            if(self.lex.next.kind != 'CLOSE_PAR'):
                raise Exception('[Parser] Syntax error: Expected CLOSE_PAR token.')

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
                raise Exception('[Parser] Syntax error: Expected OPEN_PAR token on WHILE statement.')

            self.lex.selectNext()
            condition = self.parseBoolExpr()

            if(self.lex.next.kind != 'CLOSE_PAR'):
                raise Exception('[Parser] Syntax error: Expected CLOSE_PAR token.')

            self.lex.selectNext()
            execution = self.parseStatement()

            node = While(condition, execution)

        elif self.lex.next.kind == 'VAR':
            self.lex.selectNext()

            if self.lex.next.kind == 'IDEN':
                ident = Identifier(self.lex.next.value)
                self.lex.selectNext()

                if self.lex.next.kind != 'COLON':
                    raise Exception("[Parser] Syntax error: Expected ':' after identifier in variable declaration.")
                self.lex.selectNext()

                if self.lex.next.kind != 'TYPE':
                    raise Exception("[Parser] Syntax error: Expected type after ':' in variable declaration.")
                type_token = self.lex.next.value
                self.lex.selectNext()
            else:
                raise Exception('[Parser] Syntax error: Expected type or identifier after let.')

            initializer = None
            if self.lex.next.kind == 'ASSIGN':
                self.lex.selectNext()
                # se não houver expressão após '=', reportar parser
                if self.lex.next.kind in ('END', 'CLOSE_BRA', 'CLOSE_PAR', 'EOF'):
                    raise Exception("[Parser] Syntax Error: Missing initializer expression in declaration.")
                initializer = self.parseBoolExpr()

            if self.lex.next.kind != 'END':
                raise Exception('[Parser] Syntax error: Expected ; token at the end of statement.')
            self.lex.selectNext()

            node = VarDec(type_token, ident, initializer)

        elif(self.lex.next.kind == 'RETURN'):
            self.lex.selectNext()
            expr = self.parseBoolExpr()
            if self.lex.next.kind != 'END':
                raise Exception('[Parser] Syntax error: Expected ; after return.')
            self.lex.selectNext()
            node = Return(expr)

        elif(self.lex.next.kind == 'END'):
            node = NoOp()
            self.lex.selectNext()

        else:
            node = self.parseBlock()

        return node

    def parseBlock(self):
        statements = []

        if(self.lex.next.kind != 'OPEN_BRA'):
            raise Exception("[Parser] Syntax Error: Invalid statement.")
        self.lex.selectNext()

        while(self.lex.next.kind != 'CLOSE_BRA' and self.lex.next.kind != 'EOF'):
            ast = self.parseStatement()
            statements.append(ast)

        if(self.lex.next.kind != 'CLOSE_BRA'):
            raise Exception("[Parser] Syntax Error: Expected '}' at the end of Block.")
        self.lex.selectNext()

        node = Block('block', statements)
        return node

    def parse_func_declaration(self):
        # já consumiu 'function' no chamador
        if self.lex.next.kind != 'IDEN':
            raise Exception('[Parser] Syntax Error: Expected function name after function.')
        name = Identifier(self.lex.next.value)
        self.lex.selectNext()

        if self.lex.next.kind != 'OPEN_PAR':
            raise Exception('[Parser] Syntax Error: Expected ( after function name.')
        self.lex.selectNext()

        params = []
        if self.lex.next.kind != 'CLOSE_PAR':
            # pelo menos um parâmetro
            if self.lex.next.kind != 'IDEN':
                raise Exception('[Parser] Syntax Error: Expected identifier in parameter list.')
            param_ident = Identifier(self.lex.next.value)
            self.lex.selectNext()
            if self.lex.next.kind != 'COLON':
                raise Exception("[Parser] Syntax error: Expected ':' after identifier in parameter.")
            self.lex.selectNext()
            if self.lex.next.kind != 'TYPE':
                raise Exception(" [Parser] Syntax error: Expected type after ':' in parameter.")
            param_type = self.lex.next.value
            self.lex.selectNext()
            params.append(VarDec(param_type, param_ident, None))
            while self.lex.next.kind == 'COMMA':
                self.lex.selectNext()
                if self.lex.next.kind != 'IDEN':
                    raise Exception('[Parser] Syntax Error: Expected identifier in parameter list.')
                param_ident = Identifier(self.lex.next.value)
                self.lex.selectNext()
                if self.lex.next.kind != 'COLON':
                    raise Exception("[Parser] Syntax error: Expected ':' after identifier in parameter.")
                self.lex.selectNext()
                if self.lex.next.kind != 'TYPE':
                    raise Exception(" [Parser] Syntax error: Expected type after ':' in parameter.")
                param_type = self.lex.next.value
                self.lex.selectNext()
                params.append(VarDec(param_type, param_ident, None))

        if self.lex.next.kind != 'CLOSE_PAR':
            raise Exception('[Parser] Syntax Error: Expected ) after parameter list.')
        self.lex.selectNext()

        if self.lex.next.kind != 'COLON':
            raise Exception('[Parser] Syntax Error: Expected : return type after function header.')
        self.lex.selectNext()

        if self.lex.next.kind != 'TYPE':
            raise Exception('[Parser] Syntax Error: Expected return TYPE after colon.')
        return_type = self.lex.next.value
        self.lex.selectNext()

        # body
        if self.lex.next.kind != 'OPEN_BRA':
            raise Exception('[Parser] Syntax Error: Expected { to start function body.')
        body = self.parseBlock()

        func_node = FuncDec(return_type, name, params, body)
        return func_node

    def parseProgram(self):
        statements = []
        while(self.lex.next.kind != 'EOF'):
            # aceitar declarações de função no topo
            if self.lex.next.kind == 'FUNC':
                self.lex.selectNext()
                func = self.parse_func_declaration()
                statements.append(func)
            else:
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
            raise Exception("[Parser] Syntax Error: Empty expression")
        ast = self.parseProgram()
        if self.lex.next.kind != "EOF":
            raise Exception("[Parser] Syntax Error: Unexpected token after expression")
        return ast

if __name__ == '__main__':
    parser = Parser()
    if(len(sys.argv) == 1):
        raise Exception('[Parser] Syntax Error: Empty expression.')
    else:
        with open(sys.argv[1], "r") as file:
            code = file.read()
        ast = parser.run(code)
        st = SymbolTable()
        # Avaliar todas declarações/decl de função e statements no escopo global
        ast.evaluate(st)
