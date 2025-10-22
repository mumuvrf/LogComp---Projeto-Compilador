import sys
import re
from abc import ABC, abstractmethod
import os

class Prepro:
    def filter(self, code):
        filtered_code = re.sub(r"//.*", "", code)
        return filtered_code

class Variable:
    def __init__(self, value: int | bool | str, type: str, shift: int = 0):
        self.value = value
        self.type = type
        self.shift = shift  # deslocamento relativo a EBP (em bytes)

class SymbolTable:
    def __init__(self):
        self.table = {}
        self._var_count = 0  # conta variáveis para definir shift

    def getTableValue(self, name: str):
        if name in self.table.keys():
            return self.table[name]
        else:
            raise Exception('Syntax error: Invalid variable name.')
    
    def setTableValue(self, name: str, var: Variable):
        if(name in self.table.keys()):
            if(self.table[name].type != var.type):
                raise Exception(f'Semantic error: Cannot assign value of type {var.type} to variable of type {self.table[name].type}')
            # Preserve shift assigned when declared:
            var.shift = self.table[name].shift
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
        # cada variável ocupa 4 bytes; primeira variável tem shift=4 (EBP-4), segunda 8, ...
        self._var_count += 1
        shift = 4 * self._var_count
        self.table[name] = Variable(value, type_, shift)
        return

    def total_stack_bytes(self):
        return 4 * self._var_count

class Node(ABC):
    id = 0  # id estático

    @staticmethod
    def newId():
        Node.id += 1
        return Node.id

    def __init__(self, value: int | str, children):
        self.value = value
        self.children = children
        self.id = Node.newId()  # identificador único do nó

    @abstractmethod
    def evaluate(self, st: SymbolTable):
        pass

    # cada nó deve implementar generate(st) para gerar assembly em Code
    @abstractmethod
    def generate(self, st: SymbolTable):
        pass

class Code:
    instructions = []

    @staticmethod
    def append(code: str) -> None:
        Code.instructions.append(code)

    @staticmethod
    def dump(filename: str, st: SymbolTable) -> None:
        # escreve header + data + text + instrucoes + footer
        total_vars_bytes = st.total_stack_bytes()
        header = [
            "section .data",
            "  format_out: db \"%d\", 10, 0 ; format do printf",
            "  format_in: db \"%d\", 0 ; format do scanf",
            "  scan_int: dd 0; 32-bits integer",
            "",
            "section .text",
            "  extern _printf ; usar _printf para Windows",
            "  extern _scanf ; usar _scanf para Windows",
            "  extern _ExitProcess@4 ; usar para Windows",
            "  global _start ; início do programa",
            "",
            "_start:",
            "  push ebp ; guarda o EBP",
            "  mov ebp, esp ; zera a pilha"
        ]
        if total_vars_bytes > 0:
            header.append(f"  sub esp, {total_vars_bytes} ; reserva espaço para variáveis ({total_vars_bytes} bytes)")

        footer = [
            "",
            "  ; fim do código gerado",
            "  mov esp, ebp ; reestabelece a pilha",
            "  pop ebp",
            "  push dword 0",
            "  call _ExitProcess@4"
        ]

        asm_text = "\n".join(header + [""] + Code.instructions + [""] + footer)
        with open(filename, 'w') as file:
            file.write(asm_text)

class IntVal(Node):
    def __init__(self, value: int):
        super().__init__(value, [])

    def evaluate(self, st: SymbolTable):
        return Variable(self.value, 'number')

    def generate(self, st: SymbolTable):
        Code.append(f"  ; IntVal {self.value}")
        Code.append(f"  mov eax, {self.value}  ; carregar constante")

class BoolVal(Node):
    def __init__(self, value: bool):
        super().__init__(value, [])

    def evaluate(self, st: SymbolTable):
        return Variable(self.value, 'boolean')

    def generate(self, st: SymbolTable):
        val = 1 if self.value else 0
        Code.append(f"  ; BoolVal {self.value}")
        Code.append(f"  mov eax, {val}")

class StringVal(Node):
    def __init__(self, value: str):
        super().__init__(value, [])

    def evaluate(self, st: SymbolTable):
        return Variable(self.value, 'string')

    def generate(self, st: SymbolTable):
        # Roteiro: não gerar código para nós que representam ou operam strings
        Code.append(f"  ; String literal ignored in code generation: \"{self.value}\"")
        Code.append(f"  mov eax, 0")

class Identifier(Node):
    def __init__(self, value: int | str):
        super().__init__(value, [])

    def evaluate(self, st: SymbolTable):
        return st.getTableValue(self.value)

    def generate(self, st: SymbolTable):
        var = st.getTableValue(self.value)
        Code.append(f"  ; Identifier {self.value}")
        Code.append(f"  mov eax, [ebp-{var.shift}]  ; carregar variável {self.value}")

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

    def generate(self, st: SymbolTable):
        # A alocação de espaço é feita no prologue (Code.dump) com base em st.total_stack_bytes()
        ident: Identifier = self.children[0]
        initializer = self.children[1]
        Code.append(f"  ; VarDec {ident.value} : {self.value}")
        if initializer is not None:
            # gera valor da expressão em EAX e salva no deslocamento associado
            initializer.generate(st)
            var = st.getTableValue(ident.value)
            Code.append(f"  mov [ebp-{var.shift}], eax  ; inicializar {ident.value}")

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

    def generate(self, st: SymbolTable):
        op = self.value
        Code.append(f"  ; UnOp {op}")
        self.children[0].generate(st)  # resultado em EAX
        if op == 'MINUS':
            Code.append(f"  neg eax")
        elif op == 'PLUS':
            pass  # já está em EAX
        elif op == 'NOT':
            # boolean: EAX != 0 => true(1), zero => false(0)
            Code.append("  cmp eax, 0")
            Code.append("  mov eax, 0")
            Code.append(f"  sete al")
            Code.append("  movzx eax, al")

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

    def generate(self, st: SymbolTable):
        op = self.value
        Code.append(f"  ; BinOp {op}")
        # estratégia: gerar left -> eax; push eax; gerar right -> eax; pop ecx (left)
        self.children[0].generate(st)
        Code.append("  push eax  ; salvar left")
        self.children[1].generate(st)
        Code.append("  pop ecx   ; ecx = left, eax = right")
        # agora ecx = left, eax = right
        if op == 'PLUS':
            Code.append("  add eax, ecx  ; eax = right + left  (note: operands swapped)")
            # corrigir ordem: we did right in eax, left in ecx -> do eax = ecx + eax
            Code.append("  mov edx, eax")
            Code.append("  mov eax, ecx")
            Code.append("  add eax, edx")
        elif op == 'MINUS':
            # left - right => ecx - eax -> compute in eax
            Code.append("  mov edx, eax")
            Code.append("  mov eax, ecx")
            Code.append("  sub eax, edx")
        elif op == 'MULT':
            # imul eax, ecx  => eax = eax * ecx , but ensure ordering: left in ecx, right in eax
            Code.append("  mov edx, eax")
            Code.append("  mov eax, ecx")
            Code.append("  imul eax, edx")
        elif op == 'DIV':
            # left / right: dividend in eax, divisor in edx -> we need dividend=ecx, divisor=eax
            Code.append("  mov ebx, eax    ; divisor = right")
            Code.append("  mov eax, ecx    ; dividend = left")
            Code.append("  cdq")
            Code.append("  idiv ebx        ; eax = eax / ebx")
        elif op in ('GREATER', 'LESS', 'EQUAL'):
            # compare [left] ecx with [right] eax
            Code.append("  cmp ecx, eax")
            if op == 'GREATER':
                Code.append("  mov eax, 0")
                Code.append("  mov ecx, 1")
                Code.append("  cmovg eax, ecx")
            elif op == 'LESS':
                Code.append("  mov eax, 0")
                Code.append("  mov ecx, 1")
                Code.append("  cmovl eax, ecx")
            else:  # EQUAL
                Code.append("  mov eax, 0")
                Code.append("  mov ecx, 1")
                Code.append("  sete al")
                Code.append("  movzx eax, al")
        elif op == 'AND':
            # boolean: ecx (left) and eax (right) => result in eax
            Code.append("  and eax, ecx")
            Code.append("  cmp eax, 0")
            Code.append("  mov eax, 0")
            Code.append("  sete al")
            Code.append("  movzx eax, al")
        elif op == 'OR':
            Code.append("  or eax, ecx")
            Code.append("  cmp eax, 0")
            Code.append("  mov eax, 0")
            Code.append("  sete al")
            Code.append("  movzx eax, al")
        else:
            Code.append(f"  ; operação {op} não suportada no gerador")

class Print(Node):
    def __init__(self, value: int | str, child: Node):
        super().__init__(value, [child])

    def evaluate(self, st: SymbolTable):
        var = self.children[0].evaluate(st)
        if var.type == 'boolean':
            print('true' if var.value else 'false')
        else:
            print(var.value)

    def generate(self, st: SymbolTable):
        Code.append("  ; Print")
        self.children[0].generate(st)  # resultado em EAX
        Code.append("  push eax")
        Code.append("  push format_out")
        Code.append("  call _printf")
        Code.append("  add esp, 8")

class Assignment(Node):
    def __init__(self, var_name: str, var_value: Node):
        super().__init__(var_name, [var_value])

    def evaluate(self, st: SymbolTable):
        st.setTableValue(self.value, self.children[0].evaluate(st))
        pass

    def generate(self, st: SymbolTable):
        Code.append(f"  ; Assignment {self.value}")
        self.children[0].generate(st)  # resultado em EAX
        var = st.getTableValue(self.value)
        Code.append(f"  mov [ebp-{var.shift}], eax  ; {self.value} = EAX")

class Block(Node):
    def __init__(self, value: int | str, children = list):
        super().__init__(value, children)

    def evaluate(self, st: SymbolTable):
        for child in self.children:
            child.evaluate(st)
        pass

    def generate(self, st: SymbolTable):
        Code.append(f"  ; Block {self.value} start")
        for child in self.children:
            child.generate(st)
        Code.append(f"  ; Block {self.value} end")

class Read(Node):
    def __init__(self):
        super().__init__("READ", [])
    
    def evaluate(self, st: SymbolTable):
        value = int(input())
        return Variable(value, 'number')
    
    def generate(self, st: SymbolTable):
        Code.append("  ; Read (scanf)")
        Code.append("  push scan_int")
        Code.append("  push format_in")
        Code.append("  call _scanf")
        Code.append("  add esp, 8")
        Code.append("  mov eax, [scan_int]  ; valor lido em eax")

class If(Node):
    def __init__(self, condition, expected, alternative):
        super().__init__('IF', [condition, expected, alternative])

    def evaluate(self, st: SymbolTable):
        if self.children[0].evaluate(st).type != 'boolean':
            raise Exception('Type error: IF condition must be boolean.')
        
        if(self.children[0].evaluate(st).value):
            self.children[1].evaluate(st)
        else:
            self.children[2].evaluate(st)

    def generate(self, st: SymbolTable):
        myid = self.id
        Code.append(f"  ; IF start (id={myid})")
        # gerar condição -> eax
        self.children[0].generate(st)
        Code.append("  cmp eax, 0")
        Code.append(f"  je else_{myid}")
        # then
        self.children[1].generate(st)
        Code.append(f"  jmp exit_{myid}")
        # else
        Code.append(f"else_{myid}:")
        self.children[2].generate(st)
        Code.append(f"exit_{myid}:")

class While(Node):
    def __init__(self, condition, execution):
        super().__init__('IF', [condition, execution])

    def evaluate(self, st: SymbolTable):
        if self.children[0].evaluate(st).type != 'boolean':
            raise Exception('Type error: WHILE condition must be boolean.')
        while(self.children[0].evaluate(st).value):
            self.children[1].evaluate(st)

    def generate(self, st: SymbolTable):
        myid = self.id
        Code.append(f"  ; WHILE start (id={myid})")
        Code.append(f"loop_{myid}:")
        self.children[0].generate(st)  # cond -> eax
        Code.append("  cmp eax, 0")
        Code.append(f"  je exit_{myid}")
        self.children[1].generate(st)
        Code.append(f"  jmp loop_{myid}")
        Code.append(f"exit_{myid}:")

class NoOp(Node):
    def __init__(self):
        super().__init__('', [])

    def evaluate(self, st: SymbolTable):
        pass

    def generate(self, st: SymbolTable):
        Code.append("  ; NoOp")

# -------------------------
# Função adicionada — mínima
# -------------------------
def register_declarations(node, st: SymbolTable):
    """
    Percorre a AST recursivamente e registra todas as VarDec (declarações)
    na SymbolTable, para que os shifts estejam disponíveis para a geração.
    Não avalia inicializadores — só reserva espaço.
    """
    if node is None:
        return

    # Se for VarDec, registra a variável (somente o nome e o tipo)
    if isinstance(node, VarDec):
        ident_node = node.children[0]  # Identifier
        type_name = node.value         # tipo armazenado no VarDec.value
        # createVariable levanta erro se já existir — manter esse comportamento
        st.createVariable(ident_node.value, type_name)
        # NÃO avaliamos o inicializador aqui (se houver), apenas reservamos shift

    # Percorre filhos (se houver) recursivamente
    if hasattr(node, 'children') and node.children:
        # children pode ser lista ou outro nó
        for child in node.children:
            if child is not None:
                register_declarations(child, st)

# -------------------------
# Continuação do código...
# -------------------------

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
        # não executa evaluate; gera assembly
        # ---> registra declarações primeiro (para que shifts existam)
        register_declarations(ast, st)
        ast.generate(st)
        # escreve arquivo asm com mesmo prefixo do arquivo de entrada
        inpath = sys.argv[1]
        prefix, _ = os.path.splitext(inpath)
        outname = prefix + ".asm"
        Code.dump(outname, st)
        print(f"Assembly gerado em: {outname}")
