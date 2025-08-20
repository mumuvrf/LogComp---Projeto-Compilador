import sys

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
        if(char == '+'):
            self.next = Token('PLUS', '+')
            self.position += 1
        elif(char == '-'):
            self.next = Token('MINUS', '-')
            self.position += 1
        elif(char.isdigit()):
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
            raise Exception(f"Invalid character found at position {self.position}.")

class Parser:
    def __init__(self):
        self.lex : Lexer

    def parseExpression(self):
        self.lex.selectNext()
        if(self.lex.next.kind != 'INT'):
            raise Exception('Syntax Error: Expression must start with an Integer.')
        else:
            resultado = self.lex.next.value
            self.lex.selectNext()
            if(self.lex.next.kind == 'INT'):
                raise Exception('Syntax Error: There must be an operation between two numbers.')
            while((self.lex.next.kind == 'PLUS' or self.lex.next.kind == 'MINUS') and self.lex.next.kind != 'EOF'):
                operation = self.lex.next.kind
                self.lex.selectNext()
                if(self.lex.next.kind != 'INT'):
                    raise Exception('Syntax Error: Number not found after operation signal.')
                else:
                    if(operation == 'MINUS'):
                        resultado -= self.lex.next.value
                    elif(operation == 'PLUS'):
                        resultado += self.lex.next.value
                self.lex.selectNext()
            return resultado

    def run(self, code: str):
        self.lex = Lexer(code)
        resultado = self.parseExpression()
        print(resultado)

if __name__ == '__main__':
    parser = Parser()
    parser.run(sys.argv[1])