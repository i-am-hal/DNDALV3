#[
This holds the parser for the DNDAL language, this
will produce an AST of a program, which would get
fed into an interpreter. Of course, since the language
revolves around a REPL, any variables (etc.) will be
remembered with a 'wrapper' interpreter of sorts.
Author: Alastar Slater
Date: 8/9/2019
]#
import strutils, sequtils, tables, strformat, random

#[     TYPE DEFINITIONS     ]#
type
    #All of the tokens that can appear from the lexer
    TokenType = enum
        Eol      #Marks end of the input
        Lparen   #Marks start of parenthesis
        Rparen   #Marks end of parenthesis
        Lbracket #Marks start of brackets
        Rbracket #Marks end of brackets
        Integer  #An integer number, such as 0, 5, 10, -42
        String   #A bunch of text to be used in the language
        Word     #A single or sequence of characters
        Add      #Notes the addition of two values
        Sub      #Will subtract two values
        Mul      #Multiplies two values together
        Div      #Divides two values, returns integer result
        Mod      #Returns remainer of a division
        Dice     #Some form of dice roll
        Sum      #Will take the sum of a list
        Variable #Some variable in the language
        If       #For the start of an if statement
        Then     #For the second true-part of an if statement
    
    #A token is a pairing of token type and string value aswell as column pos.
    Token = tuple[tokType: TokenType, value: string, column: int]

    #Takes in text and spits back a series of tokens
    Lexer = tuple
        text: string     #The input text to be tokenized
        pos: int         #Current position in text
        chr: char        #Current character from text ("\n" stops lexer)
        toks: seq[Token] #All of the tokens processed thus far
        error: bool      #If the lexer should stop, is error

    #Top node for the AST
    Node = ref object of RootObj

    NoOp = ref object of Node #Does literally nothing

    StringVal = ref object of Node #Holds string
        token: Token
    
    IntegerVal = ref object of Node #Holds a number
        token: Token
    
    #A roll of a single die
    SingleRoll = ref object of Node #Rolls single dice
        sides, column: int

    MultiRoll = ref object of Node #Used for the rolling of dice with many sides
        times, sides, column: int

    #A binary operation like addition or subtraction
    BinOp = ref object of Node
        left, right: Node
        op: Token 
    
    #Used for unary operators, like negation
    UnaryOp = ref object of Node
        op: Token 
        node: Node
    
    #A list is a series of values in brackets
    List = ref object of Node
        values: seq[Node]
    
    #Takes the sum of a list
    SumList = ref object of Node
        column: int
        list: Node 
    
    EvalValType* = enum
        ENil    #No value, nothing
        EString
        EInt
        EList

    #Wrapper for values returned by the interpreter
    EvalVal = object
        case evalType*: EvalValType:
        of EString:
            estring: string
        of EInt:
            eint: int
        of EList:
            elist: seq[EValVal]
        of ENil:
            enil: string
    
    #Takes in text and generates an AST based off of it's tokens
    Parser* = tuple
        lexer: Lexer     #The lexer being used in generating the ast
        currToken: Token #Current token from the lexer
        error: bool      #If we have an error or not
        errorStr: string #The error itself
    
    #Checks types and all of the operations being enacted on types
    TypeChecker = tuple
        parser: Parser   #Parses the text
        ast: Node        #The abstract syntax tree
        error: bool      #If we have an error or not
        errorStr: string #The string for what the error is
    
    #This will interpret the line given from the user
    Interpreter* = tuple
        checker: TypeChecker  #Checks for any type errors
        ast: Node             #The AST
        error: bool           #If we have an error
        errorStr: string      #the error

#[     CONVERT OBJECTS TO STRINGS     ]#

#Forward declare function to turn node into string
proc `$`*(val: Node): string

#Return just the string in the string val
proc `$`(val: StringVal): string = "StringVal(" & val.token.value & ")"
#Converts integerval to string
proc `$`(val: IntegerVal): string = "IntegerVal(" & val.token.value & ")"
#Converts single roll to a string
proc `$`(val: SingleRoll): string = "1D" & ($val.sides)
#Converts multi roll to a string
proc `$`(val: MultiRoll): string = ($val.times) & "D" & ($val.sides)
#Converts binop to string
proc `$`(val: BinOp): string = "BinOp(" & ($val.left) & fmt" {val.op.value} " & ($val.right) & ")"
#Converts list into string
proc `$`(val: List): string =  $val.values

#Converts all of the node values
proc `$`*(val: Node): string =
    #Converts stringvals
    if val of StringVal:
        return $(StringVal val)
    
    #Converts string
    elif val of IntegerVal:
        return $(IntegerVal val)
    
    #Converts singlerolls into strings
    elif val of SingleRoll:
        return $(SingleRoll val)

    #Converts multiroll into string
    elif val of MultiRoll:
        return $(MultiRoll val)

    #Converts binary operation into string
    elif val of BinOp:
        return $(BinOp val)

    #Converts list into string
    elif val of List:
        return $(List val)
    
    else: #Converts regular node
        return "Node()"

#Converts EvalVal to string
proc `$`*(val: EvalVal): string =
    if val.evalType == EInt:
        return $val.eint

    elif val.evalType == EString:
        return val.estring 
    
    elif val.evalType == EList:
        var strings: seq[string] #All values's strings
        #Go through each value
        for elem in val.elist:
            strings.add $elem
        
        #Returns the string
        return "[" & strings.join(" ") & "]"

#Makes a new eval integer
func newEvalInt(val: int): EvalVal = EvalVal(evalType:EInt, eint: val)
#Makes a new eval string
func newEvalStr(val: string): EvalVal = EvalVal(evalType:EString, estring:val)
#Makes a new eval list
func newEvalList(val: seq[EvalVal]): EvalVal = EvalVal(evalType:EList, elist:val)
#Takes the value from EvalInt
func getEvalInt(val: EvalVal): int = val.eint
#Gets the value from EvalString
func getEvalStr(val: EvalVal): string = val.estring
#Gets the value from EList
func getEvalList(val: EvalVal): seq[EvalVal] = val.elist

#[     CONSTANTS     ]#

let reserved_words: Table[string, TokenType] = #[initTable[string, TokenType]()]# {
            "SUM": Sum #For taking the sum of a list
}.toTable


#[     LEXER METHODS     ]#

#Tells us if a character is whitespace or not
func isWhitespace(chr: char): bool = chr in ['\t', ' ']

#Returns true if this character is a digit
func isDigit(chr: char): bool = chr in '0' .. '9'

#Returns if alphabet letter
func isAlpha(chr: char): bool = chr.toUpperAscii() in 'A' .. 'Z'

#Creates a token
proc makeToken(tokType:TokenType, value:string, column=0): Token =
    (tokType:tokType, value:value, column:column)

#Makes an instance of the lexer
proc makeLexer(text:string, pos=0, toks=newSeq[Token](), error=false): Lexer =
    #The default, stops lexer
    var character = '\n'

    #if the pos is within the text, get the actual char.
    if pos < len(text): character = text[pos]

    #Return this lexer instance
    (text: text, pos: pos, chr:character, toks: toks, error:error)

#Simply another way to make a lexer for the first time
proc newLexer*(text:string): Lexer = makeLexer(text)

#Move foward by one character
proc advance(lexer:var Lexer) =
    lexer.pos += 1 #Move forward

    #If we have char there, get the new character
    if lexer.pos < len(lexer.text):
        lexer.chr = lexer.text[lexer.pos]
    
    else: #Otherwise, set to unknown
        lexer.chr = '\n'

#Returns an integer (string)
proc getInteger(lexer:var Lexer): string =
    var digits: seq[char] #All of the digits of the number
    #Go through and collect all of the digits
    while lexer.chr.isDigit():
        digits.add lexer.chr #Add the character
        lexer.advance() #Move to next character
    #Move past last character
    digits.join("") #Return resulting number

#Returns next character
proc peek(lexer: Lexer, peek=1): char =
    #Peek ahead to the next character
    if lexer.pos + peek < len(lexer.text):
        return lexer.text[lexer.pos + peek]
    else: #Otherwise, end line char
        return '\n'

#Gets a word from the the input stream
proc getWord(lexer:var Lexer): Token =
    var word: seq[char] #The word being made
    
    #Add all of the characters of this word
    while lexer.chr.isAlpha() or lexer.chr in '0' .. '9' or lexer.chr == '_':
        word.add lexer.chr.toUpperAscii() #Add the current char
        lexer.advance() #Move to next character
    
    #Save the full word
    var full_Word = word.join()

    #Return reserved word if we have one
    if full_Word in reserved_words:
        let wordType = reserved_words[full_Word]
        #Return the token for this reserved word
        return makeToken(wordType, full_Word, lexer.pos + 1)
    
    else: #Return variable token
        #Return the token for a variable
        return makeToken(Variable, full_Word, lexer.pos + 1)
        
#Returns a stirng of characters
proc getString(lexer:var Lexer): string =
    var letters: seq[char]
    while not (lexer.chr in ['\n', '"']):
        letters.add lexer.chr #add the character
        lexer.advance() 
    lexer.advance() 

    #Return the string 
    letters.join()

#Skip over a comment
proc skipComment(lexer:var Lexer) =
    while lexer.chr != '\n':
        lexer.advance()

#Returns the next token, with the new lexer to use
proc nextToken(lexer:var Lexer): Token =
    #While we havent hit
    while lexer.chr != '\n':
        #Skip over all of the whitespace
        if lexer.chr.isWhitespace():
            #Skip over the whitespace
            while lexer.chr.isWhitespace():
                lexer = makeLexer(lexer.text, lexer.pos + 1, lexer.toks, lexer.error)
        
        #Skip over all of the comment
        elif lexer.chr == ';':
            lexer.skipComment()
        
        #If this is the start of s stirng
        elif lexer.chr == '"':
            let start = lexer.pos + 1 #Start of string
            lexer.advance() #Skip start of stirng
            return makeToken(String, lexer.getString(), start)
        
        #Gets an integer value
        elif lexer.chr.isDigit():
            let pos = lexer.pos + 1 #Save the position
            #Return the integer
            return makeToken(Integer, lexer.getInteger(), pos)
        
        #If this is the rolling dice operator
        elif lexer.chr in ['D', 'd'] and not (lexer.peek().isAlpha()):
            lexer.advance()
            return makeToken(Dice, "D", lexer.pos)
        
        #Return the word after it is collected
        elif lexer.chr.isAlpha():
            return lexer.getWord()

        #Returns token for the open bracket
        elif lexer.chr == '[':
            lexer.advance()
            return makeToken(Lbracket, "[", lexer.pos)

        #Returns token for the closing bracket
        elif lexer.chr == ']':
            lexer.advance()
            return makeToken(Rbracket, "]", lexer.pos)
        
        #Returns token for the open parenthesis
        elif lexer.chr == '(':
            lexer.advance()
            return makeToken(Lparen, "(", lexer.pos)

        #Returns token for the closing parenthesis
        elif lexer.chr == ')':
            lexer.advance()
            return makeToken(Rparen, ")", lexer.pos)

        #For additing two values together
        elif lexer.chr == '+':
            lexer.advance()
            return makeToken(Add, "+", lexer.pos)

        #For subtracting two values together
        elif lexer.chr == '-':
            lexer.advance()
            return makeToken(Sub, "-", lexer.pos)

        #For multipying two values together
        elif lexer.chr == '*':
            lexer.advance()
            return makeToken(Mul, "*", lexer.pos)

        #For dividing two values
        elif lexer.chr == '/':
            lexer.advance()
            return makeToken(Div, "/", lexer.pos)

        #For taking mod of two values
        elif lexer.chr == '%':
            lexer.advance()
            return makeToken(Mod, "%", lexer.pos)
        
        else: #Otherwise, is error, only for unknown character in the lexer
            lexer.advance()
            lexer.error = true
            return makeToken(Eol, "EOL", lexer.pos - 1)

    #Otherwise, return eol (end of line) token
    makeToken(Eol, "EOL", lexer.pos + 1)

#[     PARSER METHODS     ]#

#Returns a new parser
proc newParser(text: string): Parser =
    var lexer = newLexer(text) #The lexer used by the parser
    let token = lexer.nextToken() #Get the first token
    #Returns the parser instance
    (lexer: lexer, currToken: token, error:false, errorStr:"")

#Removes current token
proc eat(parser:var Parser, tokType:TokenType, error="default") =
    #Gets the next token from the input
    if parser.currToken.tokType == tokType:
        parser.currToken = parser.lexer.nextToken()
    
    else: #Otherwise, raise an error
        parser.error = true #Note that we have an error

        #Make the default error
        if error == "default":
            #Makes default error string
            let str = fmt"""Syntax error at column {parser.currToken.column}
            Expected token {tokType} but got {parser.currToken.tokType}
            """
            parser.errorStr = str
        
        else: #Save the custom error given
            parser.errorStr = error

#Forward declares this proc
proc list(parser:var Parser): Node

#Forward declare expression for recursive definitions
proc expression(parser:var Parser): Node 

#A term is the multiplication, division or modulus of two nodes
#Some value in the language (dice roll, integer, list, string)
proc value(parser:var Parser): Node =
    if parser.error == true: return NoOp() #Exit if there is an error

    #Save current token type
    let tokType = parser.currToken.tokType

    #If the current node is a string
    if tokType == String:
        #Get the string and make the node for it
        let str = StringVal(token: parser.currToken)
        parser.eat(String) #Remove the string value
        return str #Return the string node
    
    #If this is the sum command
    elif tokType == Sum:
        #Save current column
        let column = parser.currToken.column
        parser.eat(Sum)

        #If there is a value to use, do get it
        if parser.currToken.tokType != Eol:
            #Get a value
            let value = parser.value()

            #Return the sum list
            return SumList(column: column, list: value)
        
        #Raises an error
        if parser.error == false:
            parser.error = true
            parser.errorStr = "Syntax Error, Sum command requires a value to take the sum of."

    #If this is a list, return the following list
    elif tokType == Lbracket:
        return parser.list()
    
    #Nested expression
    elif tokType == Lparen:
        parser.eat(Lparen)
        let node = parser.expression()
        #Remove the closing parenthesis
        parser.eat(Rparen, fmt"Syntax Error, mismatched parenthesis at column {parser.currToken.column}")
        #Quit if there is an error
        if parser.error == true: return NoOp()
        return node #Return the sub expression
    
    #If this is a dice roll or a number
    elif tokType == Integer:
        let numToken = parser.currToken #Save the token
        parser.eat(Integer) #Remove the number node

        #If this is a dice being rolled, return dice
        if parser.currToken.tokType == Dice:
            #Save the column position of the dice
            let columnPos = parser.currToken.column
            parser.eat(Dice) #Remove the dice
            let secondInt = parser.currToken #Save the other integer
            #Remove the second integer
            parser.eat(Integer, fmt"Syntax Error, integer expected after dice keyword 'D'")

            #Exit out if there is an error
            if parser.error == true: return NoOp()

            #If the first number is one, this is a single roll
            if parseInt(numToken.value) == 1:
                return SingleRoll(sides: parseInt(secondInt.value), column: columnPos)
            
            else: #Returns multi roll otherwise
                return MultiRoll(times: parseInt(numToken.value),
                    sides: parseInt(secondInt.value), column: columnPos)
        
        #Returns integer value
        return IntegerVal(token: numToken)
    
    #if there already isn't an error
    else:
        #Setup error if there already isn't one
        if parser.error == false:
            parser.error = true
            parser.errorStr = fmt"Syntax Error, unknown statement at column {parser.currToken.column}."
        
        return NoOp()

proc term(parser:var Parser): Node =
    #[ term: unit ((* | / | %) unit)* ]#
    if parser.error == true: return NoOp() #Exit if error

    #Get the value acted upon
    result = parser.value()

    while parser.currToken.tokType in [Mul, Div, Mod]:
        let op = parser.currToken #Save the token to get placement
        parser.eat(parser.currToken.tokType) #Remove operator
        #Reshape ast to allow for these operators
        result = BinOp(left: result, right: parser.value(), op: op)

#An expression is tha addition of two nodes
proc expression(parser:var Parser): Node =
    #[ expression: term ((+ | -) term)* ]#
    if parser.error == true: return NoOp() #Exit if error

    result = parser.term() #Get the term

    #Keep building AST while we have more addition/subtraction
    while parser.currToken.tokType in [Add, Sub]:
        let op = parser.currToken #Save what operator it is temp.
        parser.eat(parser.currToken.tokType) #Remove the operator
        #REshape the ast
        result = BinOp(left: result, right: parser.term(), op: op)

#A list is a sequence of node values (expressions)
proc list(parser:var Parser): Node =
    #[ list: '[' (value)* ']' ]#

    #Return noop if there is an error
    if parser.error == true: return NoOp()

    parser.eat(Lbracket) #Remove opening bracket
    var values: seq[Node] #All values in the list

    #Collect all values while not a right bracket or end of line
    while not (parser.currToken.tokType in [Rbracket, Eol]):
        values.add parser.expression()
    
    #Remove the closing bracket
    parser.eat(Rbracket, fmt"Syntax Error: Mismatched brackets at {parser.currToken.column}.")

    #Return noop if there is an error
    if parser.error == true: return NoOp()

    #Return the list of values
    List(values: values)

#A statement
proc statement(parser:var Parser): Node =
    #Return noop node
    if parser.currToken.tokType == Eol: return NoOp() 
    parser.expression() #Parse an expression otherwise

#[     TYPE CHECKER     ]#
# int = integer, str = string, lst = list, roll = single dice roll, nil = none

#Forward declares most general function
proc check(checker:var TypeChecker, node: Node): string

#Checks types for unary operator
proc check(checker:var TypeChecker, node: UnaryOp): string =
    #Return nil if there is already an error
    if checker.error == true: return "nil"

    #If this is an integer, no problem, return int
    if node.node of IntegerVal:
        return "integer"

    #Return list if this is a list
    elif node.node of List:
        return "list"
    
    else: #Sets up the error
        checker.error = true
        checker.errorStr = fmt"Type Error at {node.op.column}, only operates on lists or integers."

#Checks the binary operators
proc check(checker:var TypeChecker, node: BinOp): string =
    #If there is an error, return nil
    if checker.error == true: return "nil"

    #Gets the operator being used
    let
        operator = node.op.tokType           #Get the operator that is in use
        leftType = checker.check(node.left)  #Get type of left value
        rightType = checker.check(node.right) #Get the type of the right value
    
    #Check if this is the addition operator
    if operator == Add:
        #If the types are the same are one of these, return their type
        if leftType == rightType and leftType in ["list", "integer", "string", "roll"]:
            return leftType
        
        else: #Setup the error
            checker.error = true
            checker.errorStr = fmt"Type error, addition operator cannot add {leftType} and {rightType}."
            return "nil"
    
    #Check if this is subtraction
    elif operator == Sub:
        #Return integer because that is the only valid type
        if leftType == rightType and leftType in ["integer"]:
            return "integer"
        
        else: #Otherwise, raise error
            checker.error = true
            checker.errorStr = fmt"Type Error, cannot subtract {leftType} and {rightType}."
            return "nil"
    
    #Checks is this is multiplication
    elif operator == Mul:
        #Integers are only allowed, return integer
        if leftType == rightType and leftType in ["integer"]:
            return "integer"
        
        else: #Raise error
            checker.error = true
            checker.errorStr = fmt"Type Error, cannot multiply {leftType} and {rightType}."
            return "nil"
    
    #Checks division
    elif operator == Div:
        #Only allow integers
        if leftType == rightType and leftType in ["integer"]:
            return "integer"
        
        else: #Otherwise, raise error
            checker.error = true
            checker.errorStr = fmt"Type Error, cannot multiply {leftType} and {rightType}"
            return "nil"
    
    #Check arguments to the mod
    elif operator == Mod:
        #Only allow integers
        if leftType == rightType and leftType in ["integer"]:
            return "integer"
        
        else: #Otherwise, raise error
            checker.error = true
            checker.errorStr = fmt"Type Error, cacnnot take mod of {leftType} and {rightType}"
            return "nil"

#Checks if a multiroll is valid
proc check(checker:var TypeChecker, node: MultiRoll): string =
    #If the number of sides is 0, raise an error
    if node.sides == 0:
        #Raise an error
        if checker.error == false:
            checker.error = true #Mark that we have error
            #Tells the user what the problem is
            checker.errorStr = fmt"Impossible to roll {node.times}d0s."

        return "nil"
    
    return "list"

#Checks a single roll
proc check(checker:var Typechecker, node:SingleRoll): string =
    #Trying to roll 0 sided die, raise error
    if node.sides == 0:
        #Raise an error
        if checker.error == false:
            checker.error = true #Mark the error
            #Tell the user what the issue is
            checker.errorStr = "Impossible to roll a d0."

        return "nil"
    
    return "integer"

#Most general check, will call the others
proc check(checker:var TypeChecker, node: Node): string =
    #For simple values, return their type
    if node of SingleRoll:
        return checker.check(SingleRoll node)

    #If this is a multiroll, check if it is valid
    elif node of MultiRoll:
        return checker.check(MultiRoll node)

    #If this is a list, just return the list
    elif node of List:
        return "list"
    #Return string if this is indeed a string
    elif node of StringVal:
        return "string"
    #Return integer type
    elif node of IntegerVal:
        return "integer"
    
    #Check the sum of a list
    elif node of SumList:
        #If these values are fine, return an integer
        if (SumList node).list of MultiRoll or (SumList node).list of List:
            #Raise error if the given value doesn't produce a list
            if checker.check((SumList node).list) != "list":
                #Setup an error
                if checker.error == false:
                    checker.error = true
                    checker.errorStr = "Type Error, Sum requires a Multi Roll or valid List."

                return "nil"
            
            #Otherwise, return integer
            return "integer"

    #Make sure list has only things that generate numbers
    elif node of List:
        var onlyInt = true
        #Go through each value
        for val in (List node).values:
            #if this value doesn't produce an integer, return false
            if checker.check(val) != "integer":
                onlyInt = false
        
        #Raise error if the list contains more than just integers
        if onlyInt == false:
            #If there isn't an error already, set the error
            if checker.error == false:
                checker.error = true
                checker.errorStr = "Type Error, list can only have values that are/become integers."
            
            return "nil"
        
        return "list"
    
    #Check the unary operation
    elif node of UnaryOp:
        return checker.check(UnaryOp node)
    
    #Check the binary operation
    elif node of BinOp:
        return checker.check(BinOp node)

#Starts checking all of the notes
proc check*(checker:var TypeChecker) =
    discard checker.check(checker.ast)

#Makes a new type checker
proc newTypeChecker*(text: string): TypeChecker =
    var parser = newParser(text)
    let tree = parser.statement() #Get the AST
    #Return the type checker
    (parser: parser, ast: tree, error: parser.error, errorStr: parser.errorStr)

#[     INTERPRETER     ]#

#Evaluates an ast
proc eval*(node: Node): EvalVal =
    #If this is a noop node
    if node of NoOp:
        return EvalVal(evalType: ENil, enil:"\n")

    #Return the raw integer value
    elif node of IntegerVal:
        return newEvalInt(parseInt((IntegerVal node).token.value))
    
    #Return the raw string value
    elif node of StringVal:
        return newEvalStr((StringVal node).token.value)
    
    #Takes the sum of all of the values in the list
    elif node of SumList:
        var sum = 0
        #Add each element together
        for value in getEvalList(eval (SumList node).list):
            sum += getEvalInt(value)
        
        #Return the sum of the list
        return newEvalInt(sum)

    #Return the raw list value
    elif node of List:
        var values: seq[EvalVal] #All of the values in the list
        #Evaluate then add each value
        for val in (List node).values:
            values.add(eval val)
        #Return eval-val list
        return newEvalList(values)
    
    #Rolls a sinle die
    elif node of SingleRoll:
        #Just return 1
        if (SingleRoll node).sides <= 1: return newEvalInt(1)
        #Actually roll dice if sides > 1
        let roll = (rand(1..  (SingleRoll node).sides))

        #TEll user that they had a natural (max number)
        if roll == (SingleRoll node).sides:
            echo fmt"Natural {(SingleRoll node).sides}!"

        #Return the roll
        return newEvalInt(roll)
    
    #If this is a list of rolls, perform them
    elif node of MultiRoll:
        var rolls: seq[EvalVal] #All of the rolls
        #Roll the dice n times
        for _ in 1 .. (MultiRoll node).times:
            #Roll the die and add it
            rolls.add(newEvalInt(rand(1 .. (MultiRoll node).sides)))
        
        #return list of rolls
        return newEvalList(rolls)
    
    #Binary operation, does the math
    elif node of BinOp:
        let
            #get the operator being used
            operator = (BinOp node).op.tokType
            #Get the left value
            left = eval (BinOp node).left
            #Get the right value
            right = eval (BinOp node).right
        
        #Adds togethers two values
        if operator == Add:
            #If we are adding two strings, do so
            if  left.evalType == EString:
                return newEvalStr(getEvalStr(left) & getEvalStr(right))
            
            #Adds together the two integers
            elif left.evalType == EInt:
                return newEvalInt(getEvalInt(left) + getEvalInt(right))
            
            else: #Otherwise, it is a list
                var leftVals = getEvalList(left)
                #Add each value to the left list
                for val in getEvalList(right):
                    leftVals.add val
                
                #Return the joined list
                return newEvalList(leftVals)
        
        #Subtracts two integers
        elif operator == Sub:
            return newEvalInt(getEvalInt(left) - getEvalInt(right))
        
        #Multiply two values
        elif operator == Mul:
            return newEvalInt(getEvalInt(left) * getEvalInt(right))
        
        #Divide two values
        elif operator == Div:
            #Avoid zero division, return 0
            if getEvalInt(right) == 0: return newEvalInt(0)
            #Divides the two values
            return newEvalInt(int(getEvalInt(left) / getEvalInt(right)))
        
        #Takes remainder of division
        elif operator == Mod:
            #Avoid zero division
            if getEvalInt(right) == 0: return newEvalInt(0)
            #Takes mod of the two values
            return newEvalInt(getEvalInt(left) mod getEvalInt(right))
    


