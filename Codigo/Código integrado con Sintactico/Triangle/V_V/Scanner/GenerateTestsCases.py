import random

def getCharLiteral():
    matrix= [['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z',],
    ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z',],
    ['0','1','2','3','4','5','6','7','8','9'],
    [' ','\t'],
    ['.',':',';',',','~','(',')','[',']','{','}','_','|','!','\\','`','"','#','$']]

    result = "'"

    row = (random.randint(0, len(matrix)-1))
    column = (random.randint(0, len(matrix[row])-1))
    result += matrix [row][column]

    result += "'"
    return result

def getIntLiteral():
    matrix= [['0','1','2','3','4','5','6','7','8','9']]

    result = ""
    
    for i in range (random.randint(10, 20)):
        row = (random.randint(0, len(matrix)-1))
        column = (random.randint(0, len(matrix[row])-1))
        result += matrix [row][column]

    return result


def getIdentifier():
    matrix= [['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z',],
    ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z',],
    ['0','1','2','3','4','5','6','7','8','9']]

    row = (random.randint(0, 1))
    column = (random.randint(0, len(matrix[row])-1))
    
    result = matrix [row][column]

    for i in range (random.randint(10, 20)):
        row = (random.randint(0, len(matrix)-1))
        column = (random.randint(0, len(matrix[row])-1))
        result += matrix [row][column]

    return result




tokens = ["array",
    	  "begin",
    	  "const",
    	  "do", 
    	  "else", 
    	  "end", 
    	  "func", 
    	  "if", 
    	  "in", 
    	  "let", 
    	  "of", 
    	  "proc",
    	  "record",
    	  "then", 
    	  "type", 
    	  "var", 
    	  "while",
    	  ".", 
    	  ":", 
    	  ";", 
    	  ",", 
    	  ":=", 
    	  "~", 
    	  "(",
    	  ")", 
    	  "[",
    	  "]", 
    	  "{",
    	  "}",
          "+",
          "-",
          "*",
          "/",
          "=",
          "<",
          ">",
          "\\",
          "&",
          "@",
          "%",
          "^",
          "?",
          getCharLiteral,
          getIntLiteral,
          getIdentifier]


def getData(a):
    if type(a) is str:
        return a
    return a()

def getName(b):
    if b == getCharLiteral:
        return "CharLiteral"
    elif b == getIntLiteral:
        return "IntLiteral"
    elif b == getIdentifier:
        return "Indentifier"
    elif b == "*":
        return "asterisk"
    elif b == "/":
        return "backSlash"
    elif b == "<":
        return "lessThan"
    elif b == ">":
        return "moreThan"
    elif b == "\\":
        return "slash"
    elif b == "?":
        return "questionMark"
    elif b == ":":
        return "colon"
    elif b == ":=":
        return "becomes"
    else:
        return b
    

for i in tokens:
    f=open("Test("+getName(i)+").tri","w")
    f.write("! Test of "+ getName(i) + " with 3 units\n")
    f.write( getData(i)+ getData(i) + getData(i) + "\n\n")

    for j in tokens:

        f.write("! Test of " + getName(i) + " with " + getName(j) + "\n")
        f.write(getData(j)+" "+getData(i)+ "\n")
        f.write(getData(j)+getData(i)+ "\n")
        f.write(getData(j)+getData(i)+getData(j)+ "\n")
        f.write(getData(j)+" "+getData(i)+" "+getData(i)+" "+getData(i)+" "+getData(j)+"\n\n")
    f.close()
