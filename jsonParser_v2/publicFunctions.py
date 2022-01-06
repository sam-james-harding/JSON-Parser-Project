from .datatypes import JSymb
from .parser import parseAll, endWithError
from .lexer import lex

def parseString(jsonString: str):
    lexedJSON = lex(jsonString)

    firstVal: JSymb = lexedJSON[0]

    if firstVal in (JSymb.ObjectStart, JSymb.ArrayStart):
        return parseAll(lexedJSON)
    else:
        endWithError()
    
def parseFile(filename: str):
    with open(filename, "r") as jsonFile:
        rawContents = jsonFile.read()
        return parseString(rawContents)