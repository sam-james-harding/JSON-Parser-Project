from .mainParsers import parseObject, parseArray, parseStr, parseValue, parseAny

def parseFile(filename: str):
    with open(filename, "r") as jsonFile:
        rawContents = jsonFile.read()
        return parseString(rawContents)

def parseString(jsonString: str):
    return parseAny(jsonString)