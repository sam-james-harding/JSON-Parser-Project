from enum import Enum

class JSONType(Enum):
    OBJECT = 1
    ARRAY = 2
    STRING = 3
    VALUE = 4

def preprocess(jsonText: str) -> tuple[str, JSONType]:
    jsonText = jsonText.strip()

    stripBrackets = lambda text: text[1:-1].strip() #take away two outer chars and strip inside

    match (jsonText[0], jsonText[-1]):
        case "{", "}":
            return (stripBrackets(jsonText), JSONType.OBJECT)
        case "[", "]":
            return (stripBrackets(jsonText), JSONType.ARRAY)
        case '"', '"':
            return (jsonText.strip('"'), JSONType.STRING)
        case _:
            return (jsonText, JSONType.VALUE)

def splitNonNested(jsonString: str, splitStr: str) -> list[str]:
    splitResults = []

    prevSplitPoint = 0
    currentIdx = 0 

    nestingLevel = 0
    inString = False

    while currentIdx < len(jsonString):
        currentChar = jsonString[currentIdx]

        if currentChar == splitStr and nestingLevel == 0 and not inString:
            #slice new section, and set current index to position of the new previous split
            newSection = jsonString[prevSplitPoint:currentIdx]
            prevSplitPoint = currentIdx+1
            splitResults.append(newSection)

        elif currentChar == '"':
            inString = not inString

        elif currentChar in ("{", "["): 
            nestingLevel += 1

        elif currentChar in ("}", "]"):
            nestingLevel -= 1

        currentIdx += 1

    finalSection = jsonString[prevSplitPoint:]
    splitResults.append(finalSection)

    return splitResults