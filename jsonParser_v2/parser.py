from .datatypes import JSymb, Value

def parseAll(lexedJSON: list):
    if lexedJSON == []: endWithError() # cant parse nothing i.e. non-closed bracket

    value = lexedJSON.pop(0)

    match value:
        case JSymb.ObjectStart:
            return parseObject(lexedJSON)
        case JSymb.ArrayStart:
            return parseArray(lexedJSON)
        case Value():
            return parseValue(value)
        case _:
            endWithError()

def parseObject(lexedJSON: list):
    outputDict = {}

    while True:
        match lexedJSON.pop(0):
            case JSymb.ObjectEnd: return outputDict
            case Value() as key if key.isString: 
                value = parseAll(lexedJSON)
                outputDict[key.value] = value
            case _: endWithError()

def parseArray(lexedJSON: list):
    outputList = []

    while True:
        if lexedJSON[0] == JSymb.ArrayEnd:
            lexedJSON.pop(0)
            return outputList
        else:
            outputList.append(
                parseAll(lexedJSON)
            )

def parseValue(jsonVal: Value):
    if jsonVal.isString:
        return jsonVal.value

    match jsonVal.value:
        case "true": return True
        case "false": return False
        case "null": return None
        case i if i.isdigit(): return int(i)
        case f: 
            try:
                return float(f)
            except:
                endWithError()

def endWithError():
    raise ValueError("JSON string is incorrectly formatted")