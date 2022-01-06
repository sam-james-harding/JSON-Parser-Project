from .datatypes import JSymb, Value

def parse(lexedJSON: list):
    firstVal: JSymb = lexedJSON.pop(0) #get and remove first item from list

    if firstVal == JSymb.ObjectStart:
        return parseObject(lexedJSON)
    elif firstVal == JSymb.ArrayStart:
        return parseArray(lexedJSON)
    else:
        endWithError()

def parseObject(lexedJSON: list):
    outputDict = {}

    def addDictItem(key: Value):
        valueSymb = lexedJSON.pop(0)

        if valueSymb == JSymb.ObjectStart:
            value = parseObject(lexedJSON)
        elif valueSymb == JSymb.ArrayStart:
            value = parseArray(lexedJSON)
        elif isinstance(valueSymb, Value):
            value = parseValue(valueSymb)
        else: endWithError()

        if key.isString:
            outputDict[key.value] = value
        else: endWithError()

    while True:
        currentVal = lexedJSON.pop(0)

        match currentVal:
            case JSymb.ObjectEnd: return outputDict
            case Value(): addDictItem(currentVal)
            case _: endWithError()

def parseArray(lexedJSON: list):
    outputList = []

    while True:
        currentVal = lexedJSON.pop(0)

        match currentVal:
            case JSymb.ArrayEnd: return outputList
            case JSymb.ObjectStart: 
                outputList.append(
                    parseObject(lexedJSON)
                )
            case JSymb.ArrayStart:
                outputList.append(
                    parseArray(lexedJSON)
                )
            case Value():
                outputList.append(
                    parseValue(currentVal)
                )
            case _: endWithError()

def parseValue(jsonVal: Value):
    if jsonVal.isString:
        return jsonVal.value

    match jsonVal.value:
        case "true": return True
        case "false": return False
        case "null": return None
        case i if i.isdigit(): return int(i)
        case f if float(f): return float(f)
        case _: endWithError()

def endWithError():
    raise ValueError("JSON string is incorrectly formatted")