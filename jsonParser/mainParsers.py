from .helperFunctions import JSONType, preprocess, splitNonNested

def parseAny(jsonString: str):
    jt = JSONType #type alias

    jsonText, jsonType = preprocess(jsonString)

    match jsonType:
        case jt.OBJECT: return parseObject(jsonText)
        case jt.ARRAY: return parseArray(jsonText)
        case jt.STRING: return jsonText
        case jt.VALUE: return parseValue(jsonText)

def parseObject(objectStr: str):
    lines = splitNonNested(objectStr, ",")

    resultDict = {}
    
    for line in lines:
        key, value = splitNonNested(line, ":")

        key, value = key.strip(), value.strip()

        processedKey = parseStr(key.strip())
        processedValue = parseAny(value.strip())

        resultDict[processedKey] = processedValue

    return resultDict

def parseArray(arrayStr: str):
    lines = splitNonNested(arrayStr, ",")
    processedLines = list(map(parseAny, lines))
    return processedLines

def parseStr(stringStr: str):
    return stringStr.strip('"')

def parseValue(valStr: str):
    match valStr:
        case "true": 
            return True
        case "false": 
            return False
        case "null": 
            return None
    
    try: #if value is an int
        return int(valStr)
    except ValueError:
        try: #if value is a float
            return float(valStr)
        except ValueError:
            raise ValueError(f"Invalid type in JSON: {valStr}")