from .datatypes import JSymb, Value

symbolMap = {
    "{": JSymb.ObjectStart,
    "}": JSymb.ObjectEnd,
    "[": JSymb.ArrayStart,
    "]": JSymb.ArrayEnd,
}

def lex(jsonString: str):
    lexed = []
    currentVal = ""

    inString = False

    for char in jsonString:
        # if currently in a string, either finish the string if the char is "
        # or add the char to the current string
        # and do not go on to pattern match the character (instead continue to next loop)
        if inString:
            if char == '"':
                inString = False
                lexed.append(
                    Value(True, currentVal)
                )
                currentVal = ""
            else:
                currentVal += char
            continue
        
        # otherwise, pattern match the char based on whether it starts an object/array
        # or if it ends a current value, or if it starts a string, or if it
        # is part of a non-string value (i.e. is not a whitespace character)
        match char:
            case "{" | "[" as startChar:
                lexed.append(
                    symbolMap[startChar]
                )
            case "}" | "]" as endChar:
                if currentVal:
                    lexed.append(
                        Value(False, currentVal)
                    )
                    currentVal = ""
                lexed.append(
                    symbolMap[endChar]
                )
            case "," | ":": #these chars flush
                if currentVal:
                    lexed.append(
                        Value(False, currentVal)
                    )
                    currentVal = ""
            case '"': inString = True
            case c if not c.isspace(): currentVal += c

    return lexed