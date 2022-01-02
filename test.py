from jsonParser import parseFile, parseString, splitNonNested

from pprint import pprint

test = parseFile("test_inputs/test1.json")
pprint(test)