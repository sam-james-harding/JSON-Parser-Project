from pprint import pprint

from jsonParser_v1 import parseString
from jsonParser_v2 import lex, parse

testInput = open("test_input.json").read()
test = lex(testInput)
test2 = parse(test)

pprint(test2)
print()
pprint(parseString(testInput))