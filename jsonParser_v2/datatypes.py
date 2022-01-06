from enum import Enum
from collections import namedtuple

class JSymb(Enum):
    ObjectStart = 1
    ObjectEnd = 2
    ArrayStart = 3
    ArrayEnd = 4

Value = namedtuple("Value", ["isString", "value"])