from pprint import pprint
from timeit import timeit
from textwrap import dedent

import json

import jsonParser_v1 as json1
import jsonParser_v2 as json2

FILENAME = "test_input.json"

def outputTest():
    parsedBenchmark = json.load(open(FILENAME))

    parsed1 = json1.parseFile(FILENAME)
    parsed2 = json2.parseFile(FILENAME)

    print("Correct Output")
    pprint(parsedBenchmark)
    print()

    parser1_result = "correct" if parsed1 == parsedBenchmark else "incorrect"
    print(f"Parser 1 produced the {parser1_result} output")

    parser2_result = "correct" if parsed2 == parsedBenchmark else "incorrect"
    print(f"Parser 2 produced the {parser2_result} output")

def timeTest():
    NUM_TESTS = 1000
    rawInput = open(FILENAME).read()

    benchmark_time = timeit(lambda: json.loads(rawInput), number=NUM_TESTS)

    parser1_time = timeit(lambda: json1.parseString(rawInput), number=NUM_TESTS)
    parser2_time = timeit(lambda: json2.parseString(rawInput), number=NUM_TESTS)

    def formatTime(name, seconds: float):
        return dedent(f"""\
            {name} took {seconds} seconds to complete {NUM_TESTS}
            repetitions, or {seconds/NUM_TESTS} seconds per repetition.""")

    print(formatTime("JSON Parser v1", parser1_time), end="\n\n")
    print(formatTime("JSON Parser v2", parser2_time), end="\n\n")
    print(formatTime("stdlib JSON parser", benchmark_time), end="\n\n")

timeTest()
outputTest()