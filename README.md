# JSON-Parser-Project
This is a personal project of mine, where I am making a basic, and probably inefficient and terrible, JSON parser. Obviously not intended for actual use, just as a learning exercise.

The first two versions (jsonParser_v1 and jsonParser_v2) were programmed in Python. v1 was based solely on my first thoughts on how to write a simple JSON parser. v2, however, was written with the concept of lexers and parsers in mind after some light research, and has shown to be more readable and faster. Tests for the Python versions are conducted in test.py.

Recently, I also learnt about combinatorial parsing, and decided to try to implement my own as another learning exercise. It is written in Haskell, with the Parsing module adapted from the module used in [this Computerphile video](https://www.youtube.com/watch?v=dDtZLm7HIJs), written by Professor Graham Hutton of the University of Nottingham. My reimplementation (using the Maybe monad) and the required research in functors, applicative functors, monads and alternative functors to understand it has helped me gain a better understanding of functional programing as a whole.

Note: AFAIK, these are complete parsers except that none of them support escape characters in strings (yet)