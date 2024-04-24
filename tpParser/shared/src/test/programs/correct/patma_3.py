# This example tests whether 'case' and 'match' can still be used as identifiers
import re

def case(m):
    print(m)

match = re.match(r"(\w+) (\w+)", "Isaac Newton, physicist")
case(match)
match[1234]
