# Examples from PEP 701 (https://peps.python.org/pep-0701/)
things = ["egg", "spam", "bacon"]
things.append(f"{f"{f"infinite"}"}" + " " + f"{f"nesting!!!"}")
print(f"These are the things: {", ".join(things)}")

a = ["hello", "world"]
print(f"{'\n'.join(a)}")

print(f"{f"{f"{f"{f"{f"{1+1}"}"}"}"}"}")
