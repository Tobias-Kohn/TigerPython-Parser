def make_a():
    return open('a.txt', 'w')

def make_b():
    return open('b.txt', 'w')

# a single item in parens still behaves like a plain `with`
with (make_a()):
    pass

# PEP 617: parenthesized grouping of several context managers, each with its
# own `as` target
with (make_a() as a, make_b() as b):
    a.write('hi')
    b.write('there')

# grouping without any `as` at all
with (make_a(), make_b()):
    pass

# grouping with a mix of items that do and don't have their own `as`
with (make_a(), make_b() as b):
    b.write('hi')

# a trailing comma before the closing paren is allowed
with (make_a() as a, make_b() as b,):
    pass

# three grouped items
with (make_a() as a, make_b() as b, make_a() as c):
    pass

async def run():
    async with (make_a() as a, make_b() as b):
        pass

# the rare case: this is *not* grouping (none of the items has its own `as`,
# and there is a trailing `as` after the closing paren) - it is a single
# tuple-valued context manager, bound as a whole
with (make_a(), make_b()) as t:
    pass

# ordinary parentheses around a single non-trivial expression still work too
with (make_a() if True else make_b()):
    pass
