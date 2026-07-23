def risky():
    raise ExceptionGroup('group', [ValueError('a'), TypeError('b')])

# ordinary except still works
try:
    pass
except ValueError:
    pass

# except* (PEP 654), no name binding
try:
    risky()
except* ValueError:
    print('caught a ValueError subgroup')

# except* with a name binding
try:
    risky()
except* ValueError as e:
    print(e)

# multiple except* handlers on the same try
try:
    risky()
except* ValueError:
    pass
except* TypeError as e:
    print(e)

# except* with a tuple of exception types
try:
    risky()
except* (ValueError, TypeError) as e:
    print(e)

# except* combined with else/finally
try:
    risky()
except* ValueError:
    pass
else:
    print('no exception')
finally:
    print('done')
