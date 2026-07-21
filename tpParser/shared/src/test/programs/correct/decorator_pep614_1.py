def identity(f):
    return f

def decorator_factory():
    return identity

buttons = [identity]
registry = {'widgets': identity}
flag = True

# ordinary cases (dotted name, call, dotted name + call) still work
@identity
def f1():
    pass

@decorator_factory()
def f2():
    pass

# PEP 614: any expression is allowed as a decorator, not just a dotted name
# with an optional single call

@buttons[0]
def f3():
    pass

@(identity if flag else decorator_factory())
def f4():
    pass

@registry['widgets']
class Widget:
    pass

# stacking decorators of different forms on the same definition
@identity
@buttons[0]
@(identity if flag else decorator_factory())
def f5():
    pass
