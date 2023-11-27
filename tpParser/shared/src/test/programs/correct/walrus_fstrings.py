# An example taken from the official Python 3.8 documentation (What's New in Python 3.8)
def spam(a):
    if (n := len(a)) > 10:
        print(f"List is too long ({n} elements, expected <= 10)")
    do_some_proccessing(a)
