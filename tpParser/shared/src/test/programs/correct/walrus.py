# An example taken from the official Python 3.8 documentation (What's New in Python 3.8)
discount = 0.0
if (mo := re.search(r'(\d+)% discount', advertisement)):
    discount = float(mo.group(1)) / 100.0
