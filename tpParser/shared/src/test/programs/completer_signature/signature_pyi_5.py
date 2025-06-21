# pyi:signature.pyi.ex5
# class Foo:
#     def __init__(self, name : str, /, tags: list[str], **attrs: tuple[str, int]): ...
# 32
# self, name : str, /, tags : list[str], **attrs : dict[str, tuple[str, int]]
from signature.pyi.ex5 import *
Foo()
