# pyi:pyi.generictest2
# class Foo:
#     def bar(self) -> None: ...
# class Baz:
#     def qux(self) -> None: ...
# def make_foo_list() -> list[Foo]: ...
# 50
# bar
from pyi.generictest2 import *
make_foo_list()[0].