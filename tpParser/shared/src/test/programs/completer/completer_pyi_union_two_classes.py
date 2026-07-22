# pyi:pyi.uniontest2
# class Foo:
#     def bar(self) -> None: ...
# class Baz:
#     def qux(self) -> None: ...
# def make_foo_or_baz() -> Foo | Baz: ...
# 47
# bar;qux
from pyi.uniontest2 import *
make_foo_or_baz().