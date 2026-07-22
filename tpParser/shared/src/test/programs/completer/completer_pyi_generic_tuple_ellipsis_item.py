# pyi:pyi.generictest6
# class Foo:
#     def bar(self) -> None: ...
# class Baz:
#     def qux(self) -> None: ...
# def make_foo_tuple_ellipsis() -> tuple[Foo, ...]: ...
# 60
# bar
from pyi.generictest6 import *
make_foo_tuple_ellipsis()[0].