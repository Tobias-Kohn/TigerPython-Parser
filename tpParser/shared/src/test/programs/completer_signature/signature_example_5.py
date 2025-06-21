# 106
# !self, name : str, /, tags : list[str], **attrs : dict[str, tuple[str, int]]
class Foo:
    def __init__(self, name : str, /, tags: list[str], **attrs: tuple[str, int]):
        pass
Foo()
