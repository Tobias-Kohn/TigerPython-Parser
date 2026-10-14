# 260
# only_q
class P:
    def only_p(self):
        pass

class Q:
    def only_q(self):
        pass

class A:
    def __init__(self):
        self.item = P()

class B:
    def __init__(self):
        self.item = Q()

class C(B, A):
    def go(self):
        super().item.
