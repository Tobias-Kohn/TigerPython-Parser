# 260
# only_p
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

class C(A, B):
    def go(self):
        super().item.
