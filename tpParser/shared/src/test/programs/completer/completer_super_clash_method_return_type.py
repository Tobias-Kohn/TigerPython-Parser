# 244
# only_p
class P:
    def only_p(self):
        pass

class Q:
    def only_q(self):
        pass

class A:
    def make(self):
        return P()

class B:
    def make(self):
        return Q()

class C(A, B):
    def go(self):
        super().make().
