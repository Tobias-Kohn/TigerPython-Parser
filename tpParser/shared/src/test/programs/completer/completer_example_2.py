# 204
# draw;extent;pos_x;pos_y
class Shape:

    def __init__(self, x, y):
        self.pos_x = x
        self.pos_y = y

    def draw(self):
        pass

    def extent(self):
        return (0, 0)

myshape = Shape(123, 456)
myshape.draw()
