# 326
# area;perimeter
class Rect:
   def __init__(self):
       pass
   def area(self):
       pass
   def perimeter(self):
       pass
class Shape:
    def __init__(self, x, y):
        self.pos_x = x
        self.pos_y = y

    def draw(self):
        pass

    def bounds(self):
        return Rect()

myshape = Shape(123, 456)
myshape.bounds().area()
