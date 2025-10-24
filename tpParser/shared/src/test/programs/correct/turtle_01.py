import turtle
# import winsound

# Window properties
win = turtle.Screen()
win.title('Pong')
win.bgcolor('green')
# win.setup(width=800,height=600)
win.tracer(0)

def drawField():
	draw = turtle.Turtle()
	draw.penup()
	draw.speed(0)
	draw.color('white')
	draw.hideturtle()
	draw.goto(-390,295)
	draw.pendown()
	for i in range(2):
		draw.forward(770)
		draw.right(90)

