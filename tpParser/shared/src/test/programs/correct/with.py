a = "test"
print(a)
with a:
    pass
with a as b:
    pass
with open('output.txt', 'w') as f:
    f.write('Hi there!')
