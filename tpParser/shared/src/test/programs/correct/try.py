# Making sure there a valid 'try' structure code does not trigger errors
try:
    print("in try")
except:
    print("in except")

try:
    print("in try")
finally:
    print("in finally")

try:
    print("in try")
except:
    print("in except")
finally:
    print("in finally")

try:
    print("in try")
except:
    print("in except")
else:
    print("in else")

try:
    print("in try")
except:
    print("in except")
else:
    print("in else")
finally:
    print("in finally")
