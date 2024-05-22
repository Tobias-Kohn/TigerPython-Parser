# Testing guards in patterns

def check_points(p):
    match p:
        case (0, 0):
            return "origin"
        case (x, y) if x == y or x == -y:
            return "diagonal"
        case (x, y) if x * y == 1:
            return "hyperbola"
        case (x, _) if x > 0:
            return "above"
