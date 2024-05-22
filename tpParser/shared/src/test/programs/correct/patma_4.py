# "Real-world" example taken from https://github.com/gvanrossum/patma/blob/master/examples/expr.py
# Wo are only interested in the pattern matching parts here

def eval_expr(expr):
    """Evaluate an expression and return the result."""
    match expr:
        case BinaryOp('+', left, right):
            return eval_expr(left) + eval_expr(right)
        case BinaryOp('-', left, right):
            return eval_expr(left) - eval_expr(right)
        case BinaryOp('*', left, right):
            return eval_expr(left) * eval_expr(right)
        case BinaryOp('/', left, right):
            return eval_expr(left) / eval_expr(right)
        case UnaryOp('+', arg):
            return eval_expr(arg)
        case UnaryOp('-', arg):
            return -eval_expr(arg)
        case VarExpr(name):
            raise ValueError(f"Unknown value of: {name}")
        case float() | int():
            return expr
        case _:
            raise ValueError(f"Invalid expression value: {repr(expr)}")

def simplify_expr(expr):
    """Simplify an expression by folding constants and removing identities."""
    match expr:
        case BinaryOp(op, left, right):
            left = simplify_expr(left)
            right = simplify_expr(right)
            match (op, left, right):
                case [_, float() | int(), float() | int()]:
                    return eval_expr(BinaryOp(op, left, right))
                case ['+', 0, _] | ['*', 1, _]:
                    return right
                case ['+' | '-', _, 0] | ['*' | '/', _, 1]:
                    return left
                case ['-', 0, _]:
                    return UnaryOp('-', right)
                case ['*', 0, _] | ['*', _, 0]:
                    return 0
                case _:
                    return BinaryOp(op, left, right)
        case UnaryOp(op, arg):
            arg = simplify_expr(arg)
            match (op, arg):
                case ['+', _]:
                    return arg
                case ['-', UnaryOp('-', arg2)]:
                    return arg2
                case ['-', float() | int()]:
                    return -arg
                case _:
                    return UnaryOp(op, arg)
        case VarExpr(name):
            return expr
        case float() | int():
            return expr
        case _:
            raise ValueError(f"Invalid expression value: {repr(expr)}")
