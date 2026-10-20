# Testing rest patterns: star in sequence patterns, double-star in mapping patterns

def describe(value):
    match value:
        case [x, y, *rest]:
            return "sequence", x, y, rest
        case {"name": name, **extra}:
            return "mapping", name, extra
        case {"id": ident, "tag": tag, **others}:
            return "mapping2", ident, tag, others
        case {**everything}:
            return "only rest", everything
        case _:
            return "other"
