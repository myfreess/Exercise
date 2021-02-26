cons = lambda x, y: lambda f: f(x,y)

car = lambda p: p(lambda x, y: x)

cdr = lambda p: p(lambda x, y: y)

nil = None

is_nil = lambda x: x is nil

partial_map = lambda f, x: x if is_nil(x) else cons(car(x),f(cdr(x)))


ana = lambda coalg: lambda x: partial_map(ana(coalg),coalg(x))

format_l = lambda l: "|" + ("nil|" if is_nil(l) else str(car(l)) + format_l(cdr(l)))

def my_range(n):
    return ana(lambda x: nil if x == n else cons(x,x+1))(0)
    
    
