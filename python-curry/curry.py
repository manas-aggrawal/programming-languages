def foo(x , y):
    return x + y

def curry_foo2(x):
    return lambda y : x + y

def curry_foo3(x):
    return lambda y : lambda z : x + y + z

def curry_foo4(x):
    return lambda y : lambda z : lambda a:  x + y + z + a


print(foo(1, 2))
print(curry_foo2(1)(2))
print(curry_foo3(1)(2)(3))
print(curry_foo4(1)(2)(3)(4))
