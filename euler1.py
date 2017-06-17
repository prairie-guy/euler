def fibsUpTo(max):
    f2, f1  = 1, 0
    fibs = [1]
    sum = 0
    while f1 < max:
        if f1 % 2 == 0:
            sum = sum + f1
        fibs.append(f1)
        f2, f1  = f2 + f1, f2
    return sum

fibs = fibsUpTo(4000000)

        
def fib(n):    # write Fibonacci series up to n
    """Print a Fibonacci series up to n."""
    a, b = 0, 1
    while a < n:
        print a,
        a, b = b, a+b
