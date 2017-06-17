"""
euler104.jl
12/18/2016

The Fibonacci sequence is defined by the recurrence relation:

Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
It turns out that F541, which contains 113 digits,
is the first Fibonacci number for which the last nine digits are 1-9 pandigital
(contain all the digits 1 to 9, but not necessarily in order).
And F2749, which contains 575 digits, is the first Fibonacci number
for which the first nine digits are 1-9 pandigital.

Given that Fk is the first Fibonacci number for which the first nine digits AND
the last nine digits are 1-9 pandigital, find k.
"""

function euler104(max::Integer)
    (a, b) = (0, 1)
    for ith = 1:max
        (a, b)  = (a + (b % 10^9), a)
        if (a > 999999999) && lastpandigits(a) && firstpandigits(ith)
            return(ith)
        end
    end
    return "Not Found"
end

function lastpandigits(fib::Integer)
    last = lastdigits(fib, 9)
    return sort(last) == [1, 2, 3, 4, 5, 6, 7, 8, 9]
end

function firstpandigits(ith::Integer)
    fib = fibonacci(ith)
    first = firstdigits(fib, 9)
    return sort(first) == [1, 2, 3, 4, 5, 6, 7, 8, 9]
end

function firstdigits{T<:Integer}(n::T, d::Integer)
    len =  ceil(T, log10(n))
    return digits(div(n, 10^(len - d)))
end

function lastdigits{T<:Integer}(n::Integer, d::T)
    l = n % 10^d
    if l == 0
        return zeros(T, d)
    else
        return digits(l)
    end
end

"""Fast doubling Fibonacci algorithm"""
function fibonacci(n::Integer)
    function fibonacci2(n::Integer)
        if n == 0
            return (BigInt(0), BigInt(1))
        else
            a, b = fibonacci2(div(n, 2))
            c = a * (b * 2 - a)
            d = a * a + b * b
            if n % 2 == 0
                return (c, d)
            else
                return (d, c + d)
            end
        end
    end
    return fibonacci2(n)[1]
end


## @time euler104(350000)
## 0.666689 seconds (1.70 M allocations: 219.733 MB, 2.02% gc time)
## 329468

