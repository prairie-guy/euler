"""
euler108.jl (SEE euler108.jl for better analysis)
1/1/2107

In the following equation x, y, and n are positive integers.

1/x + 1/y = 1/n

For n = 4 there are exactly three distinct solutions:

1/5 + 1/20 = 1/4
1/6 + 1/12 = 1/4
1/8 + 1/8  = 1/4

What is the least value of n for which the number of distinct solutions exceeds one-thousand?

NOTE: This problem is an easier version of Problem 110
"""
               
## Provides ++ shorthand for merging vectors and strings. Basically, vcat, hcat and * for strings
# using CBD.++di


##  Iterators Package  clobbers Standard Library version of Iterators
##  Standard Library: Base.Iterators
##  Package  Library: Main.Iterators or just Iterators
#import Iterators
import  IterTools
import Combinatorics
using Primes

function quick_search(n, max = 10000)
    return [[div(x*y , (x + y))  [x y]]
            for x in 1:max for  y in x:1000
            if x*y % (x + y) == 0 && div(x*y,(x + y)) == n]
end

function diophantine_reciprocals(n)
    n2 = n^2
    return [[n + k, n + div(n2, k)]  for k in n:-1:1 if n2 % k == 0 ]
end

"""
Number of divisors. Example: n_divisors(24) = 8, i.e.,  {1,2,3,4,6,8,12,24}
Divisors is equal to the cartesian product of the prime factors.
For example, 2^3*3^2*5  would be [2^0, 2^1 ,2^2, 2^3]*[3^0, 3^1, 3^2]*[5^0, 5^1]
d(n) = 4*3*2 = 24
"""
n_divisors(n) = prod(v+1 for (k, v) in factor(n))
n_proper_divisors(n) = n_divisors(n) - 1


## Idea is simply that divisors are maximized when primes are multiplied together rather than when they are composites.
## This is because the number of divisors is equal to the 
## Didn't bother with a wrapper. This was plug and chug
## length(diophantine_reciprocals(reduce(*,big(1),[  2  3  5  7  11  13 6 ])))
## 1013

## julia> reduce(*,big(1),[  2  3  5  7  11  13 6 ])
## 180180    

"""Number of diophantine_reciprocals for the pi product of primes up to n """
n_diophantine_reciprocals_of_ppp(n) = div((3^n + 1), 2)



