"""
euler110.jl 
1/6/2107

In the following equation x, y, and n are positive integers.

1/x + 1/y = 1/n

For n = 4 there are exactly three distinct solutions:

1/5 + 1/20 = 1/4
1/6 + 1/12 = 1/4
1/8 + 1/8  = 1/4

It can be verified that when n = 1260 there are 113 distinct solutions and this is the least value
of n for which the total number of distinct solutions exceeds one hundred.
What is the least value of n for which the number of distinct solutions exceeds four million?
"""

#using CBD.++          ## Shorthand for merging vectors and strings.
using Combinatorics
using Primes
using Transducers
"""
Pattern Hunting::

diophantine_reciprocals(n) generates (x,y) pairs per n, which we then count. We really don't need the (x,y) pairs; we just
need the number of (x,y) pairs as a function of n. To generate the diophantine_reciprocals(n), we need to determine if
i = {1,2...,n} divides n^2.

Is there a better way to find the number of diophantine reciprocals? Let's look for patterns.

From euler108 we know that the prime factorization of n is central to the solution, i.e.,
n = p_1^a_1 * p_2^a_2 *...* p_n^a_n.

Moreover, the number of divisors is is also related to the prime factorization of n, i.e.,
d(n) = (a_1+1) * (a_2+1) *...* (a_n+1).

In other words, d(n), the number of divisors of n, is solely a function of the product of the exponents;
the prime factors do not matter.

For example, the prime factorization of 48 is: 2^4 * 3^1 (sum of the exponents is 5) and the
number of divisors are (4+1)*(2+1) = 8. Finally, the number of diophantine reciprocals of 48 is 14.

We will look for a relationship between the number of divisors and the number of diophantine reciprocals.

"""


"""
Calculate the number of divisors of n. Usage: n_divisors(24) -> 8, i.e., {1,2,3,4,6,8,12,24}
Divisors are the the Cartesian product of the prime factors. For example, 2^3*3^2*5 = 360  would be:
[2^0, 2^1 ,2^2, 2^3] * [3^0, 3^1, 3^2] * [5^0, 5^1] and n_divisors(360) = 4*3*2 = 24
"""
n_divisors(n) = prod(v+big(1) for (k, v) in factor(n))
n_proper_divisors(n) = n_divisors(n) - 1

"""ppp(n) means pi_product_of_primes_upto(n), i.e., ppp(5) = 2*3*5*7*11"""
primes_local = primes(10000);
ppp(n) = reduce(*, primes_local[1:n],init = big(1))


"""Generate diophantine_reciprocals of n. Developed in euler108.jl"""
function diophantine_reciprocals(n)
    n2 = n^2
    return [[n + k, n + div(n2, k)]  for k in n:-1:1 if n2 % k == 0 ]
end

""" Look for a relationship between n_divisors(i^2) and diophantine_reciprocals"""
function pattern_hunting(n)
    for i = 2:n
        println("$i, $(n_divisors(i)), $(n_divisors(i^2)), $(length(diophantine_reciprocals(i)))")
    end
end

## pattern_hunting(12)
## 2, 2, 3, 2
## 3, 2, 3, 2
## 4, 3, 5, 3
## 5, 2, 3, 2
## 6, 4, 9, 5
## 7, 2, 3, 2
## 8, 4, 7, 4
## 9, 3, 5, 3
## 10, 4, 9, 5
## 11, 2, 3, 2
## 12, 6, 15, 8


"""
Looks like a relationship between in the last two columns, i.e., n_divisors(i^2) and n_diophantine_reciprocals(i)!!!
Very simple: n_diophantine_reciprocals(n) = (n_divisors(n^2) + 1)/2
This is the magic! We now have a very efficient method of determining the number of diophantine_reciprocals.
O(n) is determined by the factor(n). Though in general this may be an issue for arbitrarily large numbers, but
in these problems, we are looking for minimums, which in general start with a ppp(n), which are generally easily
factored.


"""
n_diophantine_reciprocals(n) = div(n_divisors(n^2) + 1, 2)

"""
Test it out on the prior problems we have solved
"""
## n_diophantine_reciprocals(180180)
## 1013

## n_diophantine_reciprocals(1260)
## 113

# function search(n, limit=4000000)
#     for i = 2:10000
#         if n_diophantine_reciprocals(n*i)  > limit
#             return n*i
#         end
#     end
#     Inf
# end

function search(n, limit=4000000)
    for i = 2:10000
        if n_diophantine_reciprocals(n*i)  > limit
            return n*i
        end
    end
    Inf
end



"""
Let's search for the lowest n such that the number of solutions exceeds 4,000,000.
For a given n = ppp(k) for k = {11,12,13,14}, search for lowest ppp(k)*i, i = 2:1000 such that ppp(k)*i > 4,000,000
"""

## search(ppp(11))
## "nothing"

## search(ppp(12))
## 1×3 Array{BigInt,2}:
## 1260  9350130049860600  4018613

## search(ppp(13))
## 1×3 Array{BigInt,2}:
## 36  10953009486979560  4340102

## search(ppp(14))
## 1×3 Array{BigInt,2}:
## 4  52331045326680120  5580131

"""
    Looks like 9350130049860600 is the lowest!!
"""

function euler110()
    minimum([search(ppp(p)) for p in 12:14])
end

# @time euler110()
# 0.067052 seconds (571.10 k allocations: 16.231 MB)
# 9350130049860600


