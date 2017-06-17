"""
euler157.jl
1/8/2017

Solving the diophantine equation 1/x + 1/y= p/10^n

Consider the diophantine equation 1/x +1/x= p/10^n with x, y, p, n positive integers and x ≤ y.
For n=1 this equation has 20 solutions that are listed below:

1/1+1/1=20/10	1/1+1/2=15/10	1/1+1/5=12/10	1/1+1/10=11/10	1/2+1/2=10/10
1/2+1/5=7/10	1/2+1/10=6/10	1/3+1/6=5/10	1/3+1/15=4/10	1/4+1/4=5/10
1/4+1/20=3/10	1/5+1/5=4/10	1/5+1/10=3/10	1/6+1/30=2/10	1/10+1/10=2/10
1/11+1/110=1/10	1/12+1/60=1/10	1/14+1/35=1/10	1/15+1/30=1/10	1/20+1/20=1/10

How many solutions has this equation for 1 ≤ n ≤ 9
"""

using CBD.++          ## Shorthand for merging vectors and strings.
using Base.Iterators
using Iterators      ## Iterators Module (Main.Iterators or Iterators) overrides Base.Iterators
using Combinatorics
using Primes


"""
Calculate the number of divisors of n. Usage: n_divisors(24) -> 8, i.e., {1,2,3,4,6,8,12,24}
Divisors are the the Cartesian product of the prime factors. For example, 2^3*3^2*5 = 360  would be:
[2^0, 2^1 ,2^2, 2^3] * [3^0, 3^1, 3^2] * [5^0, 5^1] and n_divisors(360) = 4*3*2 = 24
"""

n_divisors(n) = prod(v + big(1) for (k, v) in factor(n))
n_proper_divisors(n) = n_divisors(n) - 1
function divisors(n)
    f = [one(n)]
    for (p,e) in factor(n)
        f = reduce(vcat, f, [f*p^j for j in 1:e])
    end
    return length(f) == 1 ? [one(n), n] : sort!(f)
end

function diophantine(n)
    p_ks = filter(x-> x>= 10^n, divisors(10^2n))
    solutions = []
    for p_k in  p_ks
        x_in = div(10^2n, p_k) + 10^n
        for p in divisors(x_in)
            x = div(x_in, p)
            k= p_k/p
            y = k + 10^n/p
            if isinteger(y)
                y = Integer(y)
                push!(solutions, [x, y, p])
            end
        end
    end
    return solutions
end

function euler157()
    return sum(length(diophantine(n)) for n in 1:9 )
end

# @time euler157()
# 0.182257 seconds (580.02 k allocations: 20.197 MB, 1.97% gc time)
# 53490
