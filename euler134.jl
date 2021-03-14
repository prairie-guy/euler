#/usr/bin/env julia

# Consider the consecutive primes p1 = 19 and p2 = 23.
# It can be verified that 1219 is the smallest number such that:
# 1) The last digits of the number are formed by p1
# 2) The number is divisible by p2.

# In fact, with the exception of p1 = 3 and p2 = 5,
# for every pair of consecutive primes, p2 > p1,
# there exist values of n for which the last digits
# are formed by p1 and n is divisible by p2.

# Let S be the smallest of these values of n.
# Find ∑ S for every pair of consecutive primes with 5 ≤ p1 ≤ 1000000.

using Primes

function p1_p2mult(p1,p2)
    p  = digits(p2)[1]
    ds = digits(p1)
    sum = 0
    for (d1,b) in zip(ds, 0:length(ds)-1)
        d2 = digits(sum)[b+1]
        m = mod((d1-d2)*invmod(p,10),10)
        sum = sum + p2 * m * 10^b
        #println("$p, $d1, $d2, $(10^b), $m, $sum, ")
    end
    return(sum)
end


function euler134(until)
    total = BigInt(0)
    ps = primes(5,until)
    append!(ps,nextprime(ps[end] + 1))
    for i in 1:length(ps)-1
        total = total + p1_p2mult(ps[i],ps[i+1])
    end
    return(total)
end

# julia> @time euler134(10^6)
# 0.074097 seconds (616.97 k allocations: 82.778 MiB, 3.52% gc time)
# 18613426663617118


# Modular arthmitic approach from solutions:
# Problem is to find:

# p2 * k == p1 (mod 10^d), where d is number of digits in p1
# then,
# k = p1 * inv(p2, 10^d) mod 10^d
# and,
# n = p2 * k

# Example 131,137
# k = 131 * invmod(137,10^3) mod 10^3 = 563
# n = 137 * 563 = 77131

function p1_p2mult2(p1,p2)
    b = Int(ceil(log10(p1)))
    k = mod(p1 * invmod(p2, 10^b),10^b)
    return(p2*k)
end








