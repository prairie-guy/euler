#!/usr/bin/env julia

# Euler 120
# Let r be the remainder when (a−1)^n + (a+1)^n is divided by a^2.
# For example, if a = 7 and n = 3, then r = 42: 63 + 83 = 728 ≡ 42 mod 49.
# And as n varies, so too will r, but for a = 7 it turns out that rmax = 42.
# For 3 ≤ a ≤ 1000, find ∑ rmax.

# Frequency of a cycle is either a for even or 2a for odd
# Didn't bother to consider both cases. Just used a frequency of 2a
# saved the range and found the max.

function cycle(a)
    values = []
    for n in BigInt(1):2*a
        #println("($(a-1),$(a+2)):$(a^2):$(n)-> $(mod((a-1)^n, a^2)) $(mod((a+1)^n, a^2)) $(mod((a-1)^n + (a+1)^n, a^2))")
        append!(values, mod((a-1)^n + (a+1)^n, a^2))
    end
    return(maximum(values))
end

function euler120(n)
    sum([cycle(a) for a in 3:n])
end



# @time euler120(1000)
# 6.229961 seconds (24.03 M allocations: 2.711 GiB, 7.79% gc time)
# 333082500
