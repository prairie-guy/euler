#
# euler179.clj
#
# Find the number of integers 1 < n < 10^7, for which n and n + 1
# have the same number of positive divisors.
# For example, 14 has the positive divisors 1, 2, 7, 14
# while 15 has 1, 3, 5, 15.
#

using Primes

sigma(n) = reduce(*, map(x-> x+1, values(factor(n))))

function neighbor_sigmas(n)
    div = [sigma(n) for n=1:n]
    sum(div[1:end-1].==div[2:end])
end

# @time println(neighbor_sigmas(10^7))
# 986262
# 10.680027 seconds (44.12 M allocations: 3.023 GiB, 2.96% gc time)
