#
# Euler 187 - Semiprimes
# A composite is a number containing at least two prime factors.
# For example, 15 = 3 × 5; 9 = 3 × 3; 12 = 2 × 2 × 3.
#
# There are ten composites below thirty containing
# precisely two, not necessarily distinct,
# prime factors: 4, 6, 9, 10, 14, 15, 21, 22, 25, 26.
#
# How many composite integers, n < 10^8, have precisely two,
# not necessarily distinct, prime factors?
#

using Primes

function euler208(lim)
    sp = 0
    sqr_lim = Int(ceil(sqrt(lim)))
    for i=primes(sqr_lim)
        for j=primes(i,lim)
            if (i*j < lim)
                sp = sp +1
                println([i,j,i*j])
            end
        end
    end
    println(sp)
end
