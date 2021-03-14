#!/usr/bin/env julia

# Leonhard Euler was born on 15 April 1707.
# Consider the sequence 1504170715041707n mod 4503599627370517.
# An element of this sequence is defined to be an Eulercoin if it is strictly smaller than all previously found Eulercoins.
# For example, the first term is 1504170715041707 which is the first Eulercoin. The second term is 3008341430083414 which is greater than 1504170715041707 so is not an Eulercoin. However, the third term is 8912517754604 which is small enough to be a new Eulercoin.
# The sum of the first 2 Eulercoins is therefore 1513083232796311.
# Find the sum of all Eulercoins.
#
#

using Primes



ppdict(n) = [(k,v) for (k,v) in pairs(Primes.factor(n))] 

function euler(k)
    f = 4503599627370517
    min = BigInt(1504170715041707)
    sum = min
    p = BigInt(1504170715041707)
    r = p % f
    #println("*$r, $(keys( Primes.factor(r))), $(keys( Primes.factor(p)))" )
    #println("*$r, $(Primes.factor(r)), $(Primes.factor(p))")
    println("1, $min, $sum" )
    
    for n in 1:10^k
        p = n*BigInt(1504170715041707)
        r = p % f
        #println("$r, $(keys( Primes.factor(r))), $(keys( Primes.factor(p)))" )
        if r < min
            min = r
            sum = sum + min
            println("$n, $min, $sum, $(r/f)" )
            #println("$min, $(Primes.factor(min)), $sum" )
            #println("$min, $(Primes.factor(min))" )
            #println("*$r, $(keys( Primes.factor(r))), $(keys( Primes.factor(p)))" )
            #println("*$r, $(keys( Primes.factor(r))), $(keys( Primes.factor(p)))" )
            #println("*$r, $(Primes.factor(r)), $(Primes.factor(p))" )
        end
    end
end

euler(7)
