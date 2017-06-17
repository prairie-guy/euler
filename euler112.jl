"""
euler112.jl 
1/7/2107

Bouncy Numbers

Working from left-to-right if no digit is exceeded by the digit to its left it is called an increasing number;
for example, 134468.
Similarly if no digit is exceeded by the digit to its right it is called a decreasing number;
for example, 66420.

We shall call a positive integer that is neither increasing nor decreasing a "bouncy" number;
for example, 155349.

Clearly there cannot be any bouncy numbers below one-hundred, but just over half of the numbers below one-thousand (525)
are bouncy. In fact, the least number for which the proportion of bouncy numbers first reaches 50% is 538.

Surprisingly, bouncy numbers become more and more common and by the time we reach 21780 the proportion of bouncy numbers
is equal to 90%.

Find the least number for which the proportion of bouncy numbers is exactly 99%.
"""

using CBD.++          ## Shorthand for merging vectors and strings.
import Iterators      ## Iterators Module (Main.Iterators or Iterators) overrides Base.Iterators
import Combinatorics
import Primes

decreasing(n) = issorted(digits(n))
increasing(n) = issorted(digits(n), lt= >)


function isbouncy(n)
    if increasing(n) || decreasing(n)
        return false
    else
        return true
    end
end

function euler112(limit= .99)
    n, bounces = 0, 0
    while true
        n += 1
        if isbouncy(n)
            bounces += 1
        end
        if bounces/n >= limit
            return n
        end
    end
end


# @time euler112(.99)
# 4.925118 seconds (4.75 M allocations: 549.352 MB, 2.74% gc time)
# 1587000
