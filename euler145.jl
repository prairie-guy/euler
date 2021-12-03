#
#
# euler145
#
#
#
# Some positive integers n have the property that the sum [ n + reverse(n) ] consists entirely of odd (decimal) digits.
# For instance, 36 + 63 = 99 and 409 + 904 = 1313. We will call such numbers reversible; so 36, 63, 409, and 904 are
# reversible. Leading zeroes are not allowed in either n or reverse(n).
#
# There are 120 reversible numbers below one-thousand.
#
# How many reversible numbers are there below one-billion (10^9)?
#
# (defn reversible_Number? [n]
#   (if (== (mod n 10) 0)
#     false
#     (->> (cbd/integer->digits  n)
#          reverse
#          cbd/digits->integer
#          (+ n)
#          cbd/integer->digits
#          (every? odd?))))
#
# (defn euler145 [n]
#   (count (for [i (range 1 (inc n)):when (reversible_Number? i)] i )))
#
# (time (euler145 (Math/pow 10 9)))
# "Elapsed time: 7284410.173 msecs"
# 608720

using Transducers

isrev(n) =
    if mod(n, 10) == 0
        return false
    else
        digits(n)|>
            join |>
            x->parse(Int,x) |>
            x->x+n |>
            digits |>
            Map(isodd) |>
            all
    end

euler145(n) =
    1:n |>
    Map(isrev)|>
    sum
