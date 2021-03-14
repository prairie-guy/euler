# ;;
# ;; euler116 - Red, green or blue tiles
# ;;
# ;; A row of five grey square tiles is to have a number of its tiles
# ;; replaced with coloured oblong tiles chosen from red (length 2),
# ;; green (length 3), or blue (length 4).
# ;;
# ;; If red (2) tiles  are chosen there are exactly seven ways this can be done.
# ;;
# ;; If green (3) tiles are chosen there are three ways
# ;;
# ;; And if blue (4) tiles are chosen there are two ways.
# ;;
# ; Assuming that colours cannot be mixed there are 7 + 3 + 2 = 12 ways of
# ; replacing the grey tiles in a row measuring five units in length.;

using Combinatorics

all_in(A,B) = Set(A) == Set(B)

red(n) = sum([length(Set(permutations(c))) for
              c=[p for p=partitions(n) if all_in([1,2], p)]])

green(n) = sum([length(Set(permutations(c))) for
                c=[p for p=partitions(n) if all_in([1,3], p)]])

blue(n) = sum([length(Set(permutations(c)))
               for c=[p for p=partitions(n) if all_in([1,4], p)]])

total(n) = red(n) + green(n) + blue(n)

# Works for small numbers, but blows up for large
#
#julia> total(5)
#12
