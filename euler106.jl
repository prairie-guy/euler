"""
euler106.jl
12/27/2016

Let S(A) represent the sum of elements in set A of size n. We shall call it a special sum set
if for any two non-empty disjoint subsets, B and C, the following properties are true:

1) S(B) ≠ S(C); that is, sums of subsets cannot be equal.
2) If B contains more elements than C then S(B) > S(C).

n = 1: {1}
n = 2: {1,  2}
n = 3: {2,  3,  4}
n = 4: {3,  5,  6,  7}
n = 5: {6,  9,  11, 12, 13}
n = 6: {11, 18, 19, 20, 22, 25}
--------------------------------------
nP= 7: {20, 31, 38, 39, 40, 42, 45}  Predicted
--------------------------------------
n = 7: {20, 31, 38, 39, 40, 42,45}   Actual

For this problem we shall assume that a given set contains n strictly increasing elements
and it already satisfies the second rule.

Surprisingly, out of the 25 possible subset pairs that can be obtained from a set
for which n = 4, only 1 of these pairs need to be tested for equality first rule.

Similarly, when n = 7, only 70 out of the 966 subset pairs need to be tested.
For n = 12, how many of the 261625 subset pairs that can be obtained need to be tested for equality?
"""

## Provides ++ shorthand for merging vectors and strings. Basically, vcat, hcat and * for strings
## For example, [1,2,3] ++ [3,6] -> [1,2,3,3,6] and "abc" ++ "de" -> "abcde"
using CBD.++

##  Iterators Package  clobbers Standard Library version of Iterators
##  Standard Library: Base.Iterators
##  Package  Library: Main.Iterators or just Iterators
import Iterators
import Combinatorics
import Primes

"""
uessps :: Unique Equal Length (p) Disjoint Pairs of a the Subsets of a Set, an example uessps for Set would be:
set: {1,2,3,4}, where the set is strictly increasing so elements 1<2<3<4...
p=1: {(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)}
p=2: {(12,34),(13,24),(14,23)}, where (12,34) means ({1,2},{3,4})
p=3: {}

For this problem, we only need consider uessps for p={2,3 ... len(Set)/2}
For {1,2,3,4,5,6,7,8,9,10,11,12} we only need to consider uespps for p = {2,3,4,5,6}

"""
function uessps(set, p::Int)::Array
    s= [c for c in combinations(set, p)]
    return [(x,y) for (x,y) in Iterators.subsets(s,2) if allunique(x ++ y)] # Need to filter out non-unique pairs
end

"""Merge all uessps for p = {2,3,..., len(set)/2}"""
function all_uessps(set)::Array
    max = div(length(set), 2) 
    all = [([0,0],[0,0])]     # Type of all needs to match anything '++' to it.
    for p in 2:max
        el = uessps(set, p)         
        if !isempty(el)
            all = all ++ el            
        end            
    end
    all[2:end]    
end    

"""
Is equality test logically needed, given strict ordering. For example, no need to ever test ([1,2],[3,4]), but
([1,4],[2,3]) needs to be tested because element 4 can clearly be of a size so that either the lhs or rhs could be greater.
"""
function skip_equality_test(set1, set2)::Bool
    len1, len2 = length(set1), length(set2)
    return len1 >= len2 && all(set1 .< set2[1:len1])
end    

"""
Find all of the Unique Equal Length (p) Disjoint Pairs of a the Subsets of a Set {1,2,...,12} of size 12, i.e,
p={2,3,4,5,6} then filter out (skip) those uessps not logically required to be tested for equality. What remains
are the uessps that absolutely need to be tested for equality.
"""
function euler106()::Int
    return length([(x,y) for (x,y) in all_uessps(1:12) if !skip_equality_test(x,y)])
end    

## @time euler106()
## 1.282191 seconds (9.03 M allocations: 864.202 MB, 13.03% gc time)
## 21384

"""
This is a correct solution, but won't scale. Moreover, it seems that there is a closed formed combinatoric solution.
I don't know how to solve this mathematically, so let's build some tools and go pattern hunting.

Idea: This problem reeks with Combinations and/or binominals, so compute binonmials: Binonmial(len, p) and look for
factors in the required comparisions. (Where len = length(Set))

"""

"""For a given set and p, find total, skipped and required comparisons in order to look for patterns"""
function comparisons(set, p::Int)::Array
    skipped  = 0    
    combs = [c for c in combinations(set, p)]
    for x in combs
        for y in combs
            if x != y && allunique(x ++ y) && skip_equality_test(x,y)
                skipped = skipped + 1
            end
        end
    end        
    total = length(uessps(set, p))
    required = total - skipped
    return [total, skipped, required]
end

"""Print out the comparisons as well as the Binomial Coefficient. Look at Required/Binomial to see if there are
and constants that appear for different p's.
"""
function show_comparisons(set, p::Int)
    total, skipped, required = comparisons(set, p)
    println("$(length(set)):$p")
    println("Total    Comparisons: ", total)
    println("Skipped  Comparisons: ", skipped)    
    println("Required Comparisons: ", required)
    println("         Binomial($(length(set)), p): ", [binomial(length(set), i) for i in 0:length(set)])
    println("Required/Binomial($(length(set)), p): ", required./[binomial(length(set), i) for i in 0:length(set)])
end


"""
There are patterns to be discovered!

Here is an example for p = 3 and set = {1:6, 1:7, 1:8, 1:9 }. Notice how the constant 5 appears in the 7th position.

show_comparisons(1:6,3)
6:3
Total    Comparisons: 10
Skipped  Comparisons: 5
Required Comparisons: 5
Binomial(6, p): [1,6,15,20,15,6,1]
Required/Binomial(6, p): [5.0,0.833333,0.333333,0.25,0.333333,0.833333,5.0]

julia> show_comparisons(1:7,3)
7:3
Total    Comparisons: 70
Skipped  Comparisons: 35
Required Comparisons: 35
Binomial(7, p): [1,7,21,35,35,21,7,1]
Required/Binomial(7, p): [35.0,5.0,1.66667,1.0,1.0,1.66667,5.0,35.0]

julia> show_comparisons(1:8,3)
8:3
Total    Comparisons: 280
Skipped  Comparisons: 140
Required Comparisons: 140
Binomial(8, p): [1,8,28,56,70,56,28,8,1]
Required/Binomial(8, p): [140.0,17.5,5.0,2.5,2.0,2.5,5.0,17.5,140.0]

julia> show_comparisons(1:9,3)
9:3
Total    Comparisons: 840
Skipped  Comparisons: 420
Required Comparisons: 420
Binomial(9, p): [1,9,36,84,126,126,84,36,9,1]
Required/Binomial(9, p): [420.0,46.6667,11.6667,5.0,3.33333,3.33333,5.0,11.6667,46.6667,420.0]


Here is what I discovered:
required_comparisons(set, 1) =   0
required_comparisons(set, 2) =   1*binomial(len, 4)
required_comparisons(set, 3) =   5*binomial(len, 6)
required_comparisons(set, 4) =  21*binomial(len, 8)
required_comparisons(set, 5) =  84*binomial(len, 10)
required_comparisons(set, 6) = 330*binomial(len, 12)


I then searched oeis.org for the sequence {1, 5, 21,84 ,330} and found BinomialC2n+1,n-1) and extended further:
{1, 5, 21, 84, 330, 1287, 5005, 19448, 75582,...}

Putting it all together results in a nice combinatoric equations, which might further be simpified. But, I am done . . 

all_required_comparisons(set) =
Sum (n = {2,3..,len/2}) [Binomial(2n - 1, n - 2)] * Binonmial(len, 2n)]

"""

function all_required_comparisons(len)
    len = BigInt(len)    
    [sum((binomial(2n - 1, n - 2) * binomial(len, 2n)) for n in  2:div(len,2))][1]
end


## julia> @time all_required_comparisons(7)
## 0.000115 seconds (115 allocations: 2.625 KB)
## 70

## julia> @time all_required_comparisons(12)
## 0.000131 seconds (240 allocations: 5.195 KB)
## 21384

## julia> @time all_required_comparisons(1000)
## 0.029514 seconds (30.57 k allocations: 1.295 MB, 33.10% gc time)
## 10150589213461492975282618693404882751891775379366876402919505716029346814032667845587020886706777415239311009193594061113747200067733036135841504782335797960965902900709787121881318379554195054696020176015147783692676123390567600811608139204613137058070115156511120149357566610383043439701181906708318210212083521856534460779339912664396043748369678314665741356166949849594818985810637306137471333352300302177197673897855932950420397696875301113467883835093042272578992252772
