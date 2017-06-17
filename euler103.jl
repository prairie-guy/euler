"""
euler103.jl
12/27/2016

Let S(A) represent the sum of elements in set A of size n. We shall call it a special sum set if for any
two non-empty disjoint subsets, B and C, the following properties are true:

S(B) ≠ S(C); that is, sums of subsets cannot be equal.
If B contains more elements than C then S(B) > S(C).
If S(A) is minimised for a given n, we shall call it an optimum special sum set.
The first five optimum special sum sets are given below.

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

It seems that for a given optimum set, A = {a1, a2, ... , an},
the next optimum set is of the form B = {b, a1+b, a2+b, ... ,an+b},
where b is the "middle" element on the previous row.

By applying this "rule" we would expect the optimum set for n = 6 to be
A = {11, 17, 20, 22, 23, 24}, with S(A) = 117.
However, this is not the optimum set, as we have merely applied an algorithm to provide a near optimum set.
The optimum set for n = 6 is A = {11, 18, 19, 20, 22, 25}, with S(A) = 115 and corresponding set string: 111819202225.
Given that A is an optimum special sum set for n = 7, find its set string.

"""
## Very confusingly, there is also a Standard Library called 'Iterators'. Calling this package clobbers the other.
## The fix is a real hack

Iters = Iterators # Preserves the Standard Library Iterator functions
import Iterators  # To access these functions, fully-delimited name must be used.

function isspecial(set)
    subsets = [[length(subset), sum(subset), subset] for subset in Iterators.subsets(set)][2:(end - 1)]
    """subset[1] -> size, subset[2]-> sum, subset[3]-> subset"""
    if !allunique(map(subset-> subset[2], subsets))
        """Test Condition (1) of a Special Sum"""
        return false
    else
        sort!(subsets; by= subset-> subset[2])
        sort!(subsets; by= subset-> subset[1])
        minsize, maxsize  = extrema(map(subset-> subset[1], subsets))        
        minsum,  maxsum  = Dict(), Dict()
        for size in minsize:maxsize
            allofsize = filter(subset-> subset[1] == size, subsets)
            minsum[size], maxsum[size] = allofsize[1], allofsize[end]
        end
        for size in minsize:(maxsize - 1)
            if maxsum[size][2] > minsum[size + 1][2]
                """Test Condition (2) of a Special Sum"""
                return false                
            end
        end
    end        
    return true        
end


function euler103(superset, n)
    minset, minsum = [], Inf    
    for s in Iterators.subsets(superset, n)
        if isspecial(s)
            if sum(s) < minsum
                minset, minsum = s, sum(s)                
            end                
        end            
    end
    return(minset, minsum)    
end

super1to5 = [1,2,3,4,5,6,7,8,9,10,11,12,13];
super6 = [i for i in 10:25];
println("n2 = ", euler103(super1to5, 2))
println("n3 = ", euler103(super1to5, 3))
println("n4 = ", euler103(super1to5, 4))
println("n5 = ", euler103(super1to5, 5))
println("n6 = ", euler103(super6, 5))

## As hoped, these are the corret values for n = 1, 6
## n2 = ([1,2],3)
## n3 = ([2,3,4],9)
## n4 = ([3,5,6,7],21)
## n5 = ([6,9,11,12,13],51)
## n6 = ([10,11,12,14,17],64)


super7 = [i for i in 19:46];
##  We start with a full range covering the expected range. Notably, the list is 28 numbers long.
## super7 = [19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46]
## @time euler103(super7, 7)
## 93.933070 seconds (1.08 G allocations: 75.749 GB, 10.75% gc time)
## ([20,31,38,39,40,42,45],255)
## 20313839404245
## Correct answer, but that took a ton of time and resouces


## Now lets take the hint that the algorithm is close to optimal. Lets take the prediction and then perturb it a bit.
## Clearly a heuristic approach.
## Lets look at what n7 is predicted to be by the the Non-optimal algorithm.
## A = {a1, a2, ... , an},
## B = {b, a1+b, a2+b, ... ,an+b}
nP7 = [20, 31, 38, 39, 40, 42, 45];
## If we check this, we find out it is a special number. Maybe this is it, but we should probably check a little more.
## Let's add a little noise to come up with a longer list. Essentially add/sub a 1 from each number. 

super7P = (function()
    a = Int64[]
    for i in nP7
        append!(a,[i+1, i, i-1])
    end
    a = collect(Iterators.distinct(a))
    sort!(a)    
    return a
end)();  # Notice the calling of the lambda function

## super7P = [19  20  21  30  31  32  37  38  39  40  41  42  43  44  45  46]
## Now at 16 numbers, this looks considerably more tractable than the 28 we tried before.
## For all subsets, 2^29 = 536,870,912 vs. 2^16 = 65536

## Great! Same answer, but runs in less than a second!
##
## @time euler103(super7P, 7)
## 0.981224 seconds (10.47 M allocations: 751.947 MB, 12.22% gc time)
## ([20,31,38,39,40,42,45],255)


