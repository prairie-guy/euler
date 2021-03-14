"""
euler105.jl
12/26/2016

Let S(A) represent the sum of elements in set A of size n. We shall call it a special sum set if 
for any two non-empty disjoint subsets, B and C, the following properties are true:

   (1) S(B) ≠ S(C); that is, sums of subsets cannot be equal.
   (2) If B contains more elements than C then S(B) > S(C).

For example, {81, 88, 75, 42, 87, 84, 86, 65} is not a special sum set because: 65 + 87 + 88 = 75 + 81 + 84, 
whereas {157, 150, 164, 119, 79, 159, 161, 139, 158}
satisfies both rules for all possible subset pair combinations and S(A) = 1286.

Using sets.txt a 4K text file with one-hundred sets containing seven to twelve elements 
(the two examples given above are the first two sets in the file), identify all the special sum sets, 
A1, A2, ..., Ak, and find the value of S(A1) + S(A2) + ... + S(Ak).

"""
import IterTools

function isspecial(set)
    subsets = [[length(subset), sum(subset), subset] for subset in IterTools.subsets(set)][2:(end - 1)]
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

function euler105()
    f = open("euler105.txt", "r")
    ssum= 0    
    for set in readlines(f)
        set = chomp(set)
        set = split(set, ",")
        set = map(x-> parse(Int, x), set)
        if isspecial(set)
            ssum += sum(set)                
        end
    end
    return ssum    
end


## @time euler105()
## 0.110295 seconds (898.94 k allocations: 63.283 MB, 7.58% gc time)
## 73702


