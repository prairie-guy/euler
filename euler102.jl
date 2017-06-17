"""
euler102.jl
12/18/2016  

Three distinct points are plotted at random on a Cartesian plane, for which -1000 ≤ x, y ≤ 1000, such that a triangle is formed.

Consider the following two triangles:
(-340,495), (-153,-910), (835,-947)
(-175,41), (-421,-714), (574,-645)

It can be verified that triangle ABC contains the origin, whereas triangle XYZ does not.

Using triangles.txt right,a 27K text file containing the co-ordinates of one thousand "random" triangles,
find the number of triangles for which the interior contains the origin.

NOTE: The first two examples in the file represent the triangles in the example given above.


"""

function length(p1,p2)
    (x1,y1) = p1
    (x2,y2) = p2
    return sqrt((x2-x1)^2 + (y2-y1)^2)
end


function  areaTriangle(p1,p2,p3)
    (x1,y1) = p1
    (x2,y2) = p2
    (x3,y3) = p3
    l = length(p1,p2)
    m = length(p1,p3)
    n = length(p2,p3)
    s = 1/2*(l + m + n)
    return sqrt(s * (s-l) * (s-m) * (s-n))
end    


function isInsideTriangle(P,p1,p2,p3,rndTo = 3)
    A = areaTriangle(p1,p2,p3)
    X = areaTriangle(P,p1,p2)
    Y = areaTriangle(P,p1,p3)
    Z = areaTriangle(P,p2,p3)
    if (round(A,rndTo) == round(X + Y + Z, rndTo))
        return true
    else
        return false
    end    
end


function euler102(r = 3)
    orig = (0.0,0.0)
    f = open("euler102.txt","r")
    count = 0
    for line in readlines(f)
        line = chomp(line)
        line = split(line,",")
        line = map(x->parse(Int,x), line)        
        (a,b, m,n, s,t) = line
        p1 = (a,b); p2 = (m,n); p3 = (s,t)        
        if isInsideTriangle(orig, p1, p2, p3, r)
            count += 1            
        end
    end
    return count    
end    

function euler102alt(r = 3)
    orig = (0,0)
    f = open("euler102.txt","r")
    M = readcsv(f)
    count = 0    
    for l = 1:size(M,1)    
        line = M[l,:]
        (a,b, m,n, s,t) = line
        p1 = (a,b);  p2 = (m,n);  p3 = (s,t)        
        if isInsideTriangle(orig, p1, p2, p3, r)
            count += 1            
        end
    end
    return count    
end     



# @time(euler102())
# 0.002696 seconds (14.03 k allocations: 704.781 KB)
# 228
