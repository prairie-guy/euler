"""
  euler102.py
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

from __future__ import division
import math
from time import time


def length(p1,p2):
    (x1,y1) = p1;
    (x2,y2) = p2
    return math.sqrt(math.pow(x2-x1,2) + math.pow(y2-y1,2))
    

def areaTriangle(p1,p2,p3):
    (x1,y1) = p1
    (x2,y2) = p2
    (x3,y3) = p3
    l = length(p1,p2)
    m = length(p1,p3)
    n = length(p2,p3)
    s = 1/2*(l + m + n)
    t = s * (s-l) * (s-m) * (s-n)
    a = math.sqrt(t)
    return a
    

def isInsideTriangle(P,p1,p2,p3, rndTo = 3):
    A = areaTriangle(p1,p2,p3)
    X = areaTriangle(P,p1,p2)
    Y = areaTriangle(P,p1,p3)
    Z = areaTriangle(P,p2,p3)
    if (round(A,rndTo) == round(X + Y + Z,rndTo)):
        return True
    else:
        return False

    
def euler102(r = 3):
    Orig = (0.0,0.0)
    f = open("euler102.txt","r")
    count = 0
    for line in f.readlines():
        line = line.strip()
        line = line.rsplit(",")
        line = map(int, line)
        (a,b, m,n, s,t) = line
        p1 = (a,b)
        p2 = (m,n)
        p3 = (s,t)
        if isInsideTriangle(Orig, p1, p2, p3, rndTo=r):
            count = count + 1
    return count

# Rounding to determine equality REAllY MATTERS; 
def test():
    for i in range(13):
        print i, euler102(i)
    
    
# start = time()    
# print euler102(3) 
# print time() - start
# -> 228
# -> 0.0275
