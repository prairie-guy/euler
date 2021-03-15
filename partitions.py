def partitions(n, m = None):
  """Partition n with a maximum part size of m. Yield non-increasing
  lists in decreasing lexicographic order. The default for m is
  effectively n, so the second argument is not needed to create the
  generator unless you do want to limit part sizes.
  """
  if m is None or m >= n: yield [n]
  for f in range(n-1 if (m is None or m >= n) else m, 0, -1):
    for p in partitions(n-f, f): yield [f] + p


    

def partition(n):
    parts = set()
    parts.add((n, ))
    for x in range(1, n):
        for y in partition(n - x):
            parts.add(tuple(sorted((x, ) + y)))
    return parts


stack = []
def P(n, m = 1):
  if n == 0:
    print(stack)
  else:
    for i in range(m, n+1):
      stack.append(str(i))
      P(n - i, i)
      stack.pop()
