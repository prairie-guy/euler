def A(n):
	n *= 9
	k = 1
	while pow(10, k, n) != 1:
		k += 1
	return k
