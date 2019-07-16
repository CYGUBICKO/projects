
k, m, j = var('k, m, j')
psum(k) = sum(1/j, j, 1, k)

print(sum(binomial(m,k)*psum(k), k, 0, m))
