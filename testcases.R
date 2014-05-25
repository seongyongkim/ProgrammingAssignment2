source("cachematrix.R")

print("Testing the non-cached version of Solve with 1000 x 1000 matrix and repeat 10 times.")
notCached <- matrix(sample(1000000), 1000)
st <- system.time(for(i in 1:10) solve(notCached))
print(st)

print("Testing the cached version of makeCacheMatrix with 1000 x 1000 matrix and repeat 10 times.")
cachedMatrix <- makeCacheMatrix(matrix(sample(1000000), 1000))
st <- system.time(for(i in 1:10) cacheSolve(cachedMatrix))
print(st)

print("Testing the cached version of makeCacheFunctions with 1000 x 1000 matrix and solve function and repeat 10 times.")
cachedMatrix <- makeCacheFunctions(matrix(sample(1000000), 1000), solve)
st <- system.time(for(i in 1:10) cachedMatrix$getResult())
print(st)

print("Testing the non-cached version of Mean with 1,000,000 vector and repeat 10 times.")
notCached <- sample(1000000)
st <- system.time(for(i in 1:10) mean(notCached))
print(st)

print("Testing the cached version of makeCacheFunctions with 1,000,000 vector and mean function and repeat 10 times.")
cachedMatrix <- makeCacheFunctions(matrix(sample(1000000), 1000), mean)
st <- system.time(for(i in 1:10) cachedMatrix$getResult())
print(st)

