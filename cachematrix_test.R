source("cachematrix.R")


testMatrix <- matrix(c(1,-1,1,2), 2, 2)

a <- makeCacheMatrix(testMatrix)
a$get()
cacheSolve(a)

