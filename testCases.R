source("cachematrix.R")
a <- makeCacheMatrix()
c <- rbind(c(1, -1/4), c(-1/4, 1))
d <- rbind(c(1, -1/8), c(-1/8, 1))
a$set(c)
cacheSolve(a)
##          [,1]      [,2]
##[1,] 1.0666667 0.2666667
##[2,] 0.2666667 1.0666667
a$get()
##      [,1]  [,2]
##[1,]  1.00 -0.25
##[2,] -0.25  1.00
a$setinverse(solve(d))
cacheSolve(a)
##getting cached data
##          [,1]      [,2]
##[1,] 1.0158730 0.1269841
##[2,] 0.1269841 1.0158730
a$get()
##       [,1]   [,2]
##[1,]  1.000 -0.125
##[2,] -0.125  1.000

