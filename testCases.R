source("cachematrix.R", echo=T)
a <- makeCacheMatrix()
c <- rbind(c(1, -1/4), c(-1/4, 1))
a$set(c)
a$get()
all.equal(a$get(), c)
cacheSolve(a)
all.equal(cacheSolve(a), solve(c))

b <- makeCacheMatrix()
d <- rbind(c(1, -1/8), c(-1/8, 1))
b$set(d)
all.equal(b$get(), d)
cacheSolve(b)
all.equal(cacheSolve(b), solve(d))

b$set(c)
all.equal(b$getinverse(), solve(c))
