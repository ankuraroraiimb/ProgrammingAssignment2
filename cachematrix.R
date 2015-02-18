makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(inv) m <<- inv
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

seq1 <- seq(1:4)
mat1 <- matrix(seq1, 2,2)


cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setmatrix(m)
        m
}

mat <- makeCacheMatrix(mat1)
cacheSolve(mat)
cacheSolve(mat)
m <- matrix(c(1, 2, 2, 1), nrow = 2, ncol = 2, byrow = TRUE)
mat <- makeCacheMatrix(m)
cacheSolve(mat)
cacheSolve(mat)
