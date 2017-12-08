
# cachematrix.R files contains functions to return the inverse of a matrix
# from the cache if it already calculated otherwise it calculates the inverse
# of the matrix and returns the inverse


# makeCacheMatrix: This function creates a special "matrix" object that can cache 
# its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## 
# cacheSolve: This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve should retrieve the
# inverse from the cache. Otherwise, it calculates the inverse of the data and
# sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
	m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m) 	#get inverse from cached data
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m   
    # library(matrixcalc)
    # if (!is.square.matrix(x)) {
        # stop('x is not a square matrix')
    # }
    # solve(x)   ## Return a matrix that is the inverse of 'x'
}

# TEST VALUES
# mat <- makeCacheMatrix(matrix(data=1:4,nrow=2))
# mat$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# cacheSolve(mat)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# cacheSolve(mat)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# Review the following pdfs for learning about inverse of a matrix.
# http://www.mathcentre.ac.uk/resources/uploaded/sigma-matrices7-2009-1.pdf
# http://www.mathcentre.ac.uk/resources/uploaded/sigma-matrices11-2009-1.pdf
# http://www.mathcentre.ac.uk/resources/Engineering%20maths%20first%20aid%20kit/latexsource%20and%20diagrams/5_5.pdf

# Usage
# 1. Construct the matrix
# Ex: matrix2 <- matrix(seq(1:4), 2)
# 2. Create the cache of the matrix
# Ex: z <- makeCacheMatrix(matrix2)
# 3. Call the cacheSolve of the matrix for getting the inverse
# Ex: cacheSolve(z)
# If determinant is zero then the inverse throws the following error message
# Error in solve.default(data) :
# Lapack routine dgesv: system is exactly singular: U[2,2] = 0