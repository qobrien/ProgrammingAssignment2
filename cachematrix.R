## PROGRAMMING ASSIGNMENT 2
## These functions will ultimately return the inverse
## of a (cached) matrix.

## FIRST FUNCTION
## This function will store (cache) a matrix as a list.
## The list contains a function to:
        ## 1. set the matrix
        ## 2. get the matrix
        ## 3. set the inverse of the matrix
        ## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## sets the matrix the user inputs
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## gets the matrix the user inputs
        get <- function() x
        ## set the inverse of the matrix
        setInverse <- function(inverse) m <<- inverse
        ## gets the inverse of the matrix
        getInverse <- function() m
        ## returns a list for use in cacheSolve
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## SECOND FUNCTION
## This function first checks to see if an inverse 
## matrix has already been defined. If so, it pulls
## the inverse from the cache. If not, it calculates
## the inverse and stores it in the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInverse() ## defines the matrix inverse
        ## looks to see if inverse exists, then returns
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## gets the matrix
        matrix <- x$get()
        ## calculates the inverse 
        m <- solve(matrix, ...)
        ## sets the inverse
        x$setInverse(m)
        m ## Returns a matrix that is the inverse of 'x'
}

## TEST RUNS
## Remove ## symbols to run code.

## TEST ONE(1)
## m <- matrix(c(1:4, nrow=3, ncol=3))
## m
##      [,1] [,2] [,3]
## [1,]    1    4    3
## [2,]    2    1    4
## [3,]    3    2    1
## cm <- makeCacheMatrix(m)
## cacheSolve(cm)
##             [,1]        [,2]        [,3]
## [1,] -0.19444444  0.05555556  0.36111111
## [2,]  0.27777778 -0.22222222  0.05555556
## [3,]  0.02777778  0.27777778 -0.19444444

## TEST TWO(2)
## m <- matrix(c(2,3,4,5,6,7,8,9,0), nrow=3, ncol=3)
## m
##      [,1] [,2] [,3]
## [1,]    2    5    8
## [2,]    3    6    9
## [3,]    4    7    0
## cm <- makeCacheMatrix(m)
## cacheSolve(cm)
##      [,1]      [,2] [,3]
## [1,] -2.1  1.866667 -0.1
## [2,]  1.2 -1.066667  0.2
## [3,] -0.1  0.200000 -0.1