## My functions create the inverse of the matrix provided as an argument
## The inverse matrix is cached locally so it can retrieved faster instead of 
## recomputing every time (if the same matrix is provided as argument)

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        set <- function(y) {
                x <<- y;
                inverseMatrix <<- NULL;
        }
        get <- function() x
        setInverse <- function(inverse) inverseMatrix <<- inverse;
        getInverse <- function() inverseMatrix;
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse);
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getInverse()
        if(!is.null(inverseMatrix)) {
                message("getting cached data for inverse matrix")
                return(inverseMatrix)
        } 
        else message ("calculating inverse matrix in first run")
        data <- x$get()
        inverseMatrix <- solve(data, ...)
        x$setInverse(inverseMatrix)
        inverseMatrix
}


## Sample run
## 
## > a <- matrix(c(2, 5, 3, 7), nrow=2, ncol=2)
##
## > b <- makeCacheMatrix(a)
##
## > b$get()
#      [,1] [,2]
# [1,]    2    3
# [2,]    5    7
##
## First time running cacheSolve - inverse is calculated
##
## > cacheSolve(b)
## calculating inverse matrix in first run
##      [,1] [,2]
## [1,]   -7    3
## [2,]    5   -2
##
## Second time running cacheSolve, so inverse isn't calculated but retrieved from ## cache
##
## > cacheSolve(b)
## getting cached data for inverse matrix
##      [,1] [,2]
## [1,]   -7    3
## [2,]    5   -2
## > 
##

