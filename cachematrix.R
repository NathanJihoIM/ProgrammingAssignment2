## The functions below will cache the calculation result of
## the inverse matrix from a given square matrix.
## 
## The codes are based on the given example by the instructor,
## 'Caching the mean of a vector'
## 
## The sample results are followed as below:
##
##
## > source("cachematrix.R")
## > a <- matrix(1:4, 2, 2)     Matrix "A" created
## > a
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > aa <- makeCacheMatrix(a)
## > cacheSolve(aa)             The first time of "A" (Calculated)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(aa)             The second time of "A" (from Cache)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## > b <- matrix(5:8, 2, 2)     Matrix "B" created
## > b
##      [,1] [,2]
## [1,]    5    7
## [2,]    6    8
## > bb <- makeCacheMatrix(b)
## > cacheSolve(bb)             The first time of "B" (Calculated)
##      [,1] [,2]
## [1,]   -4  3.5
## [2,]    3 -2.5
## > cacheSolve(bb)             The second time of "B" (from Cache)
## getting cached data
##      [,1] [,2]
## [1,]   -4  3.5
## [2,]    3 -2.5
##
## > cacheSolve(aa)             The THIRD time of "A" (from Cache)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > 


## This function, makeCacheMatrix creates a special "vector", 
## which is really a list containing a function to:
##
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        ## Initializing the inverse matrix to NULL
        inv <- NULL
        ## To set a matrix from the given matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## To read a matrix with the given information
        get <- function() x
        ## To set the inverse matrix by using 'solve' function
        setinverse <- function(solve) inv <<- solve
        ## To read the inverse matrix
        getinverse <- function() inv
        ## Return the functions
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## This function, cacheSolve calculates the inverse matrix 
## which was not calculated before. In case of the second time,
## the function will return the cached result.

cacheSolve <- function(x, ...) {
        ## Reading the existing inverse matrix for the given matrix
        inv <- x$getinverse()
        ## Return the cached result if already calculted
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## Reading the original matrix
        mtrx <- x$get()
        ## Calculating the inverse matrix
        inv <- solve(mtrx, ...)
        ## Stroing the inverse matrix
        x$setinverse(inv)
        ## Showing the inverse matrix
        inv
}
