#####################################################################################################
## Matrix inversion is usually a costly computation. 
## The following pair of functions(makeCacheMatrix, cacheSolve) caches this
## potentially time-consuming computations i.e. inverse of a matrix.
#####################################################################################################


########################################## makeCacheMatrix ########################################## 

## This function creates a special "matrix" object that can cache its inverse
## It comprises a list containing function to
##    set the value of the matix
##    get the value of the matrix
##    set the value of the inverse
##    get the value of the inverse

#####################################################################################################

makeCacheMatrix <- function(x = matrix()) {
    
    ## initialising the inverse variable to to NULL
    i <- NULL
    
    ## Constructing the 'set' function
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    
    ## constructing the 'get' function
    get <- function() x
    
    ## constructing the 'setinverse' function
    setinverse <- function(inverse) i <<- inverse
    
    ## constructing the 'getinverse' function
    getinverse <- function() i
    
    ## returning a list containing the functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


########################################## cacheSolve ##########################################

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then it retrieves the inverse from the cache.
## Computing the inverse of a square matrix is done with the 'solve' function in R

################################################################################################

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    ## 'x' is a instance of a special "matrix" returned by the makeCacheMatrix
      
    ## try to get the cache inverse value of the matrix
    i <- x$getinverse()
    
    ## checks if matrix has been inversed
    if(!is.null(i)) {
      message("getting cached data")
      
      ## returning the cache inverse value
      return(i)
    }
    
    ## retrive the cache of the matrix if the inverse has not been computed and cached
    data <- x$get()
    
    ## computing the inverse of the matrix    
    i <- solve(data, ...)
    
    ## caching the inverse of the matrix
    x$setinverse(i)
    
    ## return the inverse of the matrix
    i
}
