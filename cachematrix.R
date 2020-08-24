## Put comments here that give an overall description of what your
## functions do


## In this function, I would like to create
## a 'matrix' object which is able to
## cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  ## set the matrix
  set <- function(y)  {
         x <<- y
         inver <- NULL
  }
  ## get matrix
  get <- function() x
  ## set the inverse matrix
  setInverse <- function(inverse) inver <<- inverse
  ## get inverse matrix
  getInverse <- function() inver
  ## return the list of all methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## In this function, I want to computes the inverse of matrix
## created by previous makeCacheMatrix function. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getInverse()
        ## return the inverse if it's set
        if( !is.null(inver) ) {
          message("getting cached data")
          return(inver)
        }
        
        ## get the matrix from our object
        data <- x$get()
        ## calculate the inverse using matrix multiplication
        inver <- solve(data) %*% data
        ## set the inverse to the object
        x$setInverse(inver)
        
        ## return matrix
        inver
  
}
