## Put comments here that give an overall description of what your
## functions do
## Assumption: Matrix input is always invertible

## Write a short comment describing this function


##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL #set an empty variable m
    set <- function(y) {
      x <<- y
      m <<- NULL
    } #function that sets the matrix value and resets the matrix inverse to a null value 
    get <- function() x #function that outputs the matrix value
    setinv <- function(inverse) m <<- inverse #sets the m value to the matrix inverse
    getinv <- function() m #returns the matrix inverse
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv) #functions here can be called using the '$' in the cacheSolve function (i.e. set() can be called by VariableName$set())
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinv() #set m to the inverse of the matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m) #if the inverse of matrix x is already computed, returns m which is a cached inverse matrix of m
  }
  data <- x$get() #gets the data from x list of functions
  m <- solve(data, ...) # sets m to the matrix inverse of the set matrix
  x$setinv(m) #caches the matrix inverse to m
  m #returns the matrix inverse
}
