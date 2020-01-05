##The  purpose of these functions is to allow the user to compute and "cache" (i.e store) the result 
## of a matrix inversion computation, which can sometimes be quite computer-intensive. 
##The first function "makeCacheMatrix" creates an object to store both a matrix and its inverse. The second function
##cacheSolve returns the inverse of the matrix specified as an argument in the makeCacheMatrix function.


## This function takes a matrix as its input and will output a list of four objects that enable the original
## matrix object and its inverse to be set and retrieved. It specifies relevant defaults to ensure 

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve takes as its argument a relevant formulation of the makeCacheMatrix function (with a relevant matrix 
## in turn specified as its argument). It looks to see if there is a cached value for the matrix whose inverse it is
## evaluating, if there is not already then it retuns the matrix's inverse. If there is, then it returns the cached 
## value (if it is returning the cached value a "getting cached data" message will appear in the console).

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
