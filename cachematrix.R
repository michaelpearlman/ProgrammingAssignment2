## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this creates a list containing a function.
## it will set the value of the matrix
## it will get the value of the matrix
## it will get the value of the inverse of the matrix
## it will set the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## It will get the inverse of the matrix, if the inverse hasn't already
## been computed. If it has it will return the results. It will the set the 
## cache setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
