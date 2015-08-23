## makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix creates a list containing functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
      x <<- y
      inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## cacheSolve is a function that calculates the inverse of the matrix and caches it or retrieves the inverse from cache if already calculated. 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
    if(!is.null(inv)){
      message("getting cached inverse matrix data") 
      return(inv) 
    } else {
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  return(inv) 
  }
}
