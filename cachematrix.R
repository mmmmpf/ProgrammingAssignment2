###R Programming - Programming Assignment 2: Lexical Scoping 
### This code cash the inverse of a matrix to avoid costly computations.
### First makeCacheMatrix creates a special "matrix" object that can cache its inverse. 
### this allows cacheSolve to first check  If the inverse has already been calculated and returns the cached value,
### if not, cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.

## The makeCacheMatrix function creates a special "matrix" object called 'x' that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {                          #function that changes the matrix stored in the main function.
    x <<- y
    i <<- NULL                                  #changing to a new matrix restores 'i' to null (notifys that invers should be re-calculated)
  } 
  get <- function() x                           #returns the matrix 'x' stored in the main function
  setinverse <- function(inverse) i <<- inverse #store the value of the input in a variable 'i' into the main function makeCacheMatrix
  getinverse <- function() i                    #returns the stored cached value
  list(set = set, get = get,                    #store the 4 functions as a list in the function makeCacheMatrix
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()                           #verify if the value 'i' was stored by getinverse
  if(!is.null(i)) {                             #if it is not NULL, it was cached:
    message("getting cached data")              #returns a message and the value i
    return(i)                                   ## Return a matrix that is the inverse of 'x' (should be the inverse of i that was cached by getinverse)
  }                                             #if not the folloeing code will be executed:
  data <- x$get()                               #data gets the matrix stored with makeCacheMatrix
  i <- solve(data, ...)                         #'i' calculates the inverse of the matrix 
  x$setinverse(i)                               #x$setinverse(i) stores it in the object generated assigned with makeCacheMatrix
  i
        
}

## test case:
# n2<-matrix(c(1, 4, 0, 3), nrow=2)
