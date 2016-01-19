## These are functions that are able to cache the inverse of a matrix

## This function creates a special "matrix" that contains a list with the following functions
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse of the matrix
## 4) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {  ###Create a special matrix
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x           ###Get the matrix and putting in 'x'
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,   ###List that has several functions
       setinv = setinv,        ###set: Save the inverse that has been calculated  
       getinv = getinv)        ###get: Get the inverse, extract the inverse
}

## The following function calculates the inverse of the special "matrix" created with makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {    ###It calculates the inverse of the matrix and save it in the cache memory
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()                    
  if(!is.null(inv)) {                
    message("getting cached data")  ###If inv is there, it is not null, print message ("getting cache data") and return inv
    return(inv)                     ###Part1:  Get the inverse from the cache memory 
  }
  data <- x$get()                  ###Getting the data from the special matrix 
  inv <- solve(data, ...)          ###Part2:  Get the inverse of the matrix and sets the inverse in the cache memory
  x$setinv(inv)
  inv                              ###Print inv
}









