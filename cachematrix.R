## Function makeCacheMatrix creates a matrix and defines the set & get functions


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## Initialize m
  set <- function(y) ## This set function when called will Set values in the matrix
  {
    x <<- y ## Assigning value to variable x, which exists in the parent function
    m <<- NULL
  }
  
  get <- function() x ##'get' function
  
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)  
}


## 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getsolve()
  if(!is.null(m))
  {
    message("getting cached data")  ##If no value is reassigned to the matrix, print message
    return(m) ##then return the value of m  
  }
  
  data <- x$get() ##In case a new value is assigned to the matrix 
  m <- solve(data,...)  
  x$setsolve(m)
  m
}
