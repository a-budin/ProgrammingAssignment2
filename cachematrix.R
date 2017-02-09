## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  # initialize the inverse matrix  during the first call to makeCacheMatrix
  inverse_matrix <- matrix() 
  
  # function to set new values for the underlying matrix
  set <- function(y) 
  {
    x <<- y
  }
  
  # getter function for underlying matrix
  
  get <- function()
  {
    x
  }
  
  # set the inverse of the matrix x.
  setinverse <- function(inverse = matrix()) 
  {
    inverse_matrix <<- inverse
  }
  
  # returns the inverse_matrix.  Will be null if setinverse has not been called or
  # if set is called after the last call to setinverse
  getinverse <- function() 
  {
    inverse_matrix
  }
  
  # return value of the makeCacheMatrix function is a list
  # of functions (and variables if we wish) that we want to expose
  # as public.  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inverse_matrix <- x$getinverse()
  
  # if we've already computed the inverse and stored it via setinverse(),
  # and have not invalidated the cache by calling set(), return the cached
  # version of x
  if(!all(is.na(inverse_matrix))) 
  {
    message("getting cached data")
    # we have to explicitly use return here otherwise we'd keep
    # executing the code after the if conditional ends.  
    return(inverse_matrix)
  }
  
  # otherwise either we havent computed the cached version yet, or we've called
  # set() previously and invalidated the cache.
  
  # call get() to get the underlying matrix
  data <- x$get()
  
  # calculate the inverse matrix of the underlying vector, passing with it
  # any varargs passed to cacheSolve
  inverse_matrix <- solve(data)
  
  # now set the inverse in x so we cache it and don't need to needlessly
  # recompute it
  x$setinverse(inverse_matrix)
  
  # return the caching vector
  inverse_matrix
}

