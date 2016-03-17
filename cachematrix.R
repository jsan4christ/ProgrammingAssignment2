## The functions makeCacheMatrix and cacheSolve take a matrix, and generate its
## its inverse. The inverse is cached such that if a call is made to generate the
## inverse of the same matrix, it is not recomputed, saving on time and CPU resources

## The makeCacheMatrix function creates a matrix object takes a matrix, x and 
##sets up the functions necessary to access and set its inverse. ##
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set,
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function checks if the exists an inverse matrix for x in the cache and 
##returns it and computes it, if it does not exist.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv) #Exit cacheSolve returning the cached value if exists
      } 
      
      matx <- x$get() #else, call the get function to pick up the matrix
      inv <- solve(matx, ...) #use the solve functioncompute inverse of the matrix x
      x$setinverse(inv) #
      inv}

#Function to test efficiency of caching
# test = function(x){
#       ## @x: an invertible matrix
#       
#       temp = makeCacheMatrix(x)
#       
#       start.time = Sys.time()
#       cacheSolve(temp)
#       dur = Sys.time() - start.time
#       print(dur)
#       
#       start.time = Sys.time()
#       cacheSolve(temp)
#       dur = Sys.time() - start.time
#       print(dur)
# }
