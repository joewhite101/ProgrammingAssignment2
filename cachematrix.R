# The two functions below can be used to calculate and store the inverse of a matrix. Example usage:
# 1. Create our cache matrix object: cached <- makeCacheMatrix()
# 2. Set our data to calculate the inverse of: cached$set(matrix(1:100,10,10))
# 3. Use the cacheSolve function to get the inverse: cacheSolve(cached)
#
# Running the command in step 3 without changing the data will provide a message of using the cached value.

#
# This function creates a special matrix that attaches functions to:
# 1. Set the value of the vector
# 2. Get the value of the vector
# 3. Set the value of the inverse
# 4. Get the value of the inverse
#
# R scoping rules are used to allow a user of the function to set the value of the inverse to be cached. 
# The cached value is stored in the "inv" variable in the function below
#
makeCacheMatrix <- function(x = matrix()) {

    # The inv variable will be used to store the cached inverse once calculated
    inv <- NULL
    
    # Users of the makeCacheMatrix function will provide the input matrix using this set function. Once set is called
    # the cached value (inv) is cleared so that it is recalculated with the new data
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    
    # Return the original matrix. Allows for validation that the matrix has not changed
    get <- function() x
    
    # Once the user of this function has solved for the inverse they cache it with this setinverse function
    setinverse <- function(solved) inv <<- solved
    getinverse <- function() inv
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# 
# This function takes as an argument a matrix (x) and returns the inverse of that matrix. If the 
# inverse of the matrix had been calculated previously this function will return the cached inverse of the matrix
# to save the computational cost of calculating that matrix. See commens on makeCacheMatrix above for details
# on the caching mechanism.
#
cacheSolve <- function(x, ...) {
     
  # Check to see if the matrix has a cached inverse   
  inv <-x$getinverse()
  
  # Check to see if the inverse was previously calculated and that the matrix data has not changed. 
  # The checks on dim(x) == dim(x$get()) && all(x == x$get()) ensure that the data is the same and has not
  # changed since the value was cached.
  #
  if(!is.null(inv)){
      message("Getting cached matrix invers")  
      return(inv)
  }
  
  # In the case where the inverse was not cached then we get the data and calculate the inverse
  data <-x$get()
  inv <- solve(data, ...)
  
  # Cache the inverse for later use and return it to the caller
  x$setinverse(inv)
  inv
}
