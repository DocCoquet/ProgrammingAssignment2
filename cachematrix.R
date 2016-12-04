# Functions to Cache a special matrix,commpute the inverse of the special matrix and cache the inverse of matrix (until it 
# is replaced) to avoid repeated compuation



makeCacheMatrix <- function(x = matrix()) {            # Defines function with matrix defualt  
  
  invrs <- NULL                                        # Set initail value of inverse matrix as null
  
  set <- function(y) {                                 # Defines a function to set new matrix 
    x <<- y                                          # Superassigment of value in global enviroment
    invrs <<- NULL                                   # Sets inverse matrix to null for a new matrix
  }
  
  
  get <- function() x                                  # Defines function to return value of matrix
  setinverse <- function(inverse) invrs <<- inverse    # Supperassignment of inverse matrix value in Global Environment
  getinverse <- function() invrs                       # Returns value of inverse matrix when called
  list(set = set,                                      # Creates a list of functions to: (1) set
       get = get,                                      # and (2) call the Special Matrix; 
       setinverse = setinverse,                        # (3) set the inverse matrix and; 
       getinverse = getinverse)                        # (4) get the inverse martrix
  
}

cacheSolve <- function(x, ...) {                       # Function to to calaculate the inverse of special matrix
  
  invrs <- x$getinverse()                              # Gets value if inverse matrix exsist
  
  if(!is.null(invrs)) {                                # Check if inverse matrix exsits in cache
    message("getting cached data")
    return(invrs)                                    # Gets inverse matrix from Cache
  }
  # Caluating invser if it is not in Cache
  data <- x$get()                                      # Getting cached special matrix
  invrs <- solve(data, ...)                            # Determine inverse of special matrix
  x$setinverse(invrs)                                  # Sets inverse matrix
  invrs                                                # Retuns inverse matrxi 
  
}