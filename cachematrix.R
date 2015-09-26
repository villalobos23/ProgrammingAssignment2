##These functions create a list where some matrix is used to generate
##A data structure with functions that maintain data regarding the inverse of the 
##matrix, this matrix is always a squared invertible matrix. 

## This function creates a data structure based in functions
## with the purpose of maintaining and using previously calculated
## inverse operations over a matrix. Based on a matrix it wraps it 
## in a functional envelope, it separates data from behaviour.
## Returns a list with the diferent behaviours around the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseX <- NULL
  ##Method for setting the matrix of the inverse to be cached
  ##If the matrix is changed and the method is used then the inverse is set to null
  ##since it is required to be made for the new matrix.
  set <- function(y){
    x <<- y
    inverseX <<- NULL
  }
  #Oneliner it returns the last matrix that was set using the set function
  get <- function() x
  #It sets a matrix as the inverse of x, 
  # TODO shouldn't it calculate x also?
  setInverse <- function(inverseM) inverseX <<- inverseM
  #function to return the last calculated inverse of x
  getInverse <- function() inverseX
  #Returns the list of behaviours associated with the operation
  list(set = set,
       get = get,
       setInverse = setInverse, 
       getInverse = getInverse)
}


## This functions checks if the inverse has already been obtained and returns it, 
## if it has not been calculated, it performs the calculations and stores it in the data structure
## that is created with the makeCacheMatrix function. x is a data structure created with that function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse.X <- x$getInverse()
        #If the inverse has been calculated return the cached value
        if(!is.null(inverse.X)){
          message("using cache values")
          return (inverse.X)
        }
        #else, compute it, store it in cache and return. 
        message("calculating inverse")
        matrix.x <- x$get()
        inverse.X <- solve(matrix.x)
        x$setInverse(inverse.X)
        inverse.X
}
