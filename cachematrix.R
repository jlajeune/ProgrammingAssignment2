##############################################################################
##
## The two functions, "makeCacheMatrix" and "cacheSolve", work together to 
## return the inverse of a matrix. The results after the first inverse
## calculation are cached to forego reprocessing and save computing time.
##
##  Jason LaJeunesse, 1/24/2015
##
##  Modeled after the "makeVector" and "cachemean" functions in the 
##  R Programming Course by Roger D. Peng, PhD, Jeff Leek, PhD, 
##  and Brian Caffo, PhD
##
##
##############################################################################


#####
## The "makeCacheMatrix" function initialize an object which contains:
##    1.) A matrix, 
##    2.) An inverse matrix placeholder (initialize to NULL)
##    3.) Get and Set functions for both the matrix and its inverse
##
#####

makeCacheMatrix <- function(x = matrix()) {
    ##Initialize the inverse of matrix x to NULL
    inv <- NULL
    
    ##function to set value of the matrix, and reset the inverse to NULL in case it's not already NULL
    set <- function(y){
      x <<- y
      inv <<- NULL
    }
    
    ##Get the current matrix
    get <- function() {x}
    
    ##Function to set the inverse of the matrix
    setinv <- function(inverse) {inv <<- inverse}
    
    ##Function to get the inverse of the matrix
    getinv <- function() {inv}
    
    ##Return list
    list(
      set = set,
      get = get,
      setinv = setinv,
      getinv = getinv
      )
}

#####
## [Works with the object returned from the "makeCacheMatrix" function]
## The function "cacheSolve" will return the inverse of a matrix by either:
##      1.) Returning the already cached inverse
##          OR
##      2.) Calculating the inverse if not cached, caching that value, and then returning the inverse
#####

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ##Get the currently stored inverse
  inv <- x$getinv()
  
  ##If the inverse matrix is already cached, return it
  if(!is.null(inv)){
    message("Returning data from cache...")
    return(inv)
  }
  
  ##Since the inverse isn't in cache at this point, solve for it and set it
  message("Inverse not cached, calculating inverse...")
  
  ##Get the matrix
  matrix <-x$get()
  ##Solve for the inverse of the square matrix
  inv <- solve(matrix)
  ##Set the inverse matrix back to x
  x$setinv(inv)
  
  ##Return the inverse
  inv
  
}
