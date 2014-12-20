## Matrix caching function that stores computations on a matrix that are expensive to compute
##   The stored computations can be returned immediately if needed again, and recomputed if the matrix changes

## For ProgrammingAssignment2 in the R Programming coursera course


## Construct an object to store a matrix and its cached computed values
makeCacheMatrix <- function(x = matrix()) {
  # mInverse will hold the computed inverse of the matrix x
  # mInverse is NULL when the inverse has yet to be computed for x
  mInverse <- NULL
  
  # Setter function, to set the matrix x
  set <- function(y) {
      # Check to see that y is a matrix
      if (class(y) == "matrix") {
         # superassignment operator, set x in the enclosing environment to y
         x <<- y
         # Since we set x to a new matrix, the inverse will have to be recomputed
         mInverse <<- NULL
      } else {
         # Tried to set x to a non matrix
         print("Argument to set must be a matrix! Got a ",class(y))
         # count throw an error here, but we'll leave that for later
      }
  }
  
  # Simple getter for the matrix being stored, this just returns the matrix x
  get <- function() x
  
  # Set the matrix inverse cached value
  setInverse <- function(mi) {
     # Make sure we're at least setting a matrix
     if (class(mi) == "matrix") {
         # No checking here to see if mi is really the inverse of x
         # We'll just assume the caller of this function knows what he/she is doing
         mInverse <<- mi
     } else {
        # Tried to set mInverse to a non-matrix!
        print("Argument to setInverse must be a matrix! Got a ",class(mi))
     }
  }
  
  # Get the matrix inverse.  This will NOT calculate the matrix inverse if it has not been calculated yet
  getInverse <- function() mInverse
  
  # Return a list of the defined inner functions
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Return the inverse of matrix x.  
## If the inverse has already been calculated, return the cached value immediately
## If not, perform the compute intensive calculation, store the computed value, and return it

## Any extra parameters beyond x will be passed to the matrix inverse function, solve
cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
     
   # No error checking, we'll assume x is a makeCacheMatrix.  If not, errors will be thrown
   mInv <- x$getInverse()
   # mInv either holds the cached inverse matrix, or NULL if we need to calculate it
   if (!is.null(mInv)) {
       # Not null, so it's the cached value.  We're done
       message("returning cached matrix inverse")
       return(mInv)
   }
   
   # We need to calculate the inverse of x. First get the matrix stored in x
   m <- x$get()
   
   # m is the matrix we need to compute the inverse of
   # Pass along any extra arguments to solve
   # We are assuming the matrix is invertable
   mInv <- solve(m, ...) 
   
   # Now mInv holds our inverse, store it in x as the cached value
   x$setInverse(mInv)
   # and return the inverse
   mInv
}

## Example of usage
# > source("cachematrix.R")
# > mcm <- makeCacheMatrix()
# > mcm$set(matrix(c(1,2,3,4), nrow=2, ncol=2))
# > cacheSolve(mcm)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> cacheSolve(mcm)
#returning cached matrix inverse
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> 