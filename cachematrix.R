#makeCacheMatrix creates a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of the matrix
# get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <-function(y){
    x<<-y
    inv <<- NULL
  }
  
  get<-function() x
  setInverse<-function(inverse) inv <<-inverse
  getInverse<-function() inv
  list(set = set, get = get,
    setInverse= setInverse,
    getInverse=getInverse)
  }

#cacheSolve returns the inverse of the matrix 
#It checks if the inverse has been computed
#If so, it gets the result and skips the computation
#If not, it computes the inverse, sets the value in the case via setInverse function

cacheSolve <- function(x, ...) {
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <-x$get()
  inv<-solve(data)
  x$setInverse(inv)
  inv
}


