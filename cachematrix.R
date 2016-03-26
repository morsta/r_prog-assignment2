
## makeCacheMatrix returns a list of functions defined in the makeCacheMatrix function body. 
## each "CacheMatrix-object" holds a Matrix and its invers.



makeCacheMatrix <- function (X = matrix()){
  Inv <- NULL
  set <- function(Y){
     X <<- Y
     Inv <<- NULL
  }
  get <- function() X
  setinvers <- function(Invers) Inv <<- Invers
  getinvers <- function() Inv
  list(set = set, get = get, setinvers = setinvers, getinvers = getinvers)
}

## cacheSolve returns the inverse of a (invertible) matrix. First the function first looks up if the inverse has already been computed.
## If so, the before calculated value. If not, it computes the inverse, then stores by calling the setinvers() function and finally returns the inverse.

cacheSolve <- function(X, ...){
  Inv <- X$getinvers()
  if(!is.null(Inv)){
    message("getting cached data")
    return(Inv)
  }
  data <- X$get()
  Inv <- solve(data)
  X$setinvers(Inv)
  Inv
}