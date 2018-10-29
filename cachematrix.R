## I have, take the same structure as the example of makeVectore and cachemean just adapt it to the matrix case. The function solve was use
## to find the invers. It was supposed that the invers of the matrix exist that det(x) different of 0


## The first function create a "special" matrix it create the matrix, set the value of it, get the value, set the value of the invers 
## and then get it.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvers <- function(solve) m <<- invers
  getinvers <- function() m
  list(set = set, get = get,
       setinvers = setinvers,
       getinvers = getinvers)
}


## The second function check if the invers was already calculated and if it the case get this value. Otherwise it calcule the value of the 
## invers. 

cacheSolve <- function(x, ...) {
          m <- x$getinvers()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvers(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
