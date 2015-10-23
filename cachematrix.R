##Prog. Assn #2
##Set up a matrix called "makeCacheMatrix"
##where the input is a variable matrix type
##this matrix can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
##this sets the value of the matrix, gets the value of the matrix

## Write a short comment describing this function
##sets the inverse of the matrix and gets the inverse of the matrix
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
        ## Return a matrix that is the inverse of 'x'
##Using the code below
##ProgAssn2matrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
##ProgAssn2matrix$get()
##cacheSolve(ProgAssn2matrix)