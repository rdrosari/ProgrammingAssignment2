##Function that creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setmatrix <- function(matrix) m <<- matrix
   getmatrix <- function() m
   list(set = set, get = get,
        setmatrix = setmatrix,
        getmatrix = getmatrix)
}

##Function that computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
   m = x$getmatrix()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   m <- try(solve(data, ...), silent = TRUE)
   
   ## Return a matrix that is the inverse of 'x'
   if(is.matrix(m)) {
      x$setmatrix(m)
      m
   } else
      print("Unable to get the inverse matrix , please enter a new array")
}