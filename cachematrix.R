## Put comments here that give an overall description of what your functions do
## R Programming Week 3 Assignment: Caching the Inverse of a Matrix
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                  ## initialize inv as NULL; will hold value of matrix inverse
  set <- function(y) {         ## set the value of the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x          ##get the value of the matrix
  setinverse <- function(inverse) inv <<- inverse  ##set the value of the inverse
  getinverse <- function() inv                     ##get the value of the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()                      ##checks to see if the inverse has already been calculated
  if(!is.null(inv)) {                        ##if exist, gets the inverse from the cache and skips the computation
    message("getting cached data")              
    return(inv)
  }
  matrix_to_invert <- x$get()                ##Otherwise, it calculates the inverse and 
                                             ##sets the value of the mean in the cache via the setinverse function
  inv <- solve(matrix_to_invert, ...)
  x$setinverse(inv)
  inv		
}

my_Matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_Matrix$get()
my_Matrix$getinverse()
cacheSolve(my_Matrix)
cacheSolve(my_Matrix)
