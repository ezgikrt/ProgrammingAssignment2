## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## This function creates a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  j <- NULL       ## I`ve initialize the j as NULL`
  set <- function(y){      ##I define a set function to assign new value
    x <<- y               ## value of matrix in parent environment  
    j <<- NULL             ## If there is a new matrix, it will give NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse   ##Assinging new values to j in parent environment
  getInverse <- function() j                     ## this is for getting the value of j where it is called 
  list(set = set, get = get,        ## in order to refer to the functions with the $ operator you need this
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function

##cacheSolve is a function which computes the inverse of the special 
##"matrix" returned by makeCacheMatrix above. If the inverse has already been calculated 
## and the matrix has not changed, it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}

