## makeCacheMatrix have to be called in sequence, one caching the matrix to be inversed,
## and creating all the functions cacheSolve needs to inverse the matrix

## makeCacheMatrix creates a list of functions, which 
## 1. cache the values of a matrix that is to be inversed,
##    this function is called when makeCacheMatrix is called
## 2. get the data of this matrix
## 3. cache the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix(1:4,2,2)) {

  m <- NULL
  set <- function(x) {
    x <<- x
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve takes the list of functions created in makeCacheMatrix and inverses 
## the matrix cached in makeCacheMatrix with the help of those functions
## However, cacheSolve only inverses the function, if there is not already a cached 
## version of the inverse of the original matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve() ## get the cached data for the inverse of x, if there is any
  if(!is.null(m)) { ## if there is cached data, this function puts out a message 
                    ## and gets the cached data
    message("getting cached data")
    return(m)
  }
  data <- x$get()   ##otherwise the inverse is calculated
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

