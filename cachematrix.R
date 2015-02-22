## These functions provide inversing matrix 
## using cached data

## This function creates "improved" matrix

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverseMatrix <- function(InverseMatrix) im <<- InverseMatrix
    getInverseMatrix <- function() im
    list (set = set, get = get, 
          setim = setInverseMatrix,
          getim = getInverseMatrix)
}


## This function returns the inverse matrix for "improved" matrix x,
## considering cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    im <- x$getim()
    if (!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setim(im)
    im
}
