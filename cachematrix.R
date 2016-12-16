## Enhanced performance Set&Get&Calculate Inverse of a matrix object.


## Function to define the matrix with that can cache the Inverse of it

makeCacheMatrix <- function(x = matrix()) {
        ##definition of null InverseVariable to be used.
		invVariable <- NULL
        set <- function(y) {
                x <<- y
                invVariable <<- NULL
        }
        get <- function() x
        setInverse <- function(passedInverse) {
			invVariable <<- passedInverse
			}
        getInverse <- function() {
			invVariable
			}
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Function to get the Inverse of the matrix for the first time and save it in invVariable to be used from cache after wards.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invVariable <- x$getInverse()
        if (!is.null(invVariable)) {
                message("getting cached data")
                return(invVariable)
        }
        matrixVariable <- x$get()
        invVariable <- solve(matrixVariable, ...)
        x$setInverse(invVariable)
        invVariable
}