## Together, these two functions calculate and cache (store) the inverse of a 
## matrix. Why? Because calculating the inverse of a matrix is time-consuming 
## so if you need to do this repeatedly (e.g., in a loop) it is more efficient 
## to do it once, store the calculated values, and simply recall those values.

## The first function creates the inverse matrix cache.

makeCacheMatrix <- function(x = matrix()) { 
        inv_mat <- NULL
        set <- function(y) {
                x <<- y
                inv_mat <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv_mat <<- inverse
        getinv <- function() inv_mat
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The second function submits a matrix to the first function and calculates the 
## inverse, but only if it has not occurred previously. In this case, it 
## retrieves the calculated inverse matrix.

cacheSolve <- function(x, ...) {
        inv_mat <- x$getinv()
        if(!is.null(inv_mat)) {
                message("retrieving cached matrix")
                return(inv_mat)
        }
        data <- x$get()
        inv_mat <- solve(data, ...)
        x$setinv(inv_mat)
        inv_mat
}

## TESTING/USAGE INSTRUCTIONS
## To begin you need an invertible matrix. You can create one for testing 
## purposes like this:
##
## my_test_matrix <- matrix(1:4,2,2)
## 
## To run the whole thing, (1) create an object to contain the inverse matrix  
## variables and run makeCacheMatrix on your matrix:
##
## test = makeCacheMatrix(my_test_matrix)
##
## Then, (2) run cacheSolve on your inverse matrix object:
##
## cacheSolve(test)
##
## Finally, (3) run it again to retrieve your cached matrix
##
## cacheSolve(test)