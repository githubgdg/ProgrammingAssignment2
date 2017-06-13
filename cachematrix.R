# R Programming
# Peer-graded Assignment: Programming Assignment 2: Lexical Scoping

## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly. makeCacheMatrix and cacheSolve are a pair of functions 
## that cache the inverse of a matrix.

## Test Result provided after code of cacheSolve.


## makeCacheMatrix is a function that creates a special "matrix" object 
## that can cache its inverse.
## This function creates a list containing a function to 
## set the value of the matrix, get the value of the matrix,
## set the inverse value of the matrix and get the inverse value of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        
        cache_local <- NULL			# Clear local cache for every new matrix value
        
        set <- function(y) {		        # Define function to store passed matrix
                cache_x <<- y			# Set the value of matrix
                cache_m <<- NULL		# Clear old inverse from the cache
        }
        
        get <- function() cache_x		# Define function to get the value of the matrix
        
        # Define function to set the inverse when there is no cached inverse yet
        setInverse <- function(cache_local) cache_m <<- cache_local
        
        
        getInverse <- function() cache_m	# Define function to get the inverse
        
        # Return a list of the four functions
        list(set = set, 
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve is a function that computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        cache_local <- x$getInverse() 	        # Get the cached value for the inverse
        
        if(!is.null(cache_local)) { 	        # If the cache is not empty
                message("Getting cached data.")
                return(cache_local)		# return the value from cache
        } else {message("Calculating Inverse value.")}
        
        # The cache is empty: calculate inverse, cache and return value.
        rawdata <- x$get()            	        # Get value of matrix
        result_data <- solve(rawdata) 	        # Calculate inverse
        x$setInverse(result_data)     	        # Cache the result
        result_data                   	        # Return the inverse to the caller
}

# Test Result, 2 matrix samples:

# > mx <- makeCacheMatrix()
# > mx$set(matrix(1:4,2,2))
# > mx$get()
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(mx)
# Calculating Inverse value.
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(mx)
# Getting cached data.
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5


# > mx$set(matrix(c(500,2,2,500),2,2))
# > mx$get()
#      [,1] [,2]
# [1,]  500    2
# [2,]    2  500
# > cacheSolve(mx)
# Calculating Inverse value.
#               [,1]          [,2]
# [1,]  2.000032e-03 -8.000128e-06
# [2,] -8.000128e-06  2.000032e-03
# > cacheSolve(mx)
# Getting cached data.
#               [,1]          [,2]
# [1,]  2.000032e-03 -8.000128e-06
# [2,] -8.000128e-06  2.000032e-03