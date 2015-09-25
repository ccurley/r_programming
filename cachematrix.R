# Coursera - Data Science - Introduction to R
# These functions calculate the inverse of a matrix and cache the result
# Using the function, if the inverse has already been calculated, the function
# will return the cached value instead of performing an expensive, unncessary
# recacluation.

# This function creates a special "matrix" object, which is really a list 
# containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        # create m to hold the cache matrix, set the intial value to NULL
        m <- NULL
        
        # 1. set the cached value of the matrix.
        # pass the input y into the function and assign it globally (<<-) to x and
        # set matrix m to NULL globally (<<-)
        set <- function(y) {
                x <<- y 
                m <<- NULL
        }
        
        # 2. get the value of the matrix
        # just return the matrix x when called
        get <- function() x
        
        # 3. set the cached value of the inverse i globally to seti - Set I
        seti <- function(i) m <<- i
        
        # 4. get the value of the inverse getu - Get I
        # to the inverse of the matrix x
        geti <- function() m 
        
        # and now spit 1, 2, 3, and 4 - set, get, seti, and geti out as a list object
        list(set = set, get = get, seti = seti, geti = geti)
}


## The following function calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse
## has already been caclulated. If so, it 'get's the inverse from the cache
## and skips the computation. Otherwise, it calculates the matrix inverse
## and sets the value of the inverse in the cache via the 'setinverse' function.

cacheSolve <- function(x, ...) {
        
        # Return the inverse matrix of x
        # If matrix m is NOT null, print a friendly message
        m <- x$geti()
        if(!is.null(m)) {
                message("Fetching cached matrix to save you time and money, because we care.")
                return(m)
        }
        
        # pass the matrix into an object called data, then pass the inverse into matrix m
        data <- x$get()
        m <- solve(data, ...)
        
        # set and pass matrix M out
        x$seti(m)
        m
}

# End of function

# x <- matrix(c(1, 0, 1, 5, -3, 1, 2, 4, 7), nrow = 3)
#
# > x
# [,1] [,2] [,3]
# [1,]    1    5    2
# [2,]    0   -3    4
# [3,]    1    1    7
#
# z <- makeCacheMatrix(x)
#
# > z$get()
# [,1] [,2] [,3]
# [1,]    1    5    2
# [2,]    0   -3    4
# [3,]    1    1    7
#
# > z$seti
# function(i) m <<- i
# <environment: 0x106e8e1c8>
#         > cacheSolve(z)
# Fetching cached matrix to save you time and money, because we care.
# [,1] [,2] [,3]
# [1,]  -25  -33   26
# [2,]    4    5   -4
# [3,]    3    4   -3
# 
# > solve(x)
# [,1] [,2] [,3]
# [1,]  -25  -33   26
# [2,]    4    5   -4
# [3,]    3    4   -3
