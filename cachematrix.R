# Function to create an object to hold matrix and cache its inverse
# Note: *the supplied matrix must be invertible*
# Usage example:
#       #make a matrix
#       x <- makeCacheMatrix(matrix(1:100, nrow=10, ncol=10))
#       cacheSolve(x) #solve for the first time
#       cacheSolve(x) #solve again; returns from cache

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {            #set the value of the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x             #get the value of matrix
        setinverse <- function(solve) m <<- solve    #set the value of inverse
        getinverse <- function() m                   #get the value of inverse                  
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# function to return inverse of the matrix but if the inverse is
# already calculated, then it retrives the inverse from cache

cacheSolve <- function(x, ...) {
        #return inverse of the matrix
        minv <- x$getinverse()
        if(!is.null(minv)) {
                message("retriving cached data") #return message to console
                return(minv)
        }
        mat <- x$get()
        minv <- solve(mat, ...)
        x$setinverse(minv)
        minv #return inverse
}
