##Pair of function to cache the inverse of a given matrix
##that we can retrieve easilye from cache

##This function was made to caching the inverse of a matrix "x"
makeCacheMatrix <- function(x = as.matrix()) {
        inverse <- NULL
        
        ##a function to set a value to "x"
        set <- function(y = as.matrix()) { ##"y" must be a matrix, to fix in x original matrix
                x <<- y
                inverse <<- NULL  ##because we didn't calculate the inverse,
                                  ##but we can fix it doing inverse<<-solve(x)
        }
        
        ##a function to retrieve "x" value
        get <- function() x
        
        ##a function to set  a new value for the inverse of "x" by solve() function
        setinverse <- function(newinverse) inverse <<- newinverse
        
        ##a function to retrieve the value of the inverse of "x"
        getinverse <- function() inverse
        
        ##retur a list with the function to access the data
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##This function recover the cached inverse of matrix "x" from makeCacheMatrix

cacheSolve <- function(x, ...) {
        ##first, it check if the inverse it's already calculated
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached inverse of your matrix")
                return(inverse)
        }
        ##if there is not inverse calculated, the we calculate it
        oldmatrix <- x$get()
        inverse <- solve(oldmatrix, ...)
        x$setinverse(inverse)   ##cache the inverse calculated
        inverse                 ##return the inverse
}
