## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function was made to caching the inverse of a matrix "x"
makeCacheMatrix <- function(x = matrix()) {
        inverse<-NULL
        set<-function(y){
                x<<-y
                inverse<<-NULL
        }
        get<-function() x
        setinverse<-function(solve) inverse<<-solve
        getinverse<-function() inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## Write a short comment describing this function

##This function recover the cached inverse of matrix "x" from makeCacheMatrix
##but first, it check if the inverse it's already calculated

cacheSolve <- function(x, ...) {
        inverse<-x$getinverse()
        if(!is.null(inverse)){
                message("getting cached inverse of your matrix")
                return(inverse)
        }
        alldata<-x$get()
        inverse<-solve(x, ...)
        x$setinverse(inverse)
        inverse
}
