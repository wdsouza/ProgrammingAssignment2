## The following R code contains two functions: makeCachematrix and
## cacheSolve to create a special object that stores a numeric invertible
## matrix and cache's its inverse.

## The function makeCacheMatrix creates a special matrix, which includes 
## (1) setting the value of the matrix, (2) getting the value of of the 
## matrix, (3) setting the value of the matrix inverse and getting the value
## of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
            m<-NULL
            set<-function(y) {
                    x<<-y
                    m<<-NULL
            }
            
            get <- function() x
            setinverse <- function(solve) m<<-solve
            getinverse <- function() m
            
            list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The function cacheSolve calculates the inverse of the special "matrix" 
## created with the above function. It first checks to see if the inverse 
## has already been calculated. If so, it gets the inverse from the cache 
## and skips the computation. Otherwise, it calculates the inverse of the 
## matrix and sets the value of the inverse in the cache via the setinverse 
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
  
}
