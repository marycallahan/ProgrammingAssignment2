## Together, these functions 1) make the function to create the inverse of a matrix and store it as
## a list so you can 2) compute the inverse of the a matrix later by calling the function in part 1.

## makeCacheMatrix will store the function that can make the inverse of a 
## matrix as a list so it can be used later, i.e. caches itself


makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y) {
        x<<-y
        inverse<<-NULL
    }
    get<- function() x
    setinverse<-function(inverse) inv<<-inverse
    getinverse<-function()inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
      
}


## cacheSolve will compute the inverse of the matrix stored above by calling a specific component
##of makeCacheMatrix 

cacheSolve <- function(x, ...) {
        inv<-x$getinverse()
        if(!is.null(inv)){
            message("Getting cached data")
            return(inv)
        }
        matrix<-x$get()
        inv<-solve(matrix, ...)
        x$setinverse(inv)
        inv
}
