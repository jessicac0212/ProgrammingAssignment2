## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function creates a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse_x<-NULL
        set <- function(y){
                x <<- y
                inverse_x<<-NULL
        }
        get <- function() x
        setInverse <- function(in_x) inverse_x<<-in_x
        getInverse <- function() inverse_x
        list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function
## this function calculates the inverse of the "matrix"
## it first checks whether the inverse is already been calculated
## if so, it gets the inverse from the cache and skips the computation
## if not, it calculates the inverse of the matrix and sets the value 
## to the inverse_x in the cache via setInverse function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_x<-x$getInverse()
        if(!is.null(inverse_x)){
                message("getting cached data")
                return(inverse_x)
        }
        data <- x$get()
        inverse_x <- solve(data,...)
        x$setInverse(inverse_x)
        inverse_x
}
