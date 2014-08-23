## The makeCacheMatrix creates an object that stores a matrix and its inverse
## in cache. Use cacheSolve to compute the inverse. If it is not in chache, it
## will compute it and stores it there. 
## Examples:
## mtx<-makeCacheMatrix(matrix(1:4,nrow=2,ncol=2))
## cacheSolve(mtx)
## mtx$set(matrix(c(3,8,9,1,24,1,23,89,43,23,12,5,67,12,34,1),nrow=4,ncol=4))
## cacheSolve(mtx)
## mtx$getinverse()

## This function creates a special "matrix" object that can cache its inverse.
## It returns a list with four functions:
## set - set a matrix to the special "matrix" object
## get - get the current matrix
## setinverse - set the inverse of the matrix
## getinverse - get the inverse of the matrx

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) { #set matrix
        x <<- y          #save matrix x in global environment
        i <<- NULL       #reset inverse matrix
    }
    get <- function() x  #get matrix
    setinverse <- function(inverse) { #set inverse matrix
        i <<- inverse    #save inverse matrix in global environment.
    }                    
    getinverse <- function() i #get inverse matrix
    #return list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()   #Check if the inverse is in cache
    if(!is.null(i)) {     
        message("getting cached data")
        return(i)
    }
    data <- x$get()       #Otherwise, get the matrix,
    i <- solve(data)      #compute its inverse,
    x$setinverse(i)       #set it in cache,
    i                     #and return value.
}
