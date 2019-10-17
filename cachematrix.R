##These functions will take a square matrix and find the inverse. 
##If the inverse was previously solved, it will find the inverse from the cache and return that value.

##The function below will create a special "matrix" object that will cache its inverse

makeCacheMatrix <- function(x = matrix()) {  
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
        
}

##The function below computes the inverse of the special "matrix" returned by makeCacheMatrix from above. 
##If the inverse was previously solved, then this cacheSolve function will retrieve this inverse value from the cache.

cacheSolve <- function(x, ...){  
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
} 



#Testing

#x <- matrix(1:4, nrow = 2, ncol=2) #sample matrix

#Testing Solve Function

#y <- matrix(c(-2, 1, 1.5, -0.5), nrow = 2, ncol = 2) #inverse of sample matrix
#identical(solve(x), y) #Should be TRUE

#inverse_x <- makeCacheMatrix(x)
#cacheSolve(inverse_x)
