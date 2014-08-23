## Programming Assignment 2: Lexical Scoping

## makeCacheMatrix() creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        #Inverse is set to NULL everytime makeCacheMatrix() is called
        inv <- NULL 
        
        
        #"set" is per se not needed, but can be used to change the matrix and in this case setting "inv" back to NULL
        set <- function(y) { 
                x <<- y
                inv <<- NULL
        }
        
        #function to read matrix:+
        get <- function() x 
        
        
        #function to set the inverse matrix "inv" into the cache
        setinverse <- function(inverse) inv <<- inverse 
        # function to load the inverse matrix "inv" from the cache
        getinverse <- function() inv 
        
        #the output vector, a list of above described functions
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 

        
}


## cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        #Getting the potential Inverse (OR a "NULL" value) from the cache and calling it "inv":
        inv <- x$getinverse()
        
        #Is there already an Inverse "inv" that was be calculated? Then return "inv" and abort function
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        #Otherwise the inverted matrix has to be caluclated.
        #Getting the Data set:
        data <- x$get()
        #Calculating the inverted matrix:
        inv <- solve(data, ...)
        #Setting the inverse into the cache
        x$setinverse(inv)
        #Setting old data into cache
        x$set(data)
        #Returning the matrix "inv" which is the inverse of "x"
        inv

}
