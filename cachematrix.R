## makeCacheMatrix and cacheSolve can be used to store a matrix,
## calculate its inverse value, then store that inverse in a variable cache.
## The purpose would be to avoid re-calculating the inverse unnecessarily.

## makeCacheMatrix can be used to get and set two matrix values
## if someone inverts the second value before storing it, then it will
## hold the inverse of the source matrix.

makeCacheMatrix <- function(x) {
        #module level variables
        m.matrix <- x
        m.matrix.inverse <- NULL
        
        #create set function
        set <- function(y) 
                {
                        #update the module-level variables
                        m.matrix <<- y
                        m.matrix.inverse <<- NULL
                }
        
        #create get function
        get <- function() m.matrix
        
        #create setinverse function
        setinverse <- function(inv) m.matrix.inverse <<- inv
        
        #create getinverse function
        getinverse <- function() m.matrix.inverse
        
        #store the functions
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve:  Given an object that supports storing cached matrices, see if
## the inverse of the matrix has already been cached.  If so, return it
## if not, perform the calculation, then return the result.

cacheSolve <- function(x, ...) {
        #store the contents of getinverse from the object passed as x
        m.matrix.inverse <- x$getinverse()
        
        #if there is already a cached mean value, return it and exit
        if(!is.null(m.matrix.inverse)) 
        {
                message("getting cached data")
                return(m.matrix.inverse)
        }
        
        #no cached inverse value found - grab the vector from our object
        data <- x$get()
        
        #now calculate the inverse - passing any options 
        m.matrix.inverse <- solve(data, ...)
        
        #now set the value on the source object
        x$setinverse(m.matrix.inverse)
        
        #return the value that was stored
        m.matrix.inverse        
}
