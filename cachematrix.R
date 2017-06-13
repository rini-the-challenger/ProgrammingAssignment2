## Put comments here that give an overall description of what your
## functions do

## Caching a matrix and retrieving the same

makeCacheMatrix <- function(x = matrix())
{
        
        invx <- NULL
        set <- function(y) 
        {
                x <<- y
                invx <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) invx <<- inv
        getInverse <- function() invx
        list(set = set, get = get,setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x = matrix, ...)
{
       ## Return a matrix that is the inverse of 'x'
        invx <- x[['getInverse']]()
        if(! is.null(invx)  ) 
        {
                message("getting cached data")
                return(invx)
        }
        else
        {
                data <- x[['get']]()
                invx <- solve(data)
                x[['setInverse']](invx)
                invx
        }
        
        
       
                
}