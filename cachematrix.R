## "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix
## makeCacheMatrix is a function which creates a special "matrix" object that can 
## cache its inverse for the input (which is an invertible square matrix)


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

## cacheSolve is a function which computes the inverse of the special "matrix"       
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache

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
