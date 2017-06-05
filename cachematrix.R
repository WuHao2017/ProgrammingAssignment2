## Put comments here that give an overall description of what your
## functions do

        


## Write a short comment describing this function
        ## The first function named makeCacheMatrix creates an object 
        ## which stores a matrix and its reverse matrix. 
        ## It defines a set of functions and returns the functions in 
        ## a list to the parent environment.
        ## Please see more detail explanation in the code of function.

makeCacheMatrix <- function(x = matrix()) {
        ## Set the inverse object to NULL.
        ## At the beginning of this function, the initial value for inverse
        ## object must be set to NULL. It is intented to make sure the
        ## initial value for reverse matrix does not has any unexpected value
        ## for whatever reason.
        inverse <- NULL
        
        ## Define a function which set the value for the matrix which need to calculate
        ## the reverse matrix. After setting the value for matrix, clear previous result
        ## in the cache for the reverse matrix.
        ## Please be aware of that this function provide another option to reset the
        ## value of the matrix which need to calculate the inversal matrix without 
        ## recall the whole makeCacheMatrix function.
        set <- function(y)      {
                x <<- y
                inverse <<- NULL
        }
        
        ## Define a function which retrieve the existing value for the object from the 
        ## parent envirnment. It get the value from the previous excution of 
        ## makeCacheMatrix function or set funciton.
        get <- function() x
        
        ## Define a function which will set the value of reversal matrix into the cache
        ## after the caculating of reversal matrix.
        ## Please be aware that the reversal calculation will not performed here. It is
        ## only assign the input argument to the value of inverse in the parent environment
        ## through using <<- operator.
        setinverse <- function(solve) inverse <<- solve
        
        ## Define a function which get the value of inversal matrix from the cache. 
        getinverse <- function() inverse
        
        ## Assign previous 4 defined functions as elements in a list and return their values
        ## to the parent environment.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
        ## The second function name cacheSolve retrieve the reverse 
        ## matrix from the cathe if the reverse calculation for that 
        ## matrix was excuted before. If not, this the function will 
        ## caculate the reverse matrix.
        ## Please see more detail explanation in the code of function.

cacheSolve <- function(x, ...) {
        ## Get the value of inversal matrix value from the cache
        ## After first excuting function makeCacheMatrix, there will be
        ## value in x$getinverse(), even it is NULL
        inverse <- x$getinverse()
        
        ## If the value for inversal matrix in cache is not NULL, 
        ## return the value in cache 
        if(!is.null(inverse))       {
                message("getting cached inversal matrix data")
                return(inverse)
        }
        
        ## If the value for the inversal matrix in cache is NULL,
        ## use the input matrix data to calculate the inversal 
        ## matrix. After that put the inversal matrix into the cache.
        matrixdata <- x$get()
        inverse <- solve(matrixdata, ...)
        x$setinverse(inverse)
        inverse
}
