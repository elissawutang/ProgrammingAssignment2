## makeCacheMatrix and cacheSolve are functions that set up data structures to easily get (retrieve), set (input), get and solve 
## the inverse of the matrix that is passed into the function

## makeCacheMatrix is a function that sets up a data structure that will get and set the inverse of a matrix (input) when used with the
## $ operator

makeCacheMatrix <- function(x = matrix()) {
            inverse <- NULL  
    set <- function(new.matrix) { 
                        ##y = new value for "x" that isnt the og matrix/vect     
        matrix <<- new.matrix ## passing in new.matrix to not confused with the original matrix argument 
        inverse <<- NULL ##resets to the current/new inverse so r won't confuse with an old inverse  
    }
    get <- function() matrix 
    set.inverse <- function(new.inverse) inverse <<- new.inverse 
    get.inverse <- function() inverse
    list(set = set, get = get, 
        set.inverse = set.inverse, 
         get.inverse = get.inverse)

}


## the function cacheSolve will set up a data structure that will automatically return the inverse if the inverse is a real value. 
## otherwise, cacheSolve will manually find the elements in matrix to solve for the new inverse  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'matrix'
    inverse <- matrix$get.inverse() ##the cached inverse from makeCacheMatrix 
    if(!is.null(inverse)) { 
        message("getting cached data") 
        return(inverse) 
    }
    data <- matrix$get() 
    new.inverse <- solve(data) ##manually do the inverse if the previous if argument fails
    matrix$set.inverse(new.inverse) 
    new.inverse 
}
