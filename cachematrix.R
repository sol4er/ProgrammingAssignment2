
## This function has four function inside it: get, set, setInv and getInv.
## INPUT is any square and inverstible matrix
## After inverse matrix computed in catcheSolve function, it will store it

makeCacheMatrix <- function(x = matrix()) { ## get a square matrix assuming it is inversible
        InverseX <- NULL                    ## initialise the inverse matrix with NULL
        set <- function(y) {
                x <<- y             ## substitutes the matrix x with y (the input) in the main function (makeCacheMatrix)
                InverseX <<- NULL   ## restores to null the value of the inverse inverseX in set function
        }
        get <- function() x
        setInv <- function(inverse) InverseX <<-inverse ##store the value of the input in a variable inverseX into the main
        getInv  <- function() InverseX                  ##function makeCacheMatrix (setInv) and return it (getInv)
        list(set = set, get = get,
             setInv = setInv,   
             getInv = getInv)  
        
}



## This functtion will first verify the value inverseX exists and is not NULL.
## Then compute the inverse of the INPUT matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseX <- x$getInv()
        if (!is.null(inverseX)) { ## If inverseX exists in memory, it simply returns a message and the value of inverseX
                message("getting cached inverse of a matrix")
                return(inverseX)   
                
        } 
        else {
                inverseX <- solve(x$get()) ## inverseX calculates the inverse of the INPUT matrix 
                x$setInv(inverseX)  ## stores it in the object generated assigned with makeCacheMatrix
                return(inverseX)
        }
}
