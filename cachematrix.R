makeCacheMatrix <- function(x = matrix()) {
        
        # Constructs setters (provide caching of the inverse) 
        #  and getters for a matrix and its inverse
        # Returns the setters and getters
        
        inversedMatrixCached <- NULL
        
        # set matrix
        setMatrix <- function(matrix) {
                x <<- matrix
                inversedMatrixCached <<- NULL
        }
        
        # get matrix
        getMatrix <- function() x
        
        # set inversed matrix
        setInversedMatrix <- function(inversedMatrix) 
                inversedMatrixCached <<- inversedMatrix
        
        # get inversed matrix
        getInversedMatrix <- function() inversedMatrixCached
        
        # return list of functions
        list(setMat = setMatrix, getMat = getMatrix,
             setInvMat = setInversedMatrix,
             getInvMat = getInversedMatrix)
        
}

cacheSolve <- function(x, ...) {

        # Computes the inverse of a matrix 'x' 
        #  (if not present in cache)
        # Stores the computed inverse in cache
        # Returns the computed inverse
        
        # check for cached inversed matrix
        inversedMatrix <- x$getInvMat()
        
        # check if inversed matrix cache is present
        if(!is.null(inversedMatrix)) {
                message("getting cached data")
                return(inversedMatrix)
        }
        
        # get matrix for inversion
        matrix <- x$getMat()
        
        # compute inversed matrix
        inversedMatrix <- solve(matrix, ...)
        
        # set inversed matrix for chaching
        x$setInvMat(inversedMatrix)
        
        # return inversed matrix
        inversedMatrix
        
}
