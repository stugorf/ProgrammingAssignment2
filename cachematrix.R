# Overall Description:
#   A pair of functions that create and cache the inverse of a matrix. The functions
#   take advantage of R's scoping rules to store/cache inverses of a supplied 
#   matrix once it has been computed and provides retrieval functions to reduce 
#   computation times rather than requiring inverses be calculated for each matrix.

makeCacheMatrix <- function(x = matrix()) {
    # Description:
    #   This function creates a special "matrix" object that can cache its inverse.
    # Args:
    #   x: a square invertabile matrix, default equals empty matrix 
    # Returns:
    #   a matrix and references to internal public functions

    unCachedMatrix <- x
    cachedMatrix <- NULL
    set <- function(y){
        cachedMatrix <<- NULL
    }
    get <- function() unCachedMatrix
    setCachedMatrix <- function(invertedMatrix) cachedMatrix <<- invertedMatrix
    getCachedMatrix <- function() cachedMatrix
    
    methods = list(set = set, get = get, setCachedMatrix = setCachedMatrix, 
         getCachedMatrix = getCachedMatrix)
}


cacheSolve <- function(x, ...) {
    # Description:
    #   This function computes the inverse of the "matrix" returned by makeCacheMatrix
    # Args:
    #   x: a square invertible matrix reference which was created using makeCacheMatrix
    #   ...: additional arguments
    # Returns:
    #   an inverted matrix OR if a cached matrix exist this function retrieves a
    #   cached inverted matrix
    
    cachedMatrix = x$getCachedMatrix()
    if(!is.null(cachedMatrix)){
        message("getting cached matrix")
        return(cachedMatrix)
    } else{
        message("cached matrix not found, solving inverse and cacheing")
        unCachedMatrix <- x$get()
        invertedMatrix <- solve(unCachedMatrix)
        x$setCachedMatrix(invertedMatrix)
        invertedMatrix
    }
}

### CODE BELOW FOR TESTING ###
#   Test making matrix
newMatrix <- matrix(c(-1, -2, 1, 1), 2,2)
m <- makeCacheMatrix(newMatrix)
m$get()

#   Test creating inverse matrix
inv <- cacheSolve(m)
inv

#   Test retrieving cached matrix
inv <- cacheSolve(m)
inv