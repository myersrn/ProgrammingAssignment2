##### makeCacheMatrix is a function that takes a matrix as an input argument
##### and returns a list of 4 functions.
####
####            1. The set function allows the value of the matrix to be assigned to a variable that
####            be used by the environment that called makeCacheMatrix.
####
####            2. The get function will simply return the matrix value assigned passed as
####            an input variable to makeCacheMatrix.
####
####            3. The setinv function allows the value of the inverse of the matrix to be 
####            assigned to a variable that be used by the environment that called makeCacheMatrix.
####
####            4. The getinv function will simply return the inverse of the matrix value assigned
####            to the variable m_inv.
####
####            The final statement returns the list containing these 4 function.

makeCacheMatrix <- function(x = matrix()) {
        m_inv <- NULL
        set <- function(y) {
                x <<- y
                m_inv <<- NULL
        }
        get <- function() x
        setinv <- function(solved) m_inv <<- solved
        getinv <- function() m_inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


####    cacheSolve takes the list returned by makeCacheMatrix, checks to see if the inverse of the
####    matrix it contains has been solved. If it has already been solved, it returns the inverse
####    of the matrix already cached. If it has not been solved, the cached matrix is obtained
####    using the makeCacheMatrix get function, the inverse of this matrix is computed, 
####    the setinv function is used to assign the value of of the inverse of the matrix, and
####    the inverse of the matrix is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m_inv <- x$getinv()
        if(!is.null(m_inv)) {
                message("getting cached data")
                return(m_inv)
        }
        data <- x$get()
        
        ##solve(x) will compute the inverse of a square matrix
        m_inv <- solve(data, ...)
        x$setinv(m_inv)
        m_inv
}
