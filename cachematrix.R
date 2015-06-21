# this function works like a class, it creates a list
# that contains 4 member functions: set, get, setInverse
# and getInverse.

# The operators <- and = assign into the environment in which they are
# evaluated. The operator <- can be used anywhere, whereas the operator
# = is only allowed at the top level 

# The operators <<- and ->> are normally only used in functions,
# and cause a search to made through parent environments for an existing
# definition of the variable being assigned. If such a variable is found 
# (and its binding is not locked) then its value is redefined, otherwise
# assignment takes place in the global environment.

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        InvMat <- NULL # this is where the result of inversion is stored
        # A setter function, use this to set a matrix to object created by makeCacheMatrix function
        # e.g makeCacheMatrix(testmatrix) # here we work on testmatrix
        # makeCacheMatrix$set(testmatrix1) # here we work on testmatrix1
        set <- function(y) {
                x <<- y
                InvMat <<- NULL # it also initialises InvMat to null
        }
        
        get <- function() x # return the input matrix
        setInverse <- function(inverse) InvMat <<- inverse # set the inversed matrix
        getInverse <- function() InvMat # return the inversed matrix
        # return a list that contains these functions, so that we can use
        # makeCacheMatrix object like these
        
        # x <- makeCacheMatrix(testmatrix)
        # x$set(newmatrix) # to change matrix
        # x$get # to get the setted matrix
        # x$setInverse # to set the inversed matrix
        # x$getInverse # to get the inversed matrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
        # The first function, makeCacheMatrix creates a special "matrix", which is 
        # really a list containing a function to
        # set the value of the matrix
        # get the value of the matrix
        # set the value of the inverse
        # get the value of the inverse
}


# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the inverse 
# from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInverse() # get the inversed matrix from object x
        # it will be null if uncalculated, remember the first line "xinv <- NULL" in the previous function
        if(!is.null(m)) { # if the inversion result is there
                message("getting cached data")
                return(m) # return the calculated inversion
        }
        data <- x$get() # if not, we do x$get to get the matrix object
        m <- solve(data) # we solve it
        # Computing the inverse of a square matrix can be done with the solve function in R. 
        # For example, if X is a square invertible matrix, then solve(X) returns its inverse.
        x$setInverse(m) # we then set it to the object
        m # return the solved result
}

# # Test
# # generate a random square, non-singular matrix
# test <- matrix(runif(9,1,100),3,3)
# # generate the makeCacheMatrix object with this matrix
# testCached <- makeCacheMatrix(test)
# # from now on calculate or retrieve calculated inversion using the cacheSolve function
# 
# testInv <- cacheSolve(testCached)
