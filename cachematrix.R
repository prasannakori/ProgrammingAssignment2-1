## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#makeCacheMatrix function caches the matrix passed as argument to it
## x: a square invertible matrix
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) 
{
        inverse = NULL
        set = function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                inverse <<- NULL
        }
        get = function() x
        setinverse = function(inv) inverse <<- inv
        getinverse = function() inverse
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
        ## @x: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        
        inv = x$getinverse()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinverse(inv)
        
        return(inv)
}

# testFunc: driver function to test the cacheSolve & makeCacheMatrix
# testMatrix: an invertible matrix (N * N matrix)
testFunc = function(testMatrix)
{
        temp = makeCacheMatrix(testMatrix)
        
        startTime1 = Sys.time()
        cacheSolve(temp)
        timeLapse1 = Sys.time() - startTime1
        print(timeLapse1)
        
        startTime2 = Sys.time()
        cacheSolve(temp)
        timeLapse2 = Sys.time() - startTime2
        print(timeLapse2)
}

set.seed(8094774)
nRandValues = rnorm(25000000)
matrix1 = matrix(nRandValues, nrow=5000, ncol=5000)
testFunc(matrix1)



