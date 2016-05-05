## The following two functions (makeCacheMatrix and cacheSolve) will allow to save 
## computing resources for inversion of a predefined matrix, taking the result directly
## from the cache in case it is calculated once, instead of calculating it once again.

## 'makeCacheMatrix' function allows to create and cache inverse of a predefined matrix.
## Usage: 1. makeCacheMatrix(x=matrix('define your matrix here')). In order to use it
## with the next function ('cacheSolve') and make operations with it more easy it is better to define an object. 
## For instance: YourMatrixObject <- makeCacheMatrix(x=matrix(1:4,2,2))
## 2. To verify that matrix you created is ok, type: YourMatrixObject$get(). This will print your matrix.
## 3. To check if there is an inverse already existing type: YourMatrixObject$getnvrtd(). The result will 
## be NULL in case there is no inverted matrix in cache. Otherwise inverted matrix will appear on the screen.

makeCacheMatrix <- function(x = matrix()) {
   nvrtd = NULL
   set = function(y) {
        x <<- y
        nvrtd <<- NULL
   }
        get = function() x
	setnvrtd = function(inverted) nvrtd <<-	inverted 
        getnvrtd = function() nvrtd
        list(set=set, get=get, setnvrtd=setnvrtd, getnvrtd=getnvrtd)
}

## 'cacheSolve' function returns inverse of the matrix object you defined by 'makeCacheMatrix' function, but taking it
## from cache in case it inverse has already been calculated.
## Be carefull since function argument (x) is not the same (x) defined in 'makeCacheMatrix'. Argument 'x' here is the 
## matrix object you defined. Usage: cacheSolve(YourMatrixObject). The result will be inverse of the matrix 

cacheSolve <- function(x, ...) {
        nvrtd = x$getnvrtd()
        if (!is.null(nvrtd)){   ## This is to check if there is an inverted matrix exists. '!is.null' means that
                return(nvrtd)   ## in case inverse matrix exists it uses one from the cache.
        }
        initialmtrx = x$get()   
        nvrtd = solve(initialmtrx, ...) ## Otherwise it returns inverse of the initial matrix using R function 'solve' 
        x$setnvrtd(nvrtd)		## and sets into the cache.
        return(nvrtd)
}
