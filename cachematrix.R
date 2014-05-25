#' Matrix inversion is usually a costly computation and their may be some 
#' benefit to caching the inverse of a matrix rather than compute it repeatedly.
#' 
#' makeCacheMatrix: This function creates a special list of functions can be 
#' used to cache the data and its inverse.  
#' cacheSolve: This function computes the inverse of the special list returned
#' by makeCacheMatrix above. If the inverse has already been calculated (and 
#' the matrix has not changed), then the cachesolve should retrieve the inverse
#' from the cache.
#' 
#' The example of use case would be
#'    amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
#'    amatrix$get()         # Returns original matrix
#'    cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
#'    cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse
#'    
#'    amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
#'    cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
#'    cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse


#' According to the assignement, makeCacheMatrix function creates a special
#' "matrix" object that can cache its inverse.  But this is NOT true according 
#' to the example: makeVector().  makeCacheMatrix also creates a list of functions 
#' to set/get the matrix and to set/get value of its inverse.  Only the different 
#' is that x's default value is empty matrix vs. makeVector's x's default value is
#' empty numeric vector().
#'
#' Since the example makeVector in the assignement works for any data type, 
#' it didn't even need to change a single line to be used by cacheSolve().
#' Therefore the name of this function should have been like makeCacheFunctions.
#' Also other than set(), the other functions doesn't need to be used directely 
#' except within cacheSolve() or other cache<Function>(). 
#' But the requirement is the requirement even when it is ambiguous and wrong.
#'
#' So here's the required short description of the function.
#'
#' makeCacheMatrix returns creates a list of functions to set/get the data and
#' to set/get value of the result of a function.

makeCacheMatrix <- function(x = matrix()) {
	r <- NULL
	set <- function(y) {
		x <<- y
		r <<- NULL
	}
	get <- function() x
	setResult <- function(result) r <<- result
	getResult <- function() r
	list(set = set, get = get, setResult = setResult, getResult = getResult)
}


#' Again, according to the assignment, cacheSolve() returns the inverse of 
#' the special "matrix" returned by makeCacheMatrix above. If the inverse 
#' has already been calculated (and the matrix has not changed), then the 
#' cachesolve should retrieve the inverse from the cache.
#' 
#' But the definition in the original file is misleading.   'x' here is not 
#' same 'x' in makeCacheMatrix.  'x' in cacheSolve(x) is the list of cache 
#' functions.  Therefore the head should have been 
#' 		cacheSolve <- function(cacheFunctions, ...) {
#'
#' While eaisest things to do to finish the assignement would have been copy
#' makeVector as is except the head, and make one single change to cacheSolve
#' by changing 'mean' to 'solve', I hate missleading code.
#'
#' So here's the required short description of the function.
#'
#' cacheSolve returns the inverse of the "matrix" returned by makeCacheMatrix above.
#' If the inverse has already been calculated (and the matrix has not changed), 
#' then the cachesolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        r <- x$getResult()
        if(!is.null(r)) {
                message("getting cached data")
                return(r)
        }
        data <- x$get()
        r <- solve(data, ...)   #' Really this is only the line that needed to be changed for the whole assignment.
        x$setResult(r)
        r
}

#' However if we are allowed to change the name and the definition of the makeCacheMatrix, 
#' then more elegant solution can be designed.
#'
#' makeCacheFunctions returns a list of sub functions $set(y) to reset the data and 
#' $getResult() to compute, store, and get the result of a function : FUN or 
#' return the previously cached result.
#'
#' The test case would be 
#'		> cr <- makeCacheFunctions(matrix(1:4, 2), solve)
#'		> cr$getResult()
#'		cacluating the function of data
#'		     [,1] [,2]
#'		[1,]   -2  1.5
#'		[2,]    1 -0.5
#'		> cr$getResult()
#'		getting cached data
#'		     [,1] [,2]
#'		[1,]   -2  1.5
#'		[2,]    1 -0.5
#'		> cr$set(matrix(c(0,5,99,66), nrow=2, ncol=2))
#'		> cr$getResult()
#'		cacluating the function of data
#'		            [,1] [,2]
#'		[1,] -0.13333333  0.2
#'		[2,]  0.01010101  0.0
#'		> cr$getResult()
#'		getting cached data
#'		            [,1] [,2]
#'		[1,] -0.13333333  0.2
#'		[2,]  0.01010101  0.0
#'		

makeCacheFunctions <- function(x = matrix(), FUN, ...) {
	r <- NULL
	set <- function(y) {
		x <<- y
		r <<- NULL
	}
	getResult <- function() {
        if(is.null(r)) {
	       message("cacluating the function of data")
  	       r <<- FUN(x, ...)   #' Really this is only the line that needed to be changed for the whole assignment.
    	} else {
	       message("getting cached data")
        }
        r
    }
	list(set = set, getResult = getResult)
}

#' This function is no longer necessary other than as a short alias.
#' In above test case, cacheResult(cr) can be used instead of cr$getResult().
#'
cacheResult <- function(cacheFunctions) {
        cacheFunctions$getResult()
}

