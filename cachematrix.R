## This file defines two function to create and cache the inverse of matrix.


## makeCacheMatrix function provides a mechanism to create, retrieve and cache the value of matrix and its inverse. it returns a list of function which are
## capable to set the matrix and its inverse value in parent enviorment.

makeCacheMatrix <- function(m = matrix()) {
		## NULL initialization of inverse matrix variable
		inverseM <- NULL

		## Method to set input matrix in parent env.
        	setMatrix <- function(matrix) {
                		m <<- matrix
                		inverseM <<- NULL
        	}

		## Method to retrieve matrix
        	getMatrix <- function() m

		## Method to set inverse matrix in parent env.
        	setInverseMatrix <- function(inverse) {
				inverseM <<- inverse
		}

		## Method to retrieve matrix
        	getInverseMatrix <- function() inverseM
		
		## List of methods being return
        	list(setMatrix = setMatrix, getMatrix = getMatrix,
             		setInverseMatrix = setInverseMatrix,
            	 	getInverseMatrix = getInverseMatrix)

}


## cacheSolve function takes above function as argument and always checks the matrix inverse in cache whenever it is being requested. If inverse matrix found
## in cache then deliver it from cache else compute and keep it in cache and deliver to calling environment.

cacheSolve <- function(x, ...) {

        	## Return inverse of matrix, if does exist 
		inverseMatrix <- x$getInverseMatrix()

		## check conditional statement whether inverse of matrix being returned
        	if(!is.null(inverseMatrix)) {
				## message notification for availability of inverse of matrix in cache
                		message("Found inverse matrix in cache")

				## return the inverse of matrix which being retrieved from cache
                		return(inverseMatrix)
        	}

		## retrieve the matrix in case inverse of matric not been found in cache
        	matrix <- x$getMatrix()

		## perform calculation to compute inverse of matrix
        	inverseMatrix <- solve(matrix) %*% matrix

		## set inverse of matrix to cache
        	x$setInverseMatrix(inverseMatrix)

		## return the inverse of matrix
        	inverseMatrix
	
}