## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix function takes the input matrix and 
##caches the inverse value of the matrix once it calculated and set
makeCacheMatrix <- function(x = matrix()) {
		    inverseMatrix <- NULL #Setting the storage variable for Inversed Matrix to NULL
        
        set <- function(suppliedMatrix) {
                x <<- suppliedMatrix
                inverseMatrix <<- NULL
        }
        
        get <- function() x
        
        setInverseMatrix <- function(invMtrx) inverseMatrix <<- invMtrx ##Setting the inverse matrix
        
        getInverseMatrix <- function() inverseMatrix ##Get the Inverse Matrix
        
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix ,
             getInverseMatrix = getInverseMatrix )
}


## Write a short comment describing this function
##This is the function which will take the cache matrix as input and calculate 
##the inverse matrix from cache if available or else it will calculate the
##inverse and put it in cache for future usage
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
        inverseMatrix <- x$getInverseMatrix()##Try to get the inverse matrix from cache
        
        if(!is.null(inverseMatrix)) {
                message("getting inverse matrix from cached data")
                return(inverseMatrix)
        }
		
        matrixData <- x$get()
		    message("Calculating the inverse matrix")
        inverseMatrix <- solve(matrixData)#Calculate the inverse matrix
		    message("Putting the inverse matrix value to cache")
        x$setInverseMatrix(inverseMatrix)#Set the inverse matrix value to cache
        inverseMatrix 

}
