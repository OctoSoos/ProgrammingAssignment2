## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function creates and empty object to later store the inverse matrix. It also defines several "behavior-function" such as set, get, setinverse and getinverse, that will be used in the next function

makeCacheMatrix <- function(x = matrix()) {
  ## Creating an empty object to store the inverse of matrix
  inverseMatrix <- NULL
  
  ## Set function: storing x value within this environment 
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  ## Get function: 
  get <- function() 
    x 
    
  ## "Setinverse" function that will then allow to set new inverse
  setinverse <- function(inverse)
    inverseMatrix <<- inverse
  
  ##This function will then return the inverse
  getinverse <- function() 
    inverseMatrix
   
  list(set = set, 
       get = get, 
       getinverse = getinverse, 
       setinverse = setinverse)
}


## Write a short comment describing this function
## The function first checks, whether any inverse matrix has already been chaced. If yes — returns it; if not — calculates a new one and stores it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## Get whatever meaning of inverse we have from previous function
  inverseMatrix <- x$getinverse()
  
  ## If the inverse was already calculated we return it 
  if(!is.null(inverseMatrix)) {
   
      message("getting cached inverse")
      return(inverseMatrix)
    }
  
  ## If not, calculating the inverse 
  my_matrix <- x$get()
  inverseMatrix <- solve(my_matrix, ...)
  
  ## Setting the calculated inverse
  x$setinverse(inverseMatrix)
  inverseMatrix
}
