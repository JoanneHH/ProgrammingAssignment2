## This code is for caching the inverse of a matrix
## so that you do not have to compute the inverse repeatedly.

## Function makeCacheMatrix will create a matrix object that can cache its inverse

makeCacheMatrix <-function(x = matrix()) {
  
  invmatrix <- NULL
  
  SetInputMatrix<-function(y) {  
      if (!identical(x,y))  ## check if x is not identical to y
      x <<- y                 ##   establish cache
      invmatrix<<-NULL   ##  to establish cache of empty inverse matrix
  }
    GetInputMatrix <<-function(x=matrix()) x
    
    SetInvmatrix <-function(x) invmatrix <<-solve(x=matrix())
   
    GetInvmatrix <<-function() invmatrix 
    
  list(SetInputMatrix = SetInputMatrix, GetInputMatrix = GetInputMatrix, 
       SetInvmatrix = SetInvmatrix, GetInvmatrix = GetInvmatrix)
}


## Function cacheSolve will compute the inverse of the matrix returned by function 
## makeCacheMatrix above.  If the inverse had already been calculated and the matrix
#has not changed, the inverse will be retrieved from the cache.

cacheSolve <- function(x, ...) {
    ## uses output of makeCacheMatrix
        ## Return a matrix called "invmatrix" that is the inverse of 'x' 
  GetInvmatrix()
  
  if(!is.null(invmatrix)) {  ##if inverse matrix is already in the cache then message user
    message ("getting cached inverse matrix")
    return(invmatrix) }
    
   ##if inverse matrix is null, it must be computed below from the matrix
 invmatrix <<-solve(x)
  
invmatrix
}


