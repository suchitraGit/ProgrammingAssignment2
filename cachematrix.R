

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
 ## setter method
  set<-function(y){
    x<<-y
    m<<-NULL
  }
 
 ## getter method
  get<-function() x
 
 ## set the inverse matrix
  setmatrix<-function(solve) m<<- solve
 
 ## get  inverse matrix
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getmatrix()
  
  ## checking if matrix is cached
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  ## if matrix is not cached get inverse and cache it
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
