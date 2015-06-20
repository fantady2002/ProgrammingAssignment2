## This program is for determining the inverse of a matrix and caching it 
## rather than computing it repeatedly. 

## The makeCacheMatrix() function creates a special "matrix" object that
## can cache its inverse. an example of using this function is
## "mats<-makeCacheMatrix(mat)" where "mats" is the variable  for
## the created special "matrix" object and "mat" is the matrix being inversed


makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.An example of using this function is 
## "cacheSolve(mats)" where mats is the variable that holds the cached matrix
## object set using the makeCacheMatrix() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
