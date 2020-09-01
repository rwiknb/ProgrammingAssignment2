##The file contains functions for efficiently obtaining the inverse of a matrix  
##by caching the result once calculated.

##This function creates an object capable of storing a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse)inv<<-inverse
  getinverse<-function()inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


##This function reads the inverse of the matrix if already cached else
##calculates and caches the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)){
    message('getting cached data')
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinverse(inv)
  inv
}
