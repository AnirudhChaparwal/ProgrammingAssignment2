## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special matrix object that cache it's inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  get<-function() x
  setinverse<-function(solve) inverse<-solve
  getinverse<-function() inverse
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special matrix returned by the above functionmakeCacheMatrix.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse<-x$getinverse()
  if(!is.null(inverse)){
    message("Getting cached inverse of matrix")
    return(inverse)
  }
  data<-x$get()
  inverse<-solve(data)
  x$setinverse(inverse)
  inverse
}
