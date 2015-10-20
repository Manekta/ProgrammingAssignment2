#Creates a special matrix and cache the inverse of that matrix 

#makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set<-function(y){
    x<<- y
    inverse<<-NULL    
  }
  get<-function() x
  setinverse<-function(solve) inverse<<-solve
  getinverse <-function() inverse
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}

#cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix function. If inverse is already computed, 
#then retreives the inverse matrix from the cache
 
cacheSolve <- function(x, ...) {
  inverse<-x$getinverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data<-x$get()
  inverse<-solve(data,...)
  x$setinverse(inverse)
  inverse
}
