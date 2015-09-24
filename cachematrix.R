makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
 
 set <- function(y){
   x <<- y
   m <<- NULL
 }
 
 get <- function() {
    x
 }
 
 setinverse <- function(inversematr) {
   m <<- inversematr
 }
 getinverse <- function() m
 list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  matri <- x$get()
  if (!nrow(matri) == ncol(matri))
  {
    message("not square matrix")
    return
  } else {
  m <- solve(matri)
  x$setinverse(m)
  }
  m

}
