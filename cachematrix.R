##Caching the Inverse of a Matrix


##This function creates a special "matrix" object that can cache its inverse.



makeCacheMatrix <- function(x = matrix()) {
 
  inverse<-NULL
  setMatrix<-function(y=x)
          {
                x<<-y
                inverse<<-NULL
  }
  
  getMatrix<-function() x
  
  getInverse<-function()inverse
  
 
  setInverse<-function(i)inverse<<-i
  
  
  list(setmatrix=setMatrix,getmatrix=getMatrix,setinverse=setInverse,getinverse=getInverse)
}


##This function computes the inverse of the special "matrix" returned by ##makeCacheMatrix above. If the inverse has already been calculated (and the ##matrix has not changed), then the cachesolve should retrieve the inverse from ##the cache.



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

       inverse<-x$getinverse()
       if(!is.null(inverse))
       {
               message("getting cached data")
               return(inverse)
       }
       
       m <- x$getmatrix()
       inverse <- solve(m)
       x$setinverse(inverse)
       inverse
}