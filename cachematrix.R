## The makeCacheMatrix creates  a list containing function to 
# 1 set the values of the "Matrix"
# 2 get the values of the  "Matrix"
# 3 set the values of the inverse of the "Matrix" 
# 4 get the values of the inverse of the "Matrix"

makeCacheMatrix<-function(mat=matrix()){
  
  inv <- NULL
  set2 <- function(y1) {
    mat <<- y1
    inv <<- NULL
  }
  get2 <- function() mat
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set2 = set2, get2 = get2,
       setinv = setinv,
       getinv = getinv)
}


## The CachSolve function calculates the inverse of the special "Matrix" created with the function "makeCacheMatrix . 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the datamat and sets the values of the inverse in the cache via the set function.
      
cachSolve <- function(mat, ...) { ## Return a matrix that is the inverse of 'mat'
  inv <- mat$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  datamat <- mat$get2()
  inv <- solve(datamat, ...)
  mat$setinv(inv)
  inv
}
m1<-makeCacheMatrix(mat=matrix(1:4,2,2))
cachSolve(m1)
