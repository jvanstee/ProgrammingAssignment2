## Matrix inversion is usually a costly computation 
## It is more efficient to cache the inverse of a matrix instead of computing it repeatedly. 
## Following is a pair of functions that cache the inverse of a matrix.

## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse
## The function makeCacheMatrix creates a  a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix())  {
  inv <- NULL
  set <- function(y){
    x <<- y                            
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,get = get,setinverse = setinverse,getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed)
## Then cacheSolve retrieves the inverse from the cache.
## if the inverse has not yet been calculated
## then cacheSolve calculates the inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinverse() 
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
  data <-x$get()
  inv <-solve(data,...)
  x$setinverse(inv)
  inv
}

## VERIFICATION:
## > matrix<-rbind(c(1,1/3),c(1/3,1))
## > matrix
##       [,1]      [,2]
##[1,] 1.0000000 0.3333333
##[2,] 0.3333333 1.0000000
##> a <- makeCacheMatrix(matrix)
##> a$get()
##      [,1]      [,2]
##[1,] 1.0000000 0.3333333
##[2,] 0.3333333 1.0000000
##> cacheSolve(a)
##      [,1]   [,2]
##[1,]  1.125 -0.375
##[2,] -0.375  1.125
##> cacheSolve(a)
##getting cached data
##       [,1]   [,2]
##[1,]  1.125 -0.375
##[2,] -0.375  1.125
##> cacheSolve(a)
##getting cached data
##       [,1]   [,2]
##[1,]  1.125 -0.375
##[2,] -0.375  1.125

## PROOF
##> M
##        [,1]      [,2]
##[1,] 1.0000000 0.3333333
##[2,] 0.3333333 1.0000000
##> cacheSolve(a)
##      [,1]   [,2]
##[1,]  1.125 -0.375
##[2,] -0.375  1.125
##> I <- a$getinverse()
##> I
##       [,1]   [,2]
##[1,]  1.125 -0.375
##[2,] -0.375  1.125

##> M %*% I
##       [,1] [,2]
##[1,]    1    0
##[2,]    0    1
