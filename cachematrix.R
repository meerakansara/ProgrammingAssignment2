## Code Added By : Meera Kansara
## Below functions calculate the inverse of a matrix and cache it so that
## the next time user attempts to calculate the Matrix Inverse, previously cached value
## is returned instead of recalculating.

##Assumption : The matrix supplied is always invertible.

## makeCacheMatrix function creates a special "matrix",  
## which is actually a list containing a functions for :
## 1. setting the matrix, 2. getting the matrix ,3. setting the matrix inverse
## 4. getting the matrix inverse

##input parameter : x (an input matrix for which inverse has to be obtained).
##output parameter : a list containing a functions.
makeCacheMatrix <- function(x = matrix()) {
  m_inverse <- NULL
  
  ##getter-setter methods for Matrix
  set <- function(y) {
    x <<- y  ## setting input matrix to parent env.
    m_inverse <<- NULL ## re-initialize m_inverse in the parent environment to null.
  }
  get <- function() x
  
  ##getter-setter methods for matrix inverse
  setinverse <- function(inverse) m_inverse <<- inverse ##setting inverse value to parent env.
  getinverse <- function() m_inverse
  
  ##composing and returning list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function calculates the inverse of the special "matrix" created
## by makeCacheMatrix function. 

##First this function checks wether inverse has already been calculated,if yes, it gets 
##inverse from the cache and skip the calculation. otherwise it calculates the inverse of the 
##given matrix and sets(using setinverse function) the value of inverse to cache.

##input parameter : x (an input list created by makeCacheMatrix function).
##output parameter : inverse of the matrix.
cacheSolve <- function(x, ...) {
  m_inverse <- x$getinverse()
  
  ##check for existance of cached inverse value.
  if(!is.null(m_inverse)) {
    message("getting cached data...")
    return(m_inverse) ##suspending further execution & returning cached inverse.
  }
  input_matrix <- x$get()  ##getting the input matrix
  m_inverse <- solve(input_matrix, ...)  ##calculating inverse
  x$setinverse(m_inverse) ##setting inverse value to cache
  m_inverse
}

## Test Cases
##Case 1 : Positive case with [2*2 Matrix] 
##testMat <- matrix(1:4,2,2) 
##tempList <- makeCacheMatrix(testMat) 
##cacheSolve(tempList) 
##cacheSolve(tempList)

##Case 2 : Positive case with [3*3 Matrix] 
##testMat <- matrix(c(1,0,5,2,1,6,3,4,0),3,3) 
##tempList <- makeCacheMatrix(testMat) 
##cacheSolve(tempList) 
##cacheSolve(tempList)

##Case 3 : Positive case with [2*2 Matrix] 
##testMat <- matrix(c(3,7,8,9),2,2)  
##tempList <- makeCacheMatrix(testMat) 
##cacheSolve(tempList) 
##cacheSolve(tempList)

##Case 4 : Positive case with [4*4 Matrix] 
##testMat <- matrix(c(2,1,7,1,5,4,8,5,0,2,9,7,8,6,3,8),4,4)  
##tempList <- makeCacheMatrix(testMat) 
##cacheSolve(tempList) 
##cacheSolve(tempList)
