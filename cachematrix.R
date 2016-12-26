## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <-NULL
  #set
  set <- function(x_2){
    x<<- x_2
    matrixInverse <<-NULL
  }
  #get
  get <-function()x_2
  #setInverse
  setInverse <-function(inverse) matrixInverse<<-inverse 
  #getInverse
  getInverse <-function()matrixInverse
  #returning list
  return(list(set=set, get=get, setInverse=setInverse, getInverse=getInverse))
}


##Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  #check if it's in cache
  matrixInverse <- x$getInverse()
  if(!is.null(matrixInverse)){
    message("getting cached data")
    return(matrixInverse)
  }
  #we need to call function(compute) when it isn't in cache.
  matrix <-x$get()
  matrixInverse <-solve(matrix, ...)   #solve() function computes inv of square matrix
  x$setInverse(matrixInverse)
  matrixInverse
}
