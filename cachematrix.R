## Put comments here that give an overall description of what your functions do
## These functions work out the inverse matrix of an input matrix, and cache the result after the first computation. It automatically
## retrieve the results if the input matrix remain unchanged.

## Inverseproduct: This function create an "Identity matrix" for the input matrix
Inverseproduct<-function(x){
  a<-dim(x)[1]
  matrix_0<-matrix(rep(0,times=as.numeric(a^2)),a,a)
  for(i in 1:a){
    matrix_0[i,i]<-1
  }
  matrix_0
}
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x=matrix()) {
  Inv_matrix<-Inverseproduct(x)
  sol_matrix <- NULL
  set <- function(y) {
    x <<- y
    sol_matrix<<-NULL
  }
  get <- function() {
    x
  }
  setsol <- function(sol) {
    sol_matrix <<- sol
  }
  getsol <- function() {
    sol_matrix
  }
  list(set = set, 
       get = get,
       setsol = setsol,
       getsol = getsol)
}

## Write a short comment describing this function
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
##retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  sol_matrix <- x$getsol()
  if(!is.null(sol_matrix)) {
    message("getting cached data")
    return(sol_matrix)
  }
  data_matrix <- x$get()
  sol_matrix <- solve(data_matrix, Inverseproduct(data_matrix))
  x$setsol(sol_matrix)
  sol_matrix
  
}