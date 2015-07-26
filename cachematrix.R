## Put comments here that give an overall description of what your
## functions do

##This function create the list of functions that can be use to set and cache inverion of matrix.

makeCacheMatrix <- function(X = matrix()) { 
  IN<-matrix()
  cache<-FALSE
  
  set <- function(Y) { 
    X<<-Y
    IN<<- matrix()
    cache<<-FALSE}
  get <- function() X
  setInverse <- function(inverse) {
    IN<<-inverse
    cache<<-TRUE
  }
  getInverse <- function() IN
  getCache <- function() cache
  setCache <- function(c) cache<<-c
  list(set = set, get=get, setInverse=setInverse, getInverse=getInverse, getCache=getCache, setCache=setCache)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  IN<- X$getInverse()
  cache<-X$getCache()
  if(cache==TRUE){ 
    message("getting cached data")
    return(IN)
  }
  data<-X$get()
  IN<-solve(data, ...)
  X$setInverse(IN)
  X$setCache(TRUE)
  IN
}
