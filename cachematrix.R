## Matrix Inversion is expensive. The functions in this file helps in caching inversion of matrix
## once calculated.

## makeCacheMatrix function helps access a "special" matrix object that can cache its inverse
makeCacheMatrix <- function(argMat = matrix()){
    ## memInvMat is the object used to cache matrix inverse
    memInvMat <- NULL
    ## set is used to reset matrix given while creating makeCacheMatrix 
    ## This matrix is set at makeCacheMatrix environment scope 
    ## and cached matrix inverse object is reset
    set <- function(givenMat){
      argMat <<- givenMat
      memInvMat <<- NULL
    }
    ## get is used to return the matrix set in makeCacheMatrix
    get <- function(){
      argMat
    }
    ## setInv is used to cache the matrix inverse 
    ## The inverse matrix is set at makeCacheMatrix environment scope
    setInv <- function(setInvMat){
      memInvMat <<- setInvMat
    }
    ## getInv is used to retrieve the cache of matrix inverse 
    getInv <- function(){
      memInvMat
    }
    ## the list gives access to the set and get functions of matrix and inverse matrix     
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv)
}
## cacheSolve function computes inverse of "special" matrix set in makeCacheMatrix for the first time
## and return cache after the first time.
cacheSolve <- function(argMatFnList, ...){
    ## retrieve the cached inverse matrix from the makeCacheMatrix's list
    memInvMat <- argMatFnList$getInv()
    ## If the retrieved inverse matrix is not null, then is returned
    if(!is.null(memInvMat)){
      message("getting cached data")
      return(memInvMat)
    }
    ## retrieve the special matrix for which inverse need to be computed
    setMat <- argMatFnList$get()
    ## solve function computes the inverse of the special matrix
    memInvMat <- solve(setMat,...)
    ## cache the inverse matrix
    argMatFnList$setInv(memInvMat)
    ## return the inverse matrix
    memInvMat
}