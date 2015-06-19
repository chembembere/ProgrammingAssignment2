## This program provides a way to cache the inverse of a matrix.
## Works with 2 functions, 
## makeCacheMatrix : provides functions to work with the cache and initialize it
## cacheSolve : Calculate the inverse of the matrix


## function makeCacheMatrix provides the tools to work with the matrix's cache
## We can use 4 functions
## set(y) : Changes the stored matrix in the main function for the one provided as param. 
## get(): Returns the matrix x stored in the main function (makeCacheMatrix)
## setInverse(inverse): Stores the inverse of the matrix for later use.Needs the inverse of the matrix as param. 
## getInverse () : Returns de inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
   ##The first time, the inverse matrix is set to NULL
   ms <- NULL
   set <- function(y){
     x <<- y
     ms <<- NULL
   }
   get <- function () x
   setInverse <- function (inverse) ms <<- inverse
   getInverse <- function () ms
   ##List of functions inside makeCacheMatrix to use any type x$<function name> ie. x$get()
   list(set=set, get=get,setInverse=setInverse,getInverse=getInverse)
}


## This function calculates the inverse of a given matrix.
## If the inverse has already been calculated (and it's the same matrix), then 
## it will return the inverse from the cache, else it will calculate the inverse
## and store the result in the cache
## IMPORTANT : You need to previously initialize the matrix by typing
## your_matrix <- makeCacheMatrix (origina_matrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ms <- x$getInverse()
        if (!is.null(ms)){
          message("Getting cached matrix")
          return (ms)
        }
        ##If the matrix is not cached, i need to calculate the inverse of X and store it
        data <- x$get()
        ms <- solve(data)
        x$setInverse(ms)
        ms
  }
