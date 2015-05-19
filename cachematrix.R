## Put comments here that give an overall description of what your
## functions do

## The overall objective of this pair of functions is to commit the 
## result of a potentially time consuming repeat operation to memory or "cache",
## such that when the operation needs to be repeated, R is directed to look
## at the cache and retrieve the previously computed answer, rather then 
## take up runtime to re-calculate it.

## Write a short comment describing this function

## This function creates a matrix object "x" that can cache it's inverse.
## The matrix object is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix
## It uses "solve()" to find the inverse of a matrix and cache it using a free floating variable "m".


makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


## Write a short comment describing this function

## This function computes the inverse of the matrix returned by the function makeCacheMatrix above.
## Before doing this, it first checks to see if the inverse has already been calculated using "if(!is.null(m))".
## If so, it gets the inverse matrix from the cache and skips the computation.
## Otherwise it calculates the inverse matrix via "x$setmatrix(m)".

cacheSolve <- function(x, ...) {
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}
