## This programming assignment demonstrates how the scoping rules of the R language can used to 
## cache the value of potentially time -consuming computations like matrix inversion and how it can be 
##  retrieved ratherthan recomputed when needed.


## First function makeCacheMatrix creates a special "matrix", 
##which is really a list contaning a function to
## 1.set the value of matrix
## 2.get the value of matrix
## 3.set the value of inverse matrix
## 4.get the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {  ##input matrix
  
  m<- NULL                                   ## to store the inverse of the input matrix
  
  set<- function(y){
    x<<-y                                    ##set the value of matrix
    m<<-NULL                                 ##previous chached inverse is removed, if any.
    }
  get<-function ()x
  setinverse<-function(inverse)m<<-inverse
  getinverse<-function()m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function calcultes the inverse of the special matrix 
## created by the previous funtion.
## its checkes and gets the inverse from the cache, provided the inverse has been already calculted
## thereby skiping the recalculation or else it calculates the inverse and sets in the cache.

cacheSolve<- function(x,...){  ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m)){             ## if the cache value is present, it is retrived with a message
    message("getting cached data.")
    return(m)
  }  
    data<-x$get()             ## gets the value of matrix
    m<-solve(data)            ## computes the inverse of the matrix
    x$setinverse(m)           ## cache the inverse of the matrix
    m                         ## inverse of the metrix
}


