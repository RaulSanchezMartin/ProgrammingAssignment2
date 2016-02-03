## Assignment 2 of the  "R programming"
## Author: Raul Sanchez Martin
## Last modification: 4 Feb 2016

## The function "makeCacheMatrix" takes as an input a matrix. Returns
## a list containing a function to: 1 set the value of the matrix, 
## 2 get the value of the matrix, 3 set the value of the inverse
## 4 get the value of the inverse

makeCacheMatrix <- function(Matrix = matrix()) {
inv <- NULL
set <- function(Y){
	Matrix <<- Y
	inv <<- NULL
	}
get <- function() Matrix
setinverse <- function(Inverse) inv <<- Inverse
getinverse <- function() inv
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The function "cacheSolve" calculates the inverse of a given matrix. However,
## first check if the inverse is in the cache. In that case, the functions skips the 
## computation of the matrix

cacheSolve <- function(Matrix, ...) 
{
inv <- Matrix$getinverse()
if(!is.null(inv)){
	message("The inverse is in cache. Computation is skipped. Inverse is returned")
	return(inv)
	}
message("The inverse is not in cache so it will be computed and returned")
data <- Matrix$get()
inv <- solve(data)
Matrix$setinverse(inv)
inv
}
