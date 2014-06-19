## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	## Set the inverse to NULL
  	inv<- NULL
  	set <- function(y,n){
  		## Create a square matrix with the given data and given dimensions
    	x <<- matrix(data=y,nrow=n,ncol=n)
    	## Make the inverse NULL again
    	inv<<- NULL
  	}
  	## Get the value of the matrix
  	get <- function() x
  	## Set the value of the inverse of the matrix
  	setinverse <- function(inverse) inv<<- inverse
  	## Display the value of the inverse of the matrix
  	getinverse <- function() inv
  	list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## CacheSolve
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	## Get the value of the inverse from x
    inv<- x$getinverse()
    ## The if condiction checks whether the inverse has already been calculated or not.
  	if(!is.null(inv)){
  		## If the inverse has been calculated already, it prints "getting cached data" and returns the inverse 
    	message("getting cached data")
    	return(inv)
  	}
  	## If the inverse has not been calculated, it calculates it.
  	## This part gets the value of the matrix from x via x$get() and stores it in a variable called 'data'.
  	data <- x$get()
  	## This line finds the inverse of the matrix using solve().
  	inv<- solve(data)
  	## This line sets the value of the inverse in x.
  	x$setinverse(inv)
  	inv
}
