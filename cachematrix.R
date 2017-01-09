## this program contains 2 functions
## the first function makeCacheMatrix creates a list

#makeCachMatrix is a function of x that is a matrix
#this function caches the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  #if inv is not null then write the message "
  "already inverted - getting cached data"
  if(!is.null(inv)) {
    message("already inverted -getting cached data.")
#returning the inverse matrix of x from the cache and skips
    #computation
        return(inv)
  }
  data <- x$get()
  #inv vector is the inverse matrix of data. 
  #inverse is taking using the solve function
  inv <- solve(data)
  x$setinverse(inv)
  #auto-print inverse
  inv
}

#testing
#t is a matrix with 2 rows and 2 columns
t<-matrix(c(1,2,3,4),nrow=2,ncol=2)
t
#the x matrix is put into the makeCacheMatrix
  m = makeCacheMatrix(t)
  m$get()

## No cache in the first run
  #cacheSolve inverts m matrix if it is not already inverted
 cacheSolve(m)

## Retrieving from the cache in the second run
 cacheSolve(m)
 