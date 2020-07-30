#the functions below will create an object which
#stores a matrix & caches its inverse
#Function "makeCacheMatrix" creates a matrix that is
#a list that contains a function to (1)
#set value of matrix (2) get value of matrix (3)
#set value of inverse (4) get value of inverse 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
#function below will compute the inverse of 
#the special matrix which is returned by "makeCacheMatrix"
#if inverse has been already calculated, then fuction "cacheSolve" will 
#retrieve inverse from cache 
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
#Now, I will call the function with the matrix 
#compute inverse 
#retrieve inverse from the cache list 
#change matrix to inverse 
#compute inverse and return the first function
A<- matrix(c(1, 2, 3, 4), 2, 2)
AA<- makeCacheMatrix(A)
cacheSolve(AA)
#results (I copy-pasted them from my RStudio)
#> cacheSolve(AA)
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> 
