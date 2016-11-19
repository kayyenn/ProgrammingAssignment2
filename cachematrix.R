## Programming Assignment 2
## Caching the Inverse of a Matrix

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse by
## creating a list containing the functions to
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}


## The cacheSolve function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinv function.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}


## The following are some samples to test if the functions above were correctly written

mat1 <- matrix(c(7, 0, -3, 2, 3, 4, 1, -1, -2), nrow=3, ncol=3)
mat2 <- rbind(c(3,1), c(4,2))
test1 <- makeCacheMatrix(mat1)
cacheSolve(test1) # This should return the inverse matrix mat1
cacheSolve(test1) # The second cacheSolve should show retrieval from cache
test2 <- makeCacheMatrix(mat2)
cacheSolve(test2) # This should return the inverse matrix mat2
cacheSolve(test2) # The second cacheSolve should show retrieval from cache
## Testing the set ( ) function
test2$set(rbind(c(2,4), c(-3,1))) # Changing value without initialising another instance of of the object
cacheSolve(test2)
cacheSolve(test2)

