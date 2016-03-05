## The two functions demonstrate how a different environemnt, the global environment, can be 
## used to cache a value to be recalled later.

## makeCacheMatrix will store a matrix as well as the inverse of the matrix
## The inverse is not calculated. Rather, the metrix, and an inverse matrix, are both entered by 
## the user. The matrix and inverse are both saved to the global environment, from which it can be recalled.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}

## I used this bit of code to create a 3x3 matrix. The setseed function makes values reproducible, if needed.
setseed(5)
matrix1 <- matrix(round(rnorm(9)*5), nrow=3)
## matrix2 is the inverse of matrix1
matrix2 <- solve(matrix1)

## The matrix value is then set with makeCacheMatrix
a <- makeCacheMatrix()
a$set(matrix1)
## The inverse is also set
a$setinv(matrix2)

## The cacheSolve function returns the cached value of the inverse matrix from the first function.
## The if statement checks to see if the cached value is NULL, and also checks whether the 
## cached value of the inverse matrix is correct. If not NULL, and if correct, it returns the cached
## value. If the cached matrix is not the inverse, the inverse matrix is calculated and returned.

## Whether the inverse matrix is correct is tested by matrix multiplying the cached matrix and the cached
## inverse matrix, which should result in an identity matrix. The identity matrix will sum to nrow(i) if correct.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i) && (round(sum(x$get() %*% i))==nrow(i))) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}

