## Put comments here that give an overall description of what your


## Write a short comment describing this function
# putting the matrix into memory and getting the inverse of matrix
#store a matrix x and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
          inv1 <- NULL
          set <- function(z){
            x <<- z
            inv1 <<- NULL
          }
          get <- function() x
          setInverse <- function(inverse) inv1 <<- inverse
          getInverse <- function() inv1
          list(set=set,get = get,
               setInverse = setInverse,
               getInverse = getInverse)
}   


## Write a short comment describing this function

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){      
          message("getting cached data")
          return(inv)   #basically when inv is not null, return inv inverse
  }
  ## Return a matrix that is the inverse of 'x' 
  mat <- x$get()
  inv <- solve(mat, ...)   #slove function is used to derive the inverse of the matrix x from previous line
  x$setInverse(inv)   # setting 
  inv }
        
## test
# My_Matrix <- makeCacheMatrix(matrix(1:4, 2,2))
#My_Matrix$get()
#[,1] [,2]
#[1,]    1    3
#[2,]    2    4
#cacheSolve(My_Matrix)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

