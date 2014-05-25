# Our function makeCacheMatrix by default takes an empty matrix and can four basic operations
# 1. Set our variable to chosen matrix
# 2. Retrieve our matrix
# 3. set the inverse of the matrix to a value provided by the user
# 4. retrieve that inverse
 
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      # this function just assigns our empty matrix with matrix given by user
      set_matrix <- function(y){
            x <<- y
            m <<- NULL
      }
      # used to retreive cahced matrix
      get_matrix<-function(){x}
      # set the inverse of our cahced matrix
      set_inverse<- function(inverse){m<<- inverse}
      # retrieving our inverse
      get_inverse<- function(){m}
      list(set_matrix = set_matrix,get_matrix=get_matrix,
           set_inverse=set_inverse,get_inverse=get_inverse)
}
# This function checks if the we have already computed the inverse of a matrix and if we have it retrieves the cached data
# If it hasn't been computed then it retrievs the matrix and the solves for the inverse
cacheSolve <- function(x,...){
      # retrives the inverse of matrix assigned to the function
      m <- x$get_inverse()
      # It then checks whether the return inverse is null or not to see if it has been calculated or not
      if(!is.null(m)){
            # if it has been already computed then it retrieves the already cached matrix
            message("Getting cached inverse....")
            return(m)
            m
      }
      # if inverse hasn't been solved it then gets the matrix and computes it matrix
      matrix1 <- x$get_matrix()
      inverse <- solve(matrix1,...)
      x$set_inverse(inverse)
      inverse
}
