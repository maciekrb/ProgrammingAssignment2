# The following code implements inverse matrix caching in R based on a special
# matrix enriched by the makeCacheMatrix function. 
# 
# The code is based on the vector caching code provided as an example 
# for the Coursera Data Science course. 

# Function attaches getter and setter methods that add caching functionality to the
# provided Matrix
# Args:
#   x:   a matrix R object
#
# Returns:
#   a matrix object with set, setinverse, get and getinverse methods attached 
#
# Sample usage
#
#   To create a matrix with caching capabilities, do this:
#   
#     caching_matrix <- rbind(c(1, 3), c(3, 1))
#     
#   The above matrix can now be used with cacheSolve function: 
#
#    cacheSolve(caching_matrix)
# 
#  If the matrix is invertable, you can test the inverse matrix, multiplying
#  the resulting matrix by the original one. You should get the identity 
#  matrix as a result.
makeCacheMatrix <- function(x = matrix()) {

        # init m variable to null, this is cheched later in the cacheSolve function
        m <- NULL
        set <- function(y) {
          # Set variables in the parent environment using the <<- operator
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# Function checks if the inverse of the given matrix has been calculated
# and returns a cached value when available. It calculates the inverse
# of a matrix otherwise, storing it using the setter methods attached
# to the x matrix.
#
# Args:
#  x:  a special matrix enriched through the makeCacheMatrix function
#      containing getter / setter methods
#
# Returns:
#   matrix: Inverse matrix of given matrix x
cacheSolve <- function(x, ...) {
        # Retrieve the m value from the matrix using the getter function
        m <- x$getinverse()

        # When cache is available, just return the stored value and 
        # issue a message notifying that the provided value comes from
        # the object cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        # In case that the inverse has not been calculated, calculate de
        # inverse of the provided matrix and use setter method to cache, 
        # and return the calculated matrix
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
