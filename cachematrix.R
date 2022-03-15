## CACHING MATRIX INVERSE 
## Below is a pair of R functions that cache the inverse of a matrix

## The following function creates a special cache matrix which 
## stores the value of a matrix and 
## its inverse if it's already computed:
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL       # sets the inverse to NULL whenever a new special matrix is created
        
        # set() stores the value of the matrix and its inverse 
        set <- function(y){   
                x <<- y 
                inverse <<- NULL    # inverse is NULL by default
        }
        
        # get() gets the values of the matrix stored in the special matrix by set()
        get <- function() x  
    
        # set_inverse() sets the value of the inverse matrix
        set_inverse <- function(solve) 
                inverse <<- solve          
        
        # get_inverse() gets the value of the inverse matrix
        get_inverse <- function()   
                inverse
    
        # list containing all 4 functions in the makeCacheMatrix() environment
        # allows us to use the $ operator on the matrix 
        list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)  
}


## The following function retrieves the inverse of 
## a matrix 'x' from the cache, or computes the inverse
## if it doesn't already exist in the cache:
cacheSolve <- function(x, ...) {    
        
        inverse <- x$get_inverse()    # stores the inverse matrix if it already exists in cache 
                                      # (the special matrix created by makeCacheMatrix)
                                      # if no inverse exists in the cache, then inverse = NULL
    
        if(!is.null(inverse)){        # if the inverse exists in the cache, return the inverse
                message("Getting cached data...")  
                return(inverse)
        }else{                        # if it doesn't exist in cache, 
                data <- x$get()           # get the value of the matrix and store it in a variable called data
                message("No cache found. Computing the inverse...")   
        
                # nested if-else statement!
                if(det(data) != 0){      # check if the determinant of the matrix is zero.       
                        inverse <- solve(data, ...)   # If the determinant isn't zero, then calculate the inverse,
                        x$set_inverse(inverse)        # store the inverse in the cache,
                        inverse                       # and return the inverse as the output
                }else{                  # if the determinant is zero, then the inverse of the matrix cannot be calculated.
                        # let the user know the inverse doesn't exist in the mathematical world.
                        message("Matrix is singular. Inverse cannot be calculated.")   
                }
        }
}
# END
