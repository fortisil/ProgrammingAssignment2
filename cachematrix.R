## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Inverting a matrix is a time-consuming computations. Due to that we will use caching method to save time on 
#a high consuming compuations.
#
# makeCacheMatrix does the following:
# 1. Set the value of the Matrix
# 2. get the value of the matrix
# 3. Set the value of inverse matrix
# 4. get the value of inverse matrix
#
#
makeCacheMatrix <- function(x = matrix()) {

        inverseM <- NULL
        set <- function(y) {
                x<<-y
                inverseM<<-NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverseM<<-inverse
        getinverse <- function() inverseM
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
# The following function first check to see if the matrix inverse has been
# already calculated. If yes, it gets the inverse matrix from
# the cache and skip calc. If not, it calc the inverse matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseM <- x$getinverse()
        if(!is.null(inverseM)) {
                message("Getting cached matrix.")
                return(inverseM)
        }
        data <- x$get()
        inverseM <- solve(data)
        x$setinverse(inverseM)
        inverseM
}

# Example of the usage & output
#> x = matrix(rnorm(16), nrow =4)
#> m=makeCacheMatrix(x)
#> m$get()
#[,1]       [,2]       [,3]        [,4]
#[1,]  0.01350441 -0.8948596  0.1311110 -0.07483831
#[2,]  1.31393412  0.6889527 -1.1721002 -0.43178911
#[3,] -0.90334049  0.7942160 -0.8882906 -0.60249284
#[4,]  0.44311300  0.1596960  0.1444491 -2.06023956
#> cacheSolve(m)
#[,1]        [,2]       [,3]        [,4]
#[1,] -0.1486892  0.38299670 -0.5149950  0.07573617
#[2,] -1.2242369 -0.06816609 -0.0775105  0.08142389
#[3,] -0.8184026 -0.47989860 -0.5652563  0.29560919
#[4,] -0.1842550  0.04344350 -0.1564041 -0.44205382
#> cacheSolve(m)
#Getting cached matrix.
#[,1]        [,2]       [,3]        [,4]
#[1,] -0.1486892  0.38299670 -0.5149950  0.07573617
#[2,] -1.2242369 -0.06816609 -0.0775105  0.08142389
#[3,] -0.8184026 -0.47989860 -0.5652563  0.29560919
#[4,] -0.1842550  0.04344350 -0.1564041 -0.44205382
#>
