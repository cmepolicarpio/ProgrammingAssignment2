## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special "matrix" 
# object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
	#get matrix value
    get <- function() x
    set <- function(n){
        x <<- n
        inv <<- NULL
    }
    
    #calculate inverse of matrix
    getInverse <- function() inv
    setInverse <- function(f){
        data <- f$get() #get matrix
        print(data)
        inv <<- solve(data) #calculate
    }
    
    list(get = get,
         set = set,
         getInverse = getInverse,
         setInverse = setInverse)
}


## Write a short comment describing this function
# This function returns a matrix that is the inverse of 'x'

# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been
# calculated (and the matrix has not changed), then the cachesolve
# should retrieve the inverse from the cache

cacheSolve <- function(x) {
	print("start")
    inv <- x$getInverse()
    print(inv)
    
    
    #get value of matrix
    m <- x$get()
    #print(m)
    
    #get value of inverse
    inv <- x$getInverse()
    
    if(is.null(m) || is.null(inv)){
        print("calculating inverse matrix")
        x$setInverse(x)
    }
    
    ret <- x$getInverse()
    
    ret
}

#test2 <- makeCacheMatrix(mydata)
#cacheSolve(test2)

