## The two functions below will:
##   makeCacheMatrix - create a matrix object that can cache its inverse
##   cacheSolve - compute the inverse of the matrix returned by makeCacheMatrix or, 
##   if the inverse matrix has already been cached, retrieves the inverse from the cache

## Create a matrix object and stores the inverse matrix
makeCacheMatrix <- function(mat = matrix()) {

        #mat, ymat
        #matinv, minv
        
        minv= NULL                 #use minv to store the matrix inverse
        
        set = function(ymat){
                mat <<- ymat       #saves the input matrix into "mat" 
                minv <<- NULL      #initiates the inverse matrix to be NULL
        }
        
        get = function() mat      #returns the value of the original matrix
        
        setinvmat = function(matinv) minv <<- matinv  #stores the inverted matrix 
        
        getinvmat = function() minv   #After initial cacheSolve(), this returns the cached inverse matrix
        
        list(set=set, get=get, setinvmat=setinvmat, getinvmat=getinvmat)   
}


## Reads the matrix object created by makeCacheMatrix and returns the 
## matrix inverse; if available, a cached inverse matrix will be returned.
cacheSolve <- function(mat, ...) {
        
        minv = mat$getinvmat()
        
        if(!is.null(minv)){                             #check to see if minv has a non-null value
                message("Getting cached data")
                return(minv)                            #if so, return it
        }
        
        data = mat$get()
        
        minv = solve(data, ...)                         #minv = invert mat
        
        mat$setinvmat(minv)
        
        minv
}
