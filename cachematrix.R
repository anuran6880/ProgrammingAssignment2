## makeCacheMatrix creates a special matrix object, which is used to cache the
## inverse of a matrix. cacheSolve calculates the inverse of the special matrix 
## returned by makeCacheMatrix

## makeCacheMatrix creates a special matrix object. It returns a list of 4 
## functions:
## 1. get():Returns the original matrix
## 2. set():Sets the value of the matrix
## 3. getInverse(): Returns the inverse of the matrix
## 4. setInverse(): Sets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	get<-function()x
	set<-function(y){
		x<<-y
		inv<<-NULL
	}
	getInverse<-function()inv
	setInverse<-function(inverse)inv<<-inverse
	list(get=get,set=set,getInverse=getInverse,setInverse=setInverse)
}


## cacheSolve returns the inverse of the special matrix object returned by the 
## makeCacheMatrix function. It first checks that whether the inverse has 
## already been calculated and stored. If it has already been calculated, then 
## it does not recalculate the inverse and returns the value from cache. 
## Otherwise, it calculates the inverse, stores it in the cache and returns 
## the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		#Checking to see if the inverse is already present in the cache
    inv<-x$getInverse()
		if(!is.null(inv))
		{
			message("getting cached inverse")
			return(inv)
		}
#Calculating the inverse
		data<-x$get()
		inv<-solve(data)
		x$setInverse(inv)
		inv
}
