##This project aims to wite a pair of functions that creates a special
##matrix object and another function which will compute the inverse of this matrix.
##If the function sees the inverse is already computed it will simply 
##fetch the cached data else it will compute the inverse. 

##This function creates a special matrix object and defines the getter 
##and setter functions.
makeCacheMatrix <- function(x = matrix()) {
        
        m<-NULL
        set<-function(y)
        {
                x<<-y
                m<<-NULL
        }
        get<-function()x
        setinverse<-function(inverse)m<<-inverse
        getinverse<-function()m
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}
##This Function Computes the inverse of a matrix if the inverse is not already stored else it fetches the stored data and displays it to the user
cacheSolve <- function(x, ...) {
        m<-x$getinverse()
        if(!is.null(m))
        {
                message("Getting Cached Data")
                return(m)
        }
        data<-x$get()
        m<-solve(data, ...)
        x$setinverse(m)
        m          ## Return a matrix that is the inverse of 'x'
}
##Solution
##m <- matrix(1:4,2,2)
##> m1 <- makeCachematrix(m)
##> cacheSolve(m1)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
