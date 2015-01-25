#The first function, makeCacheMatrix creates a list 
#which contains the four functions: set, get,
#setmatrix, get matrix.
#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  #sets the value of the matrix
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  #gets the value of the matrix
  get<-function() x
  #sets the value of the inverse
  setmatrix<-function(solve) m<<- solve
  #gets the value of the inverse
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}
#This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. 
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  
  #checks to see if the inverse has already been calculated
  #If so, it gets the inverse from the cache and skips the computation
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  #Otherwise, it calculates the mean of the data and sets the value of the inverse
  #in the cache via the setmean function
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
#The function returns the value of the inverse matrix