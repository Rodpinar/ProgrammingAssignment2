## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

  #Assume that M is an square (n x n) and invertible matrix     
 makeCacheMatrix <- function(x=matrix()){
               InvM <- NULL
               set <- function(y){
                      x <<- y
                      InvM <<- NULL
         
               }
       
              catchMatrix = function () x
              setInvMatrix = function (InverseM) InvM <<- InverseM
              getInvMatrix =function () InvM
              
              # create the list of the 4 functions
              list( set = set, catchMatrix = catchMatrix, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
       
        }
     
      
cacheSolve <- function(x,...){
                 
                 InvM <- x$getInvMatrix()  # x: output of the function makeCaheMatrix
                                           # to return the inverse of the original matrix input
                                           # to makeCacheMatrix
                    
                  if (!is.null(InvM)){ message("getting Matrix to cache")
                          
                                            # if the inverse exist
                      return (InvM)}        # take it from the cache , avoid calculation else calculate
                  
                     
                     mat.data <- x$catchMatrix()  #pass matrix to cache
                     InvM <- solve(mat.data,...)
                       
                     x$setInvMatrix(InvM)     # the result of the inverset matrix is assign to the cache with
                                             # the setInvMatrix function
                      
                    InvM
              }
     
     
     
