## The functions in this file allow for faster inversing of matrices by preventing
## duplication of work. Unfortunately, they are very memory intense for large
## matrices, due to the requirement of 3 matrices per matrix. A matrix is passed
## into makeCacheMatrix as x<-makeCacheMatrix(x). Details of the output can be
## found below. Similarly, x<-cacheSolve(x) will check for a chached inverse of
## the matrix created by the 1st function. If it exists, it is returned. Otherwise
## the matrix is inverted and the inverse cached. Details can be found below.
library(compare)

## This function creates a list out of the matrix. The first element is the matrix
## itself, the second is a check matrix to make sure the function hasn't been
## modified, and the third is the inverse to be cached. Elements 2:3 are 0s.

makeCacheMatrix <- function(x = matrix()) {
    cache<-as.matrix(0,0)
    changecheck<-matrix(0,nrow=nrow(x),ncol=ncol(x))
    
    y<-list(x,changecheck,cache)
}


## The correct way to call this function is x<-cacheSolve(x). Using the compare
## package, the 1st and 2nd elements of the passed list are checked to see if
## they are identical or mathematically equivalent. If they are, then the function
## has already been run, and is still accurate. The cached inverse of the matrix
## is retrieved. Otherwise, the 2nd element is set to be equal to the value of 
## the 1st, and the 3rd element is set equal to the inverse.

cacheSolve <- function(x, ...) {
        q<-compare(x[[2]],x[[1]])
        if (as.logical(q[1])){
            message("Retrieving cached inverse")
            print(x[[3]])
            invisible(return(x)) 
        }
        else{
            message("Calculating and caching inverse")
            x[[2]]<-x[[1]]
            x[[3]]<-solve(x[[2]])
            print(x[[3]])
            invisible(return(x))
        }    
}