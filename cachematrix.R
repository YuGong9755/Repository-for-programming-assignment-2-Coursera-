## This coding is revised for assignment 3 

## makeCheMatrix function is constructed for an environment which defines several function for cache
## I don't wish explain too much because we can find more explanations in https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md

## p.s.: tutorial for push this r coding: https://github.com/AlanBerger/ProgrammingAssignment2/blob/master/slides%20for%20walk%20through%20of%20forking%20a%20repository%20cloning%20it%20and%20pushing%20changes%2019%20May%202020.pdf


makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setinverse<-function(inverse) m<<-inverse 
        getinverse<-function() m 
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function is the one which calculates the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse()
        if(!is.null(m)){
                message("Print the cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data)
        x$setinverse(m)
        m
}

## simple test 
m1 <- makeCacheMatrix(matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2))
cacheSolve(m1)
m2<- cacheSolve(m1)
m1$set(m2)
cacheSolve(m1)