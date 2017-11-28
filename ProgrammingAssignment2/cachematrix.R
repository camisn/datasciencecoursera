## This functions allows us to create a diferent type of matrix
## that can get and set the data, also set and get inverse matrix,
## the other function see if its set de inverse matrix if it is
## not the set it.

## This function create a diferent type of matrix, and you can
## set and get the data, also you can do the same with the inverse, the 
## default value is NULL.
## Esta funcion te permite crear un tipo diferente de matriz, en donde 
## le asigna o obtiene la informacion, ademas tambien puede realizar
## las mismas operaciones para la inversa, su valor por defecto es NULL.

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y) {
        x <<- y
        inver <<- NULL
    }
    get <- function() x
    setinversa <- function(inversa) inver <<- inversa
    getinversa <- function() inver
    list(set = set, get = get,
         setinversa = setinversa,
         getinversa = getinversa)
}



## This function allows us to see if a matrix have seted the inverse
## matrix or set the inverse matrix.
## esta funcion te permite obtener el valor de la inveras de la matriz si ya
## esta seteado o setearlo si no lo tiene.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inver <- x$getinversa()
    if(!is.null(inver))
    {
        message("Getting data ~ Obteniendo informacion")
        return(inver)
    }
    data <- x$get()
    inver <- solve(data, ...)
    x$setinversa(inver)
    inver
}
