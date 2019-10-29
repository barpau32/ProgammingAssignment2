## Guardar a la memòria cau la inversa d'una matriu.

## Creem una matriu que pot guardar al "cache" la seua inversa.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        set_inv <- function(inverse) inv <<- inverse
        get_inv <- function() inv
        list(set = set,
             get = get,
             set_inv = set_inv,
             get_inv = get_inv)

}


## Es calcula o es representa desde la memòria cau, la inversa de la matriu generada abans.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$get_inv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$set_inv(inv)
        inv
}
