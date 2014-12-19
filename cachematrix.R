#Said Yakhyoev
#invertable matrix functions


# this function takes a matrix or a numeric vector and returns a constructed a square matrix.
# the constructed matrix is saved, later to be checked if a repeat request is being processed

makeCacheMatrix <- function(matrix1 = ()) {
    cache <<- NULL                  # if makeCacheMatrix is called, cache needs re-caclc. Empty cache, save step in next func
    mat_side = sqrt(length(matrix1))
    cached_matrix <<-matrix1        # this is to check later if original input has changed
    constructed_matrix <- matrix(matrix1, nrow = mat_side, ncol = mat_side)
    return(constructed_matrix)
    
}

# Solves matrix or returns existing cached result
# first checks if cache already exists and whether a pre-exisitn matrix is being requested again
cacheSolve <- function(matrix1,...){
    
    # if cache exists, check that result of the same matrix is requested. Return existing cache
    if (!is.null(cache)) {
        if (identical(matrix1, cached_matrix)){
            print('Using existing cached data:')
            return (cache)
        }
        # if cache exist, but a calculation of a new matrix is needed, re-calculate
        else{
            print('Recalculating with a new matrix:')
            cache <<- solve(makeCacheMatrix(matrix1))
            return(cache)
        }        
    }
    # if cache is altogether empty, perform calculation
    else {   
        print('Cache is empty. Solving inverse matrix:')
        cache <<- solve(makeCacheMatrix(matrix1)) 
        return (cache)
    }
}