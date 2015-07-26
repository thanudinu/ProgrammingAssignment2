## makeCacheMatrix - Returns a matrix and that can also cache its inverse
## cacheSolve      - computes the inverse of the matrix returned by makeCacheMatrix.
##                   If the inverse has already been calculated (and the matrix has not changed), 
##                   then the cachesolve would retrieve the inverse from the cache
## is_inv_mat      - checks if the provided data is convertible to a square matrix
## makemat         - converts a vector to matrix
## make_invmat     - returns inverse of a matrix

## The following function calcualtes inverse of a matrix if it is square matrix and if the matrix has non-zero determinant
## This function returns a list containing functions to set the matrix, get the matrix, set the inverse matrix and get the inverse matrix


## makeCacheMatrix()

makeCacheMatrix <- function(x = matrix()) {
        inv_mat <- NULL
        makemat <- NULL
        make_invmat <- NULL
        org_mat <- NULL
        ## Check if the provided vector can be converted to a square matrix
        ## Only square matrices can have inverse
        is_inv_mat <- function(a) {
                if((sqrt(length(a))%%1) == 0) return(TRUE)
                else return(FALSE)
        }
        
        ##Converts a vector into a matrix
        makemat <- function(y) {
                if(is_inv_mat(y)) {
                        y <- matrix(y,nrow=sqrt(length(y)),ncol=sqrt(length(y)))
                } else {
                        message("Not a Square Matrix - No Inverse Matrix")
                        y <- NULL
                } 
                y
        }
        ##Calculate inverse of a matrix after checking if it is a matrix, if it is a square matrix and if its determinant is Non-Zero
        make_invmat <- function(z) {
                if(is.matrix(z) && is_inv_mat(z) && (det(z)!=0)) {
                        solve(z)
                } else {
                
                if(det(z)== 0) message("This Matrix doesn't have an Inverse as it's Determinant is Zero")
                return(NULL)
                }
        }
        
        ##Convert the Initial Data into Matrix
        org_mat <- makemat(x)
        ## Set a matrix 
        set_mat <- function(k) {
                ##Sets new matrix if the existing is not the same and resets cache 
                if(!is.null(makemat(k))) {
                        ##Check if existing matrix is same as the new matrix
                        if(!identical(org_mat,makemat(k))) {
                                org_mat <<- makemat(k)
                                inv_mat <<- NULL
                                message("New Matrix Set")
                        } else {
                          message("New Matrix is same as the Old (Existing) Matrix")      
                        } 
                }
        }
        
        ## Returns the original matrix or New mtrix if original data is changed
        get_mat <- function() org_mat
        
        set_invmat <- function(invmat) inv_mat <<- invmat ## This sets up a inverse matrix if the data is run for first time or if the original data is changed
        
        ##Retrieves the Inverse Matrix if it exists
        get_invmat <- function() inv_mat
        
        list(set_mat = set_mat, ##list of functions returned
             get_mat = get_mat,
             set_invmat = set_invmat,
             get_invmat = get_invmat,
             make_invmat = make_invmat)
        

}


## The following function computes inverse of a matrix returned by makeCacheMatrix() Function
## If the Inverse is already calculated (and no new matrix is set), it retreives the inverse matrix from the cache memory

cacheSolve <- function(x, ...) {
        
        inv_mat <- x$get_invmat() ## Access the Object from the makeCacheMatrix function and get inverse matrix       
        if(!is.null(inv_mat)) {   ## Return a matrix if already a inverse exists in cache
                message("getting cached data")
                return(inv_mat)
        }
        mat <- x$get_mat() ## Get the Matrix from makeCacheMatrix() function if there is no Inverse in Cache
        inv_mat <- x$make_invmat(mat) ## Compute the Inverse Matrix
        x$set_invmat(inv_mat) ## Store it in the Cache
        inv_mat
}
