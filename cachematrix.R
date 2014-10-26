# Two functions below makeCachematrix and cacheSolve allow calculation of a matrix inverse
# And recall of said inverse if and only if it's already been calculated.

# 1) makeCacheMatrix
# makeCacheMatrix takes a matrix 'x' ,creates four simple subfunctions with
# associated memory caches # in the global environment, and returns them as a list
# which is associated with the current value of x.
# The purpose of these subfunctions are commented within CacheMatrix
# CacheMatrix doesn't, for example, calculate the inverse of x, but it
# creates an instruction to do so if necessary 

makeCacheMatrix <- function(x = matrix()) {

mm <- NULL			# so if we're running this for the very first time we 
				# set mm to null - this is needed in CacheSolve as 
				# this looks for mm and calls a calculation if mm is null
        setm <- function(y) {
                x <<- y
                mm <<- NULL
        }		# this subfunction records the current value of x and records the
			# fact that its index isn't set
        getm <- function() x	#this simply returns the matrix we started with 
        setinv <- function(solve) mm <<- solve  # this function provides 
								# a space in the global environment
								# for a calculated inverse to be stored
        getinv <- function() mm			# if we have an inverse, go and get it.
						
        list(setm = setm, getm = getm,
             setinv = setinv,
             getinv = getinv)
}


# 2) cacheSolve
# cacheSolve acts on the function lists created by makeCacheMatrix 
# This causes R to look for an existing inverse relating to the matrix in question
# and either return it or calculate and store it.
# NB makeCachematrix (x) must be defined as a variable in the global environment
# and cacheSolve executed as a function of said variable
# otherwise R thinks it's calculating a fresh inverse

cacheSolve <- function(x, ...) {
		
	  mm <- x$getinv()

	# Note that if we wanted error debugging we could add the original matrix 
	# as an additional function argument (...,matrix,...) and 
	# put the error checking in here
	# eg "if(det(matrix)==0) {stop ("error:  x is not an invertible matrix")}"


        if(!is.null(mm)) {
                message("getting cached data")
                return(mm)
        }
               data <- x$getm()

         mm <- solve(data, ...)

	  x$setinv(mm)
        mm
}
