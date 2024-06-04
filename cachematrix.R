## The cachematrix.R file contains two functions, makeCacheMatrix() and cacheSolve(). 

## The first function in the file, makeCacheMatrix() creates an R object that 
## stores a matrix and its inverse.

## The second function cacheSolve() requires as argument the output returned by 
## makeCacheMatrix() in order to retrieve the inverse from the cached value that 
## is stored in the makeCacheMatrix() object's environment, or compute it 
## whenever such inverse matrix has not being previously calculated and stored in 
## makeCacheMatrix().


########################################
## Description for makeCacheMatrix() ##
########################################

## The first thing that occurs in makeCacheMatrix() is the initialization of two objects, x and inv.
## x is a matrix (assumed to be invertible), and it is part of the function arguments.
## inv is set to NULL, initializing it as an object within the makeCacheMatrix() environment.
## inv will be used heavily later in the function, and it will represent the inverse matrix of x.

## From this point onward, makeCacheMatrix() will define 4 functions that will allow
## us to access and/or modify the information inside makeCacheMatrix() (in this case, x and inv).

## Function 1: set() takes an argument that is named as y. 
## Within set() we use the <<- form of the assignment operator (instead of the typical <-), 
## which assigns the value of objects to the parent environment.
## When set() is executed, it does two things:
## Assign the input argument to the x object (the matrix) in the parent environment, and
## Assign the value of NULL to the inv object in the parent environment. 
## The second is needed in case we set a new value for the matrix x, in which case 
## the inverse matrix inv should also be reset.

## Function 2: get() retrieves the value of x previously set.
## The symbol x is not defined within get(), 
## but R retrieves it from the parent environment of makeCacheMatrix(), which is why 
## we need to use <<- when setting x inside the set() function.

## Function 3: setinv() defines the value for inv.
## The only argument of setinv() is 'inverse', which is just the label use
## for the inverse matrix of x. This function defined inv as:
## inv <<- inverse, the assignment operator <<- is used set the value for 
## inv in the parent environment.

## Function 4: getinv() retrieves the value of inv previously set. Very similar
## to the description for get() before.

## Finally, at this point we have all the functions that we will use to either 
## access and/or modify the information inside makeCacheMatrix(). We just need to
## create a new object that comprises these 4 functions. We use a list object, which
## are the last lines of the function.

## At the end, every time we call makeCacheMatrix(), R will produce an object
## which will contain four functions: set(), get(), setinv(), and getinv(),
## as well as the two data objects, x and inv.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

##################################
## Description for cacheSolve() ##
##################################

## Essentially, cacheSolve() is designed to either populate and/or retrieve 
## the inverse matrix from an object of type makeCacheMatrix().

## cacheSolve() starts with a single argument, x, this will be an object of "type" 
## makeCacheMatrix(). The "..." are required to allow for the passing additional 
## arguments into the function.

## cacheSolve() then tries to retrieve the value of the inverse stored in the
## object x, using inv <- x$getinv().

## Then we enter a conditional statement. If the value for inv is NULL then it
## it ignores the lines inside the if statement. If the value is different from NULL
## (a well-defined inverse matrix) the it enters the lines inside the if statement.

## Once inside the if statement, the function will just print the message
## "getting cached data", and use the data retrieved from inv. Therefore, R will
## just use the CACHED INVERSE and return it to the parent environment.

## If the if statmenet was ignored, because inv is NULL the cacheSolve() will do three steps
## essentially. First, it will retrieve the matrix we want to invert using the object x.
## This is done using the function get, data <- x$get(). 
## Therefore inside this cacheSolve() the matrix we want to
## find the inverse of is called data. Second, we find the inverse of data using the
## preset function solve(), this is done by inv <- solve(data, ...). Now, the 
## variable inv is modified and equals the inverse of the matrix called data.
## Thirdly, we store the solution of inv in the object x. This is done using the
## function setinv(), x$setinv(inv). Finally, we return inv as the solution for 
## cacheSolve.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

