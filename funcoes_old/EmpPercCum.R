#' Sum of vector elements.
#'
#' \code{sum} returns the sum of all the values present in its arguments.
#'
#' This is a generic function: methods can be defined for it directly
#' or via the \code{\link{Summary}} group generic. For this to work properly,
#' the arguments \code{...} should be unnamed, and dispatch is on the
#' first argument.
#'
#' @param ... Numeric, complex, or logical vectors.
#' @param na.rm A logical scalar. Should missing values (including NaN)
#'   be removed?
#' @return If all inputs are integer and logical, then the output
#'   will be an integer. If integer overflow
#'   \url{http://en.wikipedia.org/wiki/Integer_overflow} occurs, the output
#'   will be NA with a warning. Otherwise it will be a length-one numeric or
#'   complex vector.
#'
#'   Zero-length vectors have sum 0 by definition. See
#'   \url{http://en.wikipedia.org/wiki/Empty_sum} for more details.
#' @examples
#' sum(1:10)
#' sum(1:5, 6:10)
#' sum(F, F, F, T, T)
#'
#' sum(.Machine$integer.max, 1L)
#' sum(.Machine$integer.max, 1)
#'
#' \dontrun{
#' sum("a")
#' }


EmpPercCum <- function(percemp){
  percemp <- percemp[[1]]
  
  numalt <- nrow(percemp[[1]])
  numit <- length(percemp)
  
  percempcum <- list()
  
  for(i in 1:numit){
    aux = list()
    aux[[1]] <- colSums(percemp[[i]])    
    for(j in 2:numalt){
      aux[[j]] <- aux[[j-1]] - percemp[[i]][j-1,]
    }
    percempcum[[i]] <- do.call(rbind,aux)
  }
  
  percempcum1 <- percempcum
  for(i in 1:numit){
    percempcum1[[i]] <-  t(t(percempcum1[[i]])/percempcum1[[i]][1,])
  }  
  percemp1 <- list()
  percemp1[[1]] <- percemp
  percemp1[[2]] <- percempcum
  percemp1[[3]] <- percempcum1
  
  percemp1
}

