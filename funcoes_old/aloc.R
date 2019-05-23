#' Alocacao das Respostas
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

aloc <- function(prof,profref) {
  profref[is.na(profref)] <- 2*max(profref, na.rm = TRUE)
  nlin <- length(prof[,1])
  ncolu <- length(profref[,1])
  tempmat <- matrix(rep(NA,nlin*ncolu),nrow=nlin,ncol=ncolu)
  for (j in 1:ncolu) {
    temp <- matrix(rep(NA,nlin*length(prof[1,])),nrow=nlin)
    for (i in 1:length(prof[1,])) {
      temp[,i] <- (prof[,i] - profref[j,i])^2
    }
    tempmat[,j] <- apply(temp,1,sum,na.rm=T)
  }
  tempmat <- tempmat - apply(tempmat,1,min,na.rm=T)
  taloc <- apply(tempmat,1,First.zero)
  aloc2 <- LETTERS[taloc]
  aloc2
}
