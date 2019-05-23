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

imputfac <- function(profic,fquest,questgeral,listar,varnome) {
  ncomp <- dim(fquest)[2]
  ndisc <- dim(profic)[2]
  for(i in 1:ncomp) {
    ncoluq <- length(levels(fquest[,i])) - listar[i]
    taux <- is.element(fquest[,i],c(LETTERS[ncoluq+1],"","*"))
    prof <- matrix(rep(NA,(sum(taux))*ndisc),ncol=ndisc)
    profref <- matrix(rep(NA,(ncoluq*ndisc)),ncol=ndisc)
    for (j in 1: ndisc) {
      prof[,j] <- profic[taux,j]
      profref[,j] <- questgeral[[i]] [1:ncoluq,varnome[j]]
    }
    raloc <- aloc(prof,profref)
    fquest[taux,i] <- raloc
    fquest[,i] <- factor(fquest[,i],levels=LETTERS[1:ncoluq])
  }
  imputfac <- fquest
  imputfac
}
