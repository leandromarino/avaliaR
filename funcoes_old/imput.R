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

imput <- function(profic,fquest,questgeral,listaq,listar,varnome) {
  #listar: 2 quando nao ha "nao sei", 3=quando ha "nao sei"
  ncomp <- length(listaq)
  ndisc <- dim(profic)[2]
  for(i in 1:ncomp) {
    k <- listaq[i]
    ncoluq <- length(levels(fquest[,k])) - listar[k]
    taux <- is.element(fquest[,k],c(LETTERS[ncoluq+1]," ","*"))
    prof <- matrix(rep(NA,(sum(taux))*ndisc),ncol=ndisc)
    profref <- matrix(rep(NA,(ncoluq*ndisc)),ncol=ndisc)
    if(sum(taux)==0){
    }else{
      for (j in 1: ndisc) {
        prof[,j] <- profic[taux,j]
        profref[,j] <- questgeral[[i]] [1:ncoluq,varnome[j]]
      }
      raloc <- aloc(prof,profref)
      fquest[taux,k] <- raloc
    }
  }
  imput <- fquest
  imput
}

