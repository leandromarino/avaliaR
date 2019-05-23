#' Not in
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %!in%
#' @keywords internal
#' @export
#' @usage 1:4 \%\!in\% 1:10

`%!in%` <- function (x, table){
  !(match(x, table, nomatch = 0L) > 0L)
}

