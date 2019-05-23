show_duplicated <- function(x){
  x[duplicated(x, fromLast = T)]
}
