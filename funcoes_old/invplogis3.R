invplogis3 <- function(p, a, b, ce){
  theta = log ( (p-ce)/(1-p) ) /a + b  
  theta
}
