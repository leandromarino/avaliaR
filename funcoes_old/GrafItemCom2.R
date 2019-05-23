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

GrafItemCom2 <- function(dirgrf,nomgrf,anos,gabitem,itcom1,param1,rj1,percpr1,itcom2,param2,rj2,percpr2,ger,geral){
  xx <- seq(-5,5,0.25)
  icex <- 0.35
  xq <- rj1[1,]
  rj1 <- rj1[-1,]
  rj2 <- rj2[-1,]
  comp <- length(itcom2)
  ip <- 0
  ng <- comp
  for (ig in 1:ng)  {
    png(paste(dirgrf,nomgrf,'_',sprintf("%03d",ig),"_",sprintf("%5s",gabitem[itcom2[ig]]),".png",sep=""))
    ip <- ip+1
    ip1 <- itcom1[ip]
    ip2 <- itcom2[ip]
    item <- param2[ip2,"it"]
    bloco <- param2[ip2,"bl"]
    ob <- param2[ip2,"ob"]
    ibg <- param2[ip2,"itemblg"]
    aa <- param2[ip2,"aorig"]
    bb <- param2[ip2,"borig"]
    cc <- param2[ip2,"c"]
    matplot(xx, plogis3(xx, aa, bb, cc), type = "l", ylim = c(0, 1),
            main = paste(geral[ger],anos[1],
                         paste("It", param1[ip1,"it"]),
                         paste(" Bl", param1[ip1,"bl"]), paste("Ob", param1[ip1,"ob"]),
                         paste("Ibg", param1[ip1,"itemblg"]),'\n',
                         geral[ger],anos[2],
                         paste("It", item), paste(" Bl", bloco), paste("Ob", ob),
                         paste("Ibg", ibg),'\n'), cex   = icex,
            xlab="Proficiência",ylab="Probabilidade")
    mtext(paste("ITEM:",sprintf("%5s",gabitem[itcom2[ig]])),line=0.5,outer=F,side=3,cex=1.1)
    points(xq,rj1[ip1,],pch=1)
    points(xq,rj2[ip2,],pch=16)
    abline(v=percpr1[2],lty=5)
    abline(v=percpr1[8],lty=5)
    abline(v=percpr2[2],lty=3)
    abline(v=percpr2[8],lty=3)
    abline(h=0.65,lty=2)
    legend('topleft',pch = c(1,16),anos,bty="n")
    dev.off()
  }
}


