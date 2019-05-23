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

ItemLevelCum <- function(empperccum,num_min=20,min_break=NULL,max_break=NULL,ndec=2,escala0100=T){
  
  
  (aux <- empperccum[[1]][[1]])
  
  (if(escala0100==F) min_breakb <- as.numeric(colnames(aux)[1]))
  (if(escala0100==F) max_breakb <- as.numeric(colnames(aux)[ncol(aux)]))
  
  (if(escala0100==T) min_breakb <- as.numeric(colnames(aux)[1]))
  (if(escala0100==T) max_breakb <- as.numeric(colnames(aux)[ncol(aux)]))
  
  (if(escala0100==F) by_break <- as.numeric(colnames(aux)[2]) - as.numeric(colnames(aux)[1]))
  (if(escala0100==T) by_break <- 10)
  
  
  (if(!is.null(min_break)) {min_breakb <- min_break})
  (if(!is.null(max_break)) {max_breakb <- max_break})
  
  
  (breaks <- seq(min_breakb,max_breakb,by_break))
  (qtd_breaks <- length(breaks))
  (nitems <- length(empperccum[[1]]))
  (alternativas <- rownames(empperccum[[1]][[1]]))
  (nalt <- length(alternativas))
  (qtd_cols <- ncol(empperccum[[1]][[1]]))
  
  tmpItemLevel <- list()
  
  tmpItemLevel[[1]] <- cbind.data.frame(It=rep(1:nitems,rep(length(alternativas),nitems)),
                                        Alt=rep(alternativas,nitems),
                                        do.call(rbind,empperccum[[3]]),
                                        stringsAsFactors=F)
  
  
  for(i in 1:(qtd_breaks+2)){
    tmpItemLevel[[1]][is.nan(tmpItemLevel[[1]][,i]),i]  <- NA
  }
  
  
  tmpItemLevel[[2]] <- tmpItemLevel[[1]]
  tmpItemLevel[[2]][,-c(1:2)] <- data.frame(matrix(as.numeric(tmpItemLevel[[1]][,-c(1:2)]  >= .65),nrow=nitems*nalt))
  tmpItemLevel[[1]]$level <- tmpItemLevel[[2]]$level <- NA
  
  
  for(i in 1:nitems){
    
    (a1 <- tmpItemLevel[[1]][tmpItemLevel[[1]]$It==i,-c(1,2)])
    
    (a2 <- colSums(empperccum[[1]][[i]]))
    
    
    (level_min <- as.integer((names(a2)[a2>=num_min])[1]))
    (level_max <- as.integer((names(a2)[a2>=num_min])[sum(a2>=num_min,na.rm=T)]))
    (if(!is.null(max_break)){ level_max = max_break})
    (if(!is.null(min_break)){ level_min = min_break})
    (level_breaks <- seq(level_min,level_max,by=by_break))
    
    for(j in 1:nalt){
      (auxItemLevel1 <- tmpItemLevel[[1]][j+(i-1)*4,paste(level_breaks,sep='')])
      (auxItemLevel2 <- tmpItemLevel[[2]][j+(i-1)*4,paste(level_breaks,sep='')])
      (aux <- which(auxItemLevel2==0)+1)
      
      if(length(aux[length(aux)])==0){
        tmpItemLevel[[1]]$level[j+(i-1)*4] <- tmpItemLevel[[2]]$level[j+(i-1)*4] <- paste("<",level_min,sep='')
      }else{
        if(aux[length(aux)] > length(level_breaks)){
          tmpItemLevel[[1]]$level[j+(i-1)*4] <- tmpItemLevel[[2]]$level[j+(i-1)*4] <- 'N.A.'
        }else{
          tmpItemLevel[[1]]$level[j+(i-1)*4] <- tmpItemLevel[[2]]$level[j+(i-1)*4] <- level_breaks[aux[length(aux)]]
        }
      }
    }
    
  }
  
  tmpItemLevel[[1]][,colnames(empperccum[[1]][[1]])] <- round(tmpItemLevel[[1]][,colnames(empperccum[[1]][[1]])],ndec)
  
  
  ItemLevel <- list()
  ItemLevel[[1]] <- tmpItemLevel[[1]]
  ItemLevel[[2]] <- tmpItemLevel[[2]]
  ItemLevel
  
}


