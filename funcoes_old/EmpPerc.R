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

EmpPerc <- function(items, responses,vecform,profi,resp_possible,nitems,nitemform,nforms,min_break,max_break,by_break,escala0100=F){
  
  (cut_break <- c(-Inf,seq((min_break+by_break/2),(max_break-by_break/2),by_break),Inf))
  (if(escala0100==T) cut_break <- c(-Inf,0.1,cut_break[-c(1:2,length(cut_break)-1,length(cut_break))],99.9,Inf))
  (exib_break <- seq((min_break) , (max_break), by_break))
  (if(escala0100==T) exib_break <- c(0,exib_break[-c(1,length(exib_break))],100))
  
  it_level <- (cut(profi,cut_break,labels = exib_break,include.lowest=T,right=F))
  
  #as.matrix(table(vecform,cut(profi,cut_break,labels = exib_break,include.lowest = TRUE, right = FALSE)))
  
  ### criando matriz de respostas
  resp_matrix <- data.frame(do.call(rbind,strsplit(responses,NULL)),stringsAsFactors=F)
  for(i in 1:nitemform){
    resp_matrix[,i] <- factor(resp_matrix[,i],levels=resp_possible)
  }
  
  
  #n?mero total de respondentes
  nalu <- length(responses)
  
  #n?mero de alternativas (incluindo branco/inv?lido e n?o-apresentado)
  nalt <- length(resp_possible)
  
  
  itempos <- ItemPos(items,nitems,tipo='character')
  
  EmpPerc <- list()
  EmpPerc[[1]] <- list()
  EmpPerc[[2]] <- list()
  for(i in 1:nitems){
    (df_empperc <- data.frame(matrix(NA,ncol=2,nrow=nalu)))
    (df_empperc[,1] <- factor(df_empperc[,1],levels=exib_break))
    (df_empperc[,2] <- factor(df_empperc[,2],levels=resp_possible))
    (nlin <- nrow(itempos[[i]]))
    for(j in 1:nlin){
      (form = itempos[[i]][j,'form'])
      (pos  = as.numeric(itempos[[i]][j,'pos']))
      (aux <- vecform==form)
      (df_empperc[aux,1] <- it_level[aux])
      (df_empperc[aux,2] <- resp_matrix[aux,pos])
    }
    #(df_empperc <- df_empperc[df_empperc[,1]!='0',])
    (EmpPerc[[1]][[i]] <- as.matrix(table(df_empperc[,2],df_empperc[,1])))
    EmpPerc[[2]][[i]] <- EmpPerc[[1]][[i]]
    for(k in 1: nrow(EmpPerc[[1]][[i]])){
      EmpPerc[[2]][[i]][k,] <- EmpPerc[[1]][[i]][k,]/colSums(EmpPerc[[1]][[i]])
      EmpPerc[[2]][[i]][,colSums(EmpPerc[[1]][[i]])==0] <- 0
    } 
    
  }
  EmpPerc
}


