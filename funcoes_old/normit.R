#' Calula o Normit
#' 
#' @description
#' Esta funcao calcula o Normit para um dado conjunto
#'
#' @param scores Vetor com os scores para o calculo do normit
#' @param caderno Vetor com os cadernos para cada um dos alunos (mesma ordem de scores)
#' @param nitemform numero de itens por caderno
#' @param peso  peso para cada um dos alunos
#'
#' @return Retorna o normit
#' 
#' @examples
#' normit...


normit <- function(scores, caderno, nitemform, peso){
  
  #tabulando numero de alunos por caderno e score
  (t1 <- matrix(as.integer(Hmisc::mApply(peso, list(factor(scores,0:nitemform), caderno), sum)), ncol = nitemform+1))
  (t1[is.na(t1)] <- 0)
  
  # acumulando o numero de alunos
  t2 <- t(apply(t1,1,cumsum))
  
  # criando a saida da tabela 3
  t3 <- matrix(ncol=ncol(t1),nrow = nrow(t1))
  for( i in 1:ncol(t2)){
    if(i == 1){
      t3[,i] <- (t2[,i] + t2[,i+1]/2)/(2*t2[,nitemform+1])
    }else{
      t3[,i] <- (t2[,i] + t2[,i-1])/(2*t2[,nitemform+1])
    }
  }
  
  # gerando o normit
  t4 <- qnorm(t3)
  t4[t3==1] <- 4
  t4[t3==0] <- -4
  
  aux <- cbind.data.frame(t1 = as.vector(t1), t2 = as.vector(t2), t3 = as.vector(t3), normit = as.vector(t4))
  aux$cad <- 1:nrow(t1)
  aux$nacer <- rep(c(0:nitemform),rep(nrow(t1),nitemform+1))
  rownames(aux) <- paste(sprintf('%03d',aux$nacer),sprintf('%03d',aux$cad),sep='')
  
  normit <- aux[paste(sprintf('%03d',scores),sprintf('%03d',caderno),sep=''),'normit']
  normit
  
}
