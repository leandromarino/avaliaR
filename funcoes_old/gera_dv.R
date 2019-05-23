# vetor <- substring(alunos01[1:100,'codigo_aluno'],1,8)
# vetor <- 1:10
gera_coddv <- function (vetor,sepa=''){
  tam <- max(nchar(vetor))
  vetor <- as.numeric(vetor)
  vetor <- sprintf(paste('%0',tam,'d',sep=''),vetor)
  multi <- c((tam+1):2)
  aux1 <- matrix(rep(NA,(length(vetor)*tam)),ncol=tam)
  for( i in 1:tam){
    aux1[,i] <- as.numeric(substr(vetor,i,i)) * multi[i]
  }
  dv <- rowSums(aux1)*10
  dv <- dv%%11
  dv[which(dv==10)] <- 0
  coddv <- paste(vetor,dv,sep=sepa)
  coddv
}


