#' Percentual de respondentes e media de desempenho para uma questao
#'
#' Retorna um \code{data.frame} com o percentual de respostas e media de 
#'  desempenho por categoria da questao tabulada.
#'
#' @param base Conjunto de dados contendo o desempenho do estudante e as variaveis
#' a serem tabuladas no questionario.
#' @param disc Vetor com o mnemonico para cada uma das disciplinas avaliadas.
#' @param quest Nome da variaval (coluna) no conjunto de dados a ser tabulda.
#' @param ncat Numero de categorias da variavel a ser tabulada.
#' @param prefixprofi Prefixo para as variaveis de proficiencia.
#' @param quantil Quantis que serao computados, Default: \code{c(.25,;5,.75)}.
#' @param pesopond Vetor de pesos para expansao da amostra. 
#' @param ndec Numero de casas decimais na saida da funcao.
#' @param branco Indicador do tipo de branco, Default: \code{" "}. Alternativamente,
#' pode-se utilizar \code{""}.
#' @param invalido Indicador do tipo de invalido, Default: \code{"*"}.
#' 
#' @return Um objeto do tipo data.frame contendo a tabulacao dos dados.
#' 
#' @examples
#' data("quest_dados")
#' data("quest_perguntas")
#' data("quest_categorias")
#' data("quest_numcategorias")
#' 
#' i=1
#' QuestProfi(base = quest_dados,
#'            disc = c('por','mat','nat','hum'),
#'            quest = paste('p',sprintf("%02d",1:20),sep='')[i],
#'            ncat = quest_numcategorias[i],
#'            prefixprofi = 'profi_',
#'            quantil=c(.25,.5,.75),
#'            pesopond=NULL,
#'            ndec=2,
#'            branco=' ',
#'            invalido='*')
#'


QuestProfi <- function(base,disc,quest,ncat,prefixprofi,quantil=c(.25,.5,.75),pesopond,ndec,branco=' ',invalido='*'){

  ndisc = length(disc)
  
  if(is.null(pesopond)){
    peso = rep(1,nrow(base))
  }else{
    peso = pesopond
  }

  base[,quest][is.na(base[,quest])] <- "NA"
  base[,quest] <- factor(base[,quest],levels=c(LETTERS[1:ncat],branco,invalido,"NA"))
  
  colprof = paste(prefixprofi,disc,sep='')
  
  
  g1 <- function(y) Hmisc::wtd.mean(y[,1],y[,2])
  t1 <- t2 <-  list()
  for( i in 1:ndisc){
    t1[[i]] <- Hmisc::summarize(cbind(base[,colprof[i]],peso), list(var=base[,quest]), g1,stat.name = paste('med',disc[i],sep=''))
    t2[[i]] <- Hmisc::summarize(base[,colprof[i]], list(var=base[,quest]), Hmisc::wtd.quantile, weights = peso, probs=quantil, na.rm=F,
                         stat.name=paste('q',quantil*100,disc[i],sep=''))
  }
  t3 <- aggregate(list(expansao=peso),list(var=addNA(base[,quest])),sum)
  t3$percexpansao <- t3$expansao/sum(t3$expansao)*100
  t3$N=aggregate(list(N=peso),list(var=addNA(base[,quest])),length)[,-1]
  t3$percN <- t3$N/sum(t3$N)*100
  
  if(is.null(pesopond)) t3 <- t3[,-c(2,3)]
  
  t1 <- do.call(cbind,t1)[,c('var',paste('med',disc,sep=''))]
  t2 <- do.call(cbind,t2)[,c('var',paste('q',rep(quantil*100,ndisc),rep(disc,rep(length(quantil),ndisc)),sep=''))]
  
  tabela = cbind(t3,t1[,-1],t2[,-1])
  tabela[,-1] = round(tabela[,-1],ndec)
  tabela
}


