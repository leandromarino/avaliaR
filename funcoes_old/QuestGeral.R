#' Percentual de respondentes e media de desempenho para um conjunto de questoes
#'
#' Retorna uma lista com cada elemento do tipo \code{data.frame} com o percentual
#' de respostas e media de desempenho para um conjunto de questoes.
#'
#' @param base Conjunto de dados contendo o desempenho do estudante e as variaveis
#' a serem tabuladas no questionario.
#' @param prefixprofi Prefixo para as variaveis de proficiencia.
#' @param disc Vetor com o mnemonico para cada uma das disciplinas avaliadas.
#' @param vectitulo Vetor com as perguntas do questionario
#' @param vecquest Vetor com os nomes das variaveis (colunas) no conjunto de 
#' dados a ser tabulada.
#' @param vecncat Vetor com o numero das categorias das variaveis a serem tabuladas.
#' @param veccat Vetor com a descrição das categorias das variaveis a serem 
#' tabuladas.
#' @param quantil Quantis que serao computados, Default: \code{c(.25,;5,.75)}.
#' @param pesopond Vetor de pesos para expansao da amostra. Default: \code{NULL}
#' @param ndec Numero de casas decimais na saida da funcao.
#' @param branco Indicador do tipo de branco, Default: \code{" "}. Alternativamente,
#' pode-se utilizar \code{""}.
#' @param invalido Indicador do tipo de invalido, Default: \code{"*"}.
#' 
#' @return Uma lista com objetos do tipo data.frame contendo as tabulacoes
#'  dos dados.
#' 
#' @examples
#' data("quest_dados")
#' data("quest_perguntas")
#' data("quest_categorias")
#' data("quest_numcategorias")
#' 
#' tabula <- QuestGeral(base = quest_dados,
#'            prefixprofi = 'profi_',
#'            disc = c('por','mat','nat','hum'),
#'            vectitulo = quest_perguntas ,
#'            vecquest = paste('p',sprintf("%02d",1:20),sep=''),
#'            vecncat = quest_numcategorias,
#'            veccat = quest_categorias,
#'            quantil=c(.25,.5,.75),
#'            pesopond=NULL,
#'            ndec=2,
#'            bra=' ',
#'            inv='*')
#' 
#' tabula


QuestGeral <- function(base,prefixprofi,disc,vectitulo,vecquest,vecncat,veccat,quantil=c(.25,.5,.75),pesopond=NULL,ndec=2,bra=' ',inv='*'){
  ndisc = length(disc)
  questgeral <- list()
  for(i in 1:length(vecncat)){
    questgeral[[i]] <- QuestProfi(base,disc,quest=vecquest[i],ncat=vecncat[i],prefixprofi,quantil,pesopond,ndec,branco=bra,invalido=inv)
    aux <- factor(c(LETTERS[1:vecncat[i]],bra,inv),levels=c(LETTERS[1:vecncat[i]],bra,inv))
    questgeral[[i]] <- merge(aux,questgeral[[i]], all.x=T, by.x="x", by.y="var",sort=T)
    questgeral[[i]]$desc <- c(veccat[[i]],'Em branco','Inválido')
    numcol = ncol(questgeral[[i]])
    questgeral[[i]] <- cbind(questgeral[[i]][,1,drop=F],questgeral[[i]][,numcol,drop=F],questgeral[[i]][,-c(1,numcol)])
    cat("\r QuestGeral: Item ",i, "de ", length(vecncat))
  }
  names(questgeral) <- vectitulo
  questgeral
}
