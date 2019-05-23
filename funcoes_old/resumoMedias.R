#' Resumo de medias
#' 
#' @description 
#'    Funcao que calcula as medias por determinadas covariaveis hierarquicamente dispostas.
#'  Esta funcao calcula por exemplo eh capaz de calcular as medias totais, por escolas
#'  e turmas. 
#' 
#' @usage 
#'    resumoMedias(dados,turmas,colmedia,colsresu,disc,probs = c(0,.1,.25,.5,.75,.9,1))
#' 
#' @param dados \code{data.frame} contendo as informacoes a nivel dos alunos para 
#'    o calculo das medias.
#' @param turmas \code{data.frame} contendo um cadastro com o menor nivel para juncao.
#'    (Em dados escolares a turma eh o menor nivel de agregacao). Parametro nao obrigatorio.
#' @param colmedia Nome da coluna que contem a proficiencia em que a media serah calculada.
#' @param colsresu Vetor de caracteres com o nome das colunas das variaveis de \code{dados}
#'    em ordem decrescente. Ex: anoescolar,uf,municipio,escola,turma
#' @param disc Mnemonico com a disciplina (util para identificacao na saida do arquivo)
#' @param probs Argumento com os quantis desejados para a saida da funcao.
#' 
#' 

# dados = profiParc1604M
# turmas = baseturmasParc16[baseturmasParc16$anoescolar=='EF04',]
# colmedia = 'profict'
# colsresu = c('anoescolar','codesc','turma')
# probs = c(0,.1,.25,.5,.75,.9,1)
# disc = 'M'

resumoMedias <- function (dados, turmas, colmedia, colsresu, disc, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1)) 
{
  (nalu <- nrow(dados))
  (nvari <- length(colsresu))
  (colsresu <- rev(colsresu))
  lista <- as.list(dados[, colsresu])
  result <- list()
  
  for (i in 1:(nvari+1)) {
    print(paste("Gerando resultados por: ", c(colsresu,'total')[i], 
                sep = ""))
    
    if (i > 1) lista[[i - 1]] <- rep("*Total", nalu)
    
    result[[i]] <- aggregate(list(profi = dados[, colmedia]), 
                             lista, function(x) c(pres = length(x), med = mean(x), 
                                                  sd = sd(x), q = quantile(x, probs)))
    result[[i]] <- as.data.frame(as.list(result[[i]]))
    colnames(result[[i]]) <- c(colsresu, paste(c("pres", 
                                                 "med", "sd"), sep = ""), paste("q", as.integer(probs * 
                                                                                                  100), sep = ""))
  }
  temp <- do.call(rbind, result)
  
  if(!missing(turmas))
  {
    result <- merge(turmas, temp, by = rev(colsresu), all = T)
    pfalta <- (result$qtdalu - result$pres)/result$qtdalu * 100
    result <- cbind(result, pfalta)
    ordemcols <- c(colnames(turmas), "pres", "pfalta", colnames(temp)[-c(1:(nvari + 
                                                                              1))])
    result <- result[, ordemcols]
    colnames(result) <- c(colnames(turmas), paste(c("pres", "pfalta", 
                                                    colnames(temp)[-c(1:(nvari + 1))]), disc, sep = ""))
  }else{
    result <- temp
  }
  
  result
}

# 
# resumoMedias(dados = profiParc1604M,
#              turmas = baseturmasParc16[baseturmasParc16$anoescolar=='EF04',],
#              colmedia = 'profict',
#              colsresu = c('anoescolar','codesc','turma'),
#              disc = 'M')
# 
