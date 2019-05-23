read.bisblg <- function (caminho, grp, dif = FALSE) 
{
  
  if(!missing(grp) & dif) warning("Nao eh possivel calcular *dif* quando *grp* estah definido")
  
  PH1_input <- readChar(con = caminho, nchars = file.info(caminho)$size)
  PH1_inp <- stringr::str_match(PH1_input, "\\n\\s*>INP\\w*\\s*[\\S\\s]*?;")
  PH1_ngrp <- as.numeric(stringr::str_match(PH1_inp, "NGRO\\w*\\s*=\\s*?(\\d+)")[,2])
  Resultados <- list()
  for (grupo in 1:PH1_ngrp) {
    PH1_grpn <- stringr::str_match(PH1_input, paste0("ITEM STATISTICS FOR GROUP:\\s*", 
                                                     grupo, "\\s*([\\S]*)[\\s\\S]*?[-]+([\\S\\s]*?)--"))[, 2]
    PH1_grp1 <- stringr::str_match(PH1_input, paste0("ITEM STATISTICS FOR GROUP:\\s*", 
                                                     grupo, "\\s*([\\S]*)[\\s\\S]*?[-]+([\\S\\s]*?)--"))[, 3]
    PH1_grp1 <- read.fwf(textConnection(PH1_grp1), widths = c(5, 
                                                              11, 9, 10, 8, 9, 10, 9), skip = 1, strip.white = T, 
                         colClasses = c("numeric", "character", rep("numeric", 
                                                                    6)), col.names = c("itemblg", "nomeblg", "ntried", 
                                                                                       "nacer", "pacer", "logit", "pearson", "bisblg"))
    PH1_grp1 <- PH1_grp1[-nrow(PH1_grp1), ]
    Resultados[[grupo]] <- PH1_grp1
    names(Resultados)[[grupo]] <- PH1_grpn
  }
  if ( missing(grp) & dif == TRUE ) {
    
    
    bisblg <- Resultados
    
    for(i in 1:length(bisblg))
    {
      bisblg[[i]]$grupo <- i
      bisblg[[i]]$nomegrupo <- names(bisblg)[i]
    }
    aux <- do.call(rbind,bisblg)
    taux <- data.frame(table(aux$nomeblg))
    taux <- as.character(taux[taux[,2] > 1,'Var1'])
    rownames(aux) <- 1:nrow(aux)
    compara <- aux[is.element(aux$nomeblg,taux),]
    compara <- split(compara, compara$nomeblg)
    rm(aux,taux,i)
    
    ### gerando todas as possiveis combinacoes 2 a 2 dos itens no banco.
    difpct <- list(); indicador_difpct = 1; indicador_item = 0 ; while(indicador_item < length(compara))
    {
      print(paste0('Item: ',indicador_item))
      indicador_item <- indicador_item + 1
      
      aux_compara <- compara[[indicador_item]]
      
      if(nrow(aux_compara) == 2)
      {
        difpct[[indicador_difpct]] <- aux_compara
        indicador_difpct <- indicador_difpct + 1 
      }else{
        auxlin <- nrow(aux_compara)
        combinacoes <- combn(1:nrow(aux_compara),2)
        for(i in 1:ncol(combinacoes))
        {
          aux_comb <- combinacoes[,i]
          difpct[[indicador_difpct]] <- rbind(aux_compara[aux_comb[1],],aux_compara[aux_comb[2],])
          indicador_difpct <- indicador_difpct + 1 
        }
      }
    }
    rm(indicador_item,indicador_difpct,i)
    rm(auxlin,aux_comb,aux_compara,combinacoes)
    rm(compara)
    
    difpct <- cbind(do.call(rbind,lapply(difpct, '[', 1, c(1:2,5,8,9))),
                    do.call(rbind,lapply(difpct, '[', 2, c(5,8,9))))
    
    colnames(difpct) <- c('itemblg', 'nomeblg', paste0('gp', rep(1:2, c(3,3)), rep(c("_pacer","_bisblg",""),2)))
    
    difpct$difpct <- difpct$gp2_pacer - difpct$gp1_pacer
    
    difpct <- difpct[order(difpct$itemblg,difpct$gp1,difpct$gp2),]
    
    Resultados[[grupo+1]] <- difpct
    
    return(Resultados)
  }else{
    if ( missing(grp) & dif == FALSE ) {
      return(Resultados)
    }else{
      if (grp > length(Resultados)) {
        stop("Grupo nao existente")
      }
      else {
        return(Resultados[[grp]])
      }
    }
  }
}
