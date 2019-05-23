CTT1 <- function (data, ids, vars, peso = FALSE, nitems, nforms, nitemform, 
                  nblform, nbl, tbl, gab, resp_possible, items, ndec = 2, 
                  acer = "1", erro = "0", napres = "9", mostra_napres = 0, 
                  calc_normit = FALSE) 
{
  if (sum(is.element(colnames(data), ids)) != length(ids)) 
    stop("Alguma das colunas de ids não existem nos dados")
  if (sum(is.element(colnames(data), vars)) != length(vars)) 
    stop("Alguma das colunas de vars não existem nos dados")
  if (sum(nchar(data[, vars[2]]) == 0) > 0) 
    stop("sum(nchar(responses)==0)>0 \n Um ou mais elementos de responses tem tamanho 0")
  if (length(unique(data[, vars[1]])) > nforms) 
    stop("length(unique(vecform)) > nforms \n Existem mais tipos de cadernos para os alunos (vecform) do que cadernos informados (form)")
  if (peso) 
    peso = data[, vars[3]]
  if (length(peso) == 1) {
    if (!peso) 
      peso = rep(1, nrow(data))
  }
  nalu <- nrow(data)
  if (mostra_napres > 0) {
    resp_possible <- c(resp_possible, napres)
  }
  data <- data[, c(ids, vars)]
  colnames(data)[which(names(data) == vars[1])] <- "caderno"
  colnames(data)[which(names(data) == vars[2])] <- "respostas"
  data[1, ]
  nalt <- length(resp_possible)
  CTTest <- list()
  nCTTest <- 3 * nalt + 5 + 2 + 1 + 1
  CTTest[[1]] <- data.frame(matrix(NA, ncol = nCTTest, nrow = nitems))
  colnames(CTTest[[1]]) <- c("IT", "GAB", "DIFI", "DISCR", "ABAI", "ACIM", "BISE", "PBISE", 
                             paste(rep(c("Perc", "Bise", "PBise"), c(nalt, nalt, nalt)), rep(resp_possible, 3), sep = ""),
                             "QTD_RESP")
  CTTest[[1]]$IT <- 1:nitems
  CTTest[[1]]$GAB <- gab
  
  Gabarito <- data.frame(caderno = 1:nrow(items), 
                         gabarito = apply(matrix(gab[items], nrow = nrow(items)), 1, paste0, collapse = ""), 
                         stringsAsFactors = F)
  if(nrow(data) < 500){
    temp <- escore_r(respostas = data[, c("caderno", "respostas")], 
           gabarito = Gabarito, NumCad = nforms, NumItens = nitemform, 
           CodAcer = acer, CodErro = erro, CodNaoAp = napres, 
           nblform = nblform, tbl = tbl)
  }else{
    temp <- Escore(respostas = data[, c("caderno", "respostas")], 
                   gabarito = Gabarito, NumCad = nforms, NumItens = nitemform, 
                   CodAcer = acer, CodErro = erro, CodNaoAp = napres, 
                   nblform = nblform, tbl = tbl)
  }
  
  CTTest[[2]] <- cbind(data[, c(ids, "caderno", "respostas")], temp)
  rm(temp
     )
  itemposNum <- ItemPos(items, nitems, tipo = "integer")
  if (calc_normit) {
    (CTTest[[2]]$normit <- normit(scores = CTTest[[2]]$nacer, 
                                  caderno = CTTest[[2]]$caderno, nitemform = nitemform, 
                                  peso = peso))
  }
  writeLines("\n Calculando as estatisticas classicas...")
  for (i in 1:nitems) {
    setTxtProgressBar(txtProgressBar(min = 1, max = nitems, style = 3, width = 70, initial = 1), i)
    auxcols <- paste(rep(c("Perc", "PBise"), c(nalt, nalt)), rep(resp_possible, 2), sep = "")
    
    CTTest[[1]][i, auxcols] <- unlist(PontoBisserial(respostas = data[,c("caderno", "respostas")], 
                                                     scores = CTTest[[2]][, ifelse(calc_normit, "normit", "pacer")], 
                                                     itempos = itemposNum[[i]], 
                                                     resposta_possivel = resp_possible, 
                                                     CodNaoAp = napres, 
                                                     Peso = peso,
                                                     mostra_napres = mostra_napres))
    
    (tp <- CTTest[[1]][i, paste(rep("Perc", nalt), resp_possible, sep = "")])
    (tb <- CTTest[[1]][i, paste(rep("PBise", nalt), resp_possible, sep = "")])
    (CTTest[[1]][i, paste(rep("Bise", nalt), resp_possible, sep = "")] <- tb * sqrt(tp * (1 - tp))/dnorm(qnorm(as.numeric(tp))))
    
    CTTest[[1]][i, c("DIFI", "BISE", "PBISE")] <- if(gab[i] != "X"){ 
    CTTest[[1]][i, paste(c("Perc", "Bise", "PBise"), gab[i], sep = "")]
    }else{
        rep(NA, 3)
      }
    
    dummyitem <- RespItem(respostas = data[, c("caderno", "respostas")], itempos = itemposNum[[i]])
      
    if (mostra_napres == 0) {
      
      scoreitem <- CTTest[[2]][, ifelse(calc_normit, "normit", "pacer")][dummyitem != napres & dummyitem != ""]
      
      pesomodi <- peso[dummyitem != napres & dummyitem != ""]
      
      dummyitem <- dummyitem[dummyitem != napres & dummyitem != ""]
      
      aux <- dummyitem == gab[i]
    }
    
    if (mostra_napres > 0) {
      scoreitem <- CTTest[[2]][, ifelse(calc_normit, "normit", "pacer")][dummyitem != ""]
      
      pesomodi <- peso[dummyitem != ""]
      
      dummyitem <- dummyitem[dummyitem != ""]
      
      aux <- dummyitem == gab[i]
    }
    
    
    dummyitem[aux] <- 1
    dummyitem[!aux] <- 0
    dummyitem <- as.integer(dummyitem)
    
    ginf <- scoreitem <= Hmisc::wtd.quantile(x = scoreitem, weights = pesomodi, probs = 0.27)
    
    gsup <- scoreitem >= Hmisc::wtd.quantile(x = scoreitem, weights = pesomodi, probs = 1 - 0.27)
    
    CTTest[[1]][i, "QTD_RESP"] <- length(dummyitem)
    
    CTTest[[1]][i, "ABAI"] <- sum(dummyitem[ginf] * pesomodi[ginf])/sum(pesomodi[ginf])
    
    CTTest[[1]][i, "ACIM"] <- sum(dummyitem[gsup] * pesomodi[gsup])/sum(pesomodi[gsup])
    
    CTTest[[1]][i, "DISCR"] <- CTTest[[1]][i, "ACIM"] - CTTest[[1]][i, "ABAI"]
    
  }
  cols <- c("DIFI", "DISCR", "ABAI", "ACIM", "BISE", "PBISE", 
            paste(rep(c("Perc", "Bise", "PBise"), c(nalt, nalt, nalt)), 
                  rep(resp_possible, 3), sep = ""))
  
  CTTest[[1]][, cols] <- round(CTTest[[1]][, cols], ndec)

  CTTest[[1]]$GAB <- gab
  
  ordemcols <- c("IT", "GAB", cols, 'QTD_RESP')
  
  CTTest[[1]] <- CTTest[[1]][, ordemcols]
  
  aux_bisepos <- CTTest[[1]][rowSums(CTTest[[1]][, paste("Bise", resp_possible[is.letter(resp_possible)], sep = "")] > 0, na.rm = T) > 1, ]
  aux_bisepeq <- CTTest[[1]][CTTest[[1]][, "BISE"] <= 0.15 & !is.na(CTTest[[1]][, "BISE"]), ]
  aux_probbise <- unique(rbind(aux_bisepos, aux_bisepeq))
  
  
  if (nrow(aux_probbise) > 0) {
    aux_probbise <- aux_probbise[order(aux_probbise$IT), ]
    
    rownames(aux_probbise) <- aux_probbise$IT
    
    aux_probbise$prob <- ""
    
    aux_probbise[as.character(intersect(aux_bisepos$IT, aux_bisepos$IT)), "prob"] <- "2 Bis > 0 & Bis < .15"
    aux_probbise[as.character(setdiff(aux_bisepos$IT, aux_bisepeq$IT)), "prob"] <- "2 Bis > 0"
    aux_probbise[as.character(setdiff(aux_bisepeq$IT, aux_bisepos$IT)), "prob"] <- "Bis < .15"
  }
  CTTest[[3]] <- aux_probbise
  
  names(CTTest) <- c("Classical Stats (using score)", "Scores, Block result and vector of corret pattern", 
                     "Classical Stats of Items with problems")
  
  CTTest
}



# ctt = 'cttEnccejaCH_EM'
# gabpar = gabparEncc17CH_EM
# colsgabpar = colnames(gabparEncc17CH_EM)[1:6]exporta_dir = dirClass
# 
