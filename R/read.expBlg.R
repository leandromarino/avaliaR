# file = paste0(dirBLG_MT_EM,'ENCC18_MT_EM_V2.EXP')
# nomeblg = gabparEnccPT16MT_EM$nomeblg
# grupo = 6

read.expBlg <- function(file, nomeblg, grupo){
  xq <- read.xqBlg(file)
  rj <- read.rjBlg(file)
  
  if(sum(duplicated(nomeblg)) > 0){
    warning("ATENCAO: Existem itens duplicados. O EXP foi gerado corretamente.", immediate. = T)
  }
  
  # separando o exp do grupo
  exp <- rj[rj$grupo == grupo,]
  rownames(exp) <- exp$nomeblg
  
  
  expF <- exp[,-c(1:2)]
  expF <- expF[nomeblg[!duplicated(nomeblg)],]
  rownames(expF) <- nomeblg[!duplicated(nomeblg)]
  expF <- rbind(xq,expF)
  
  #chegando diferencas
  if(sum(expF[exp$nomeblg,] - exp[,-c(1:2)], na.rm = T) > 0){
    warning('Possivel BUG: Erro na geracao do arquivo. Conferir arquivo de saida com o arquivo EXP do BilogMG.')
  }
  
  expF
  
}

# file = paste0(dirBLG_MT_EM,'ENCC18_MT_EM_V2.EXP'); nomeblg = gabparEnccPT12MT_EM$nomeblg; grupo = 3

# read.expBlg(file = paste0(dirBLG_MT_EM,'ENCC18_MT_EM_V2.EXP'), nomeblg = gabparEnccPT16MT_EM$nomeblg, grupo = 6)

