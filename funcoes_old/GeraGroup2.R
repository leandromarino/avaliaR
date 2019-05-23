
GeraGroup2 <- function(ordemitens, grpini, grpnome)
{
  # ordemitens <- gabparPT1602M[,c('seq','bl','ob','itemblg','aban')]
  # set.seed(1)
  # ordemitens$aban <- sample(0:1,78,replace = T, prob = c(.8,.2))
  # grpini = 5
  # grpnome = 'PT16M02'
  
  colnames(ordemitens) <- c('it','bl','ob','itemblg','aban')
  tbl <- unique(table(ordemitens$bl)) #tamanho do bloco
  nbl <- max(ordemitens$bl)           #numero total de blocos
  naban <- sum(ordemitens$aban)       #numero de itens abandonados
  nit <- tbl*nbl                      #numero de itens total no grupo
  L1 <- paste0(">GROUP",grpini+1," GNAME='",grpnome,"', LENGTH= ",nit-naban, ",  INUMBERS=(")
  L2 <- matrix('', ncol = tbl, nrow = nbl)  
  
  ## criando matriz de itemblg
  for(j in 1:nbl)
  {
    L2[j,] <- paste0(sprintf("%03d",ordemitens[ordemitens$bl == j, 'itemblg']),", ")
  }
  L2[j,tbl] <- gsub(',',');',L2[j,tbl])  
  
  # colocando espacos em branco quando item ? abandonado
  if(naban > 0)
  {
    aux <- ordemitens[ordemitens$aban == 1,]
    for(i in 1:naban)
    {
      L2[aux$bl[i],aux$ob[i]] <- paste0(rep(' ',5),collapse = '')
    }
  }
  
  ### acertando matriz para quando o ultimo item eh abandonado
  if(ordemitens[nit,'aban']==1)
  {
    L2[j,tbl] <- '   );'
    
    # verificando ultimo item nao abandonado para remover a virgula ','
    cont <-  nit
    stop <- 0
    while(stop == 0)
    {
      if(ordemitens[cont,'aban'] == 1)
      {
        cont <- cont - 1
      }else{
        stop <-  1
        cont <-  cont
      }
    }
    L2[ordemitens$bl[cont],ordemitens$ob[cont]] <- gsub(",",' ',L2[ordemitens$bl[cont],ordemitens$ob[cont]])
  }
  
  if(tbl*5 > 79){
    if(tbl%%2 == 0){
      L2 <- matrix(as.vector(t(L2)), nrow = nbl*2, ncol = tbl/2, byrow = T)
    }else{
      NROW <- c((nbl+1):100)
      #p1 = verifica se o tamanho da prova ? divisivel por nrow
      p1 = (nbl*tbl) %% NROW == 0
      # p2 = verifica se o nrow testado pode gera um objeto de comprimento menor que 78 posicoes (BLM)
      p2 = ((nbl*tbl) / NROW) * 5 < 78
      aux  <- ( p1 + p2 )
      NROW <- NROW[aux==2][1]
      L2 <- matrix(as.vector(t(L2)), nrow = NROW, byrow = T)
    }
  }
  
  write.table(L1,quote = F,row.names = F,col.names = F, sep='')
  write.table(L2,quote = F,row.names = F,col.names = F, sep='')
}




