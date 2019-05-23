GeraGroup <- function(ordemitens, grpini, grpnome)
{
  # ordemitens <- gabparPT1602M[,c('seq','bl','ob','itemblg','aban')]
  # set.seed(1)
  # ordemitens$aban <- sample(0:1,78,replace = T, prob = c(.8,.2))
  # grpini = 5
  # grpnome = 'PT1602M'
  
  colnames(ordemitens) <- c('it','bl','ob','itemblg','aban')
  tbl <- unique(table(ordemitens$bl)) #tamanho do bloco
  nbl <- max(ordemitens$bl)           #numero total de blocos
  naban <- sum(ordemitens$aban)       #numero de itens abandonados
  nit <- tbl*nbl                      #numero de itens total no grupo
  L1 <- paste0(">GROUP",grpini+1," GNAME='",grpnome,"'  LENGTH= ",nit-naban, ",  INUMBERS=(")
  L2 <- matrix('', ncol = tbl, nrow = nbl)  
  
  ## criando matriz de itemblg
  for(j in 1:nbl)
  {
    L2[j,] <- paste0(sprintf("%03d",ordemitens[ordemitens$bl == j, 'itemblg']),", ")
  }
  L2[j,tbl] <- gsub(',',');',L2[j,tbl])  
  
  # colocando espacos em branco quando item é abandonado
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
  write.table(rbind(c(L1,rep('',tbl-1)),L2),quote = F,row.names = F,col.names = F, sep='')
}

