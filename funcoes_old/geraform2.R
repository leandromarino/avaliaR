

GeraForm2 <- function(ordemitens, bib, formini)
{
  # ordemitens <- gabparPT1602M[,c('seq','bl','ob','itemblg')]
  # bib = bibpt1602M
  # formini = 40
  colnames(ordemitens) <- c('it','bl','ob','itemblg')
  tbl <- unique(table(ordemitens$bl)) #tamanho do bloco
  tfo <- tbl*ncol(bib)                #tamando do caderno/form
  nblfo <- ncol(bib)                  #numero de blocos no caderno/form
  for(i in 1:nrow(bib))
  {
    (form = formini + i)
    L1 <- paste0(">FORM", sprintf("%03d",form), "  LENGTH= ",tfo, ",  INUMBERS=(")
    L2 <- matrix('', ncol = tbl, nrow = nblfo)
    for(j in 1:nblfo)
    {
      L2[j,] <- paste0(sprintf("%03d",ordemitens[ordemitens$bl == bib[i,j], 'itemblg']),", ")
    }
    L2[j,tbl] <- gsub(',',');',L2[j,tbl])
    
    if(tbl*5 > 79){
      if(tbl%%2 == 0){
        L2 <- matrix(as.vector(t(L2)), nrow = nblfo*2, ncol = tbl/2, byrow = T)
      }else{
        NROW <- c((nblfo+1):30)
        p1 = (nblfo*tbl) %% NROW == 0
        p2 = ((nblfo*tbl) / NROW) * 5 < 78
        aux  <- ( p1 + p2 )
        NROW <- NROW[aux==2][1]
        L2 <- matrix(as.vector(t(L2)), nrow = NROW, byrow = T)
      }
    }
    write.table(L1, quote = F,row.names = F,col.names = F, sep='')
    write.table(L2, quote = F,row.names = F,col.names = F, sep='')
  }
}



