
GeraForm <- function(ordemitens,bib, formini)
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
    write.table(rbind(c(L1,rep('',tbl-1)),L2),quote = F,row.names = F,col.names = F, sep='')
  }
}
