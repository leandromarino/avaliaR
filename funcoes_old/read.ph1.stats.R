#file = paste0(dirNova18_Diag,'Blg/NovaEscola18d.PH1')

read.ph1_stats <- function(file){
  
  ph1 <- readLines(con = file) 
  
  div_hifen <- substr(ph1, 1, 74) == paste0(c(' ',rep('-',73)), collapse = '')
  div_hifen <- which(div_hifen,1:length(ph1))
  div_hifen <- matrix(div_hifen, ncol = 2, byrow = T)
  
  ph1.stats <- list()
  for(i in 1:nrow(div_hifen)){
    ph1.stats[[i]] <- ph1[ (div_hifen[i,1]+1) : (div_hifen[i,2]-1) ]  
    ph1.stats[[i]] <- read.fwf(file = textConnection(as.character(ph1.stats[[i]])), 
                               widths = c(05, 11, 09, 10, 08, 09, 10, 09), 
                               col.names = c('itemblg','nomeblg','ntried','right','pct','logit','corr','bise'),
                               as.is = T)
    ph1.stats[[i]]$nomeblg <- trim(ph1.stats[[i]]$nomeblg)
  }
  
  ini <- substr(ph1, 1, 27) == ' ITEM STATISTICS FOR GROUP:' | substr(ph1, 1, 27) == ' ITEM STATISTICS FOR MULTIP'
  ini <- which(ini,1:length(ph1))
  nome_lista <- ph1[ini]
  nome_lista <- gsub('ITEM STATISTICS FOR MULTIPLE GROUPS','', nome_lista)
  nome_lista <- gsub('ITEM STATISTICS FOR GROUP:','',nome_lista)
  nome_lista <- trim(nome_lista)
  nome_lista <- trim(nome_lista)
  nome_lista <- trim(gsub(pattern = "^\\d{1,2}(.*)$", replacement = '\\1', nome_lista))
  nome_lista
  
  names(ph1.stats) <- nome_lista
  ph1.stats
}

# Store(read.ph1_stats, lib = funcoes, lib.loc = dirNovaEscola18)
