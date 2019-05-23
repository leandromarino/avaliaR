
muda_versao_pacote <- function(muda_versao, muda_subversao, muda_subsubversao){
  description <- readLines(con = 'DESCRIPTION')

  versao <- description[grep('Version:', description)]

  versao <- strsplit(versao, "\\s|[.]") %>% unlist %>% `.`[-1] %>% as.integer

  if(muda_versao      ) versao <- versao + c(1,0,0)
  if(muda_subversao   ) versao <- versao + c(0,1,0)
  if(muda_subsubversao) versao <- versao + c(0,0,1)

  description[grep('Version:', description)] <- paste0('Version: ',
                                                       paste0(versao, collapse = '.'))

  writeLines(text = description, con = 'DESCRIPTION')
}


