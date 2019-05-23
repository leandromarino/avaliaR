compila_pacote <- function(muda_versao, muda_subversao, muda_subsubversao){

  #fazer funcao para incrementar versão do pacote.
  muda_versao_pacote(muda_versao, muda_subversao, muda_subsubversao)
  devtools::document(roclets = c('rd', 'vignette'))
  github_vignettes_to_docs()
  devtools::build()
  devtools::build(binary = TRUE, args = c('--preclean'))

}

# compila_pacote(muda_versao = TRUE,
#                muda_subversao = TRUE,
#                muda_subsubversao = TRUE)

