% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pontuacao.politomico.R
\name{pontuacao.politomico}
\alias{pontuacao.politomico}
\title{Funcao que gera \code{data.frame} de pontuacao para itens politômicos}
\usage{
pontuacao.politomico(respostas, pontuacao, seq_item = NULL,
  data = NULL)
}
\description{
Esta funcao tem por objetivo gerar um objeto do tipo list que sera utilizado
   dentro da funcao escore e tambem da funcao de TCT. Função apenas para a geração 
   pontuação de itens dicotômicos.
 
 @param respostas objeto do tipo \code{list} onde cada elemento é o conjunto
    de opções de respostas possíveis. \code{Ex.: c(LETTER[1:4], '*', 'c')}.
 @param gabarito objeto do tipo \code{vetor} indicando os gabaritos para os itens 
    dicotômicos. Em caso de item politômico declarar NULL. Exemplo: \code{c("A", NULL)}]
 @param pontuacao Se \code{NULL} então será considerada a pontuacao \code{0} para a 
    alternativa errada e \code{1} para a alternativa certa. Podem ser atribuídas pontuações
    diferentes para itens dicotômicos distintos. \code{list(c(0,1),c(-1,1),c(0,0.5), NULL} para um 
    caso de quatro itens onde o primeiro errado = 0 e acerto = 1; o segundo errado = -1 e
    acerto = 1; o terceiro errado = 0 e acerto = 0.5; o último item com pontuacao NA será tratado
    como 0 erro e 1 acerto.
 @param seq_item Indica o sequencial do item caso o item tenha um sequencial 
    diferente de \code{1:length(gabarito)} \code{Default: NULL}. Argumento opcional e
    somente declarado dentro da funcao \code{gera_gabarito}.
 @param data parâmetro opcional. Default: \code{NULL}. Caso esteja adicionando 
    novos itens à um gabarito já existente então o gabarito existente pode
    ser definido em \code{data = gabarito_existente}. Assim, os itens serão 
    acrescentados após o \code{gabarito_existente}.
}
