

#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <numeric>



using namespace Rcpp;
// respostas = dataframe com coluna de caderno e respostas
// gabarito = dataframe com coluna de caderno e respostas
// numcad = numero de cadernos (forms) diferentes 

// [[Rcpp::export]]
CharacterVector RespItem(DataFrame respostas, NumericMatrix itempos) {
  
  // acessando as colunas do dataframe
  std::vector<std::string> vecPadrao = respostas["respostas"];        // acessando a coluna com os padroes
  std::vector<int>         vecCaderno = respostas["caderno"];    // acessando a coluna de caderno
  int                      ncaditem = itempos.nrow();            // numero de cadernos em que o item (itematual) aparece
  int                      qtdRespondentes = vecPadrao.size();   // numero de respondentes a prova    
  std::vector<std::string> respitem(qtdRespondentes);            // criando vetor com as respostas ao item 
  
  for(int j = 0; j < ncaditem; ++j)
  {
    int auxcad = itempos(j,0);
    int auxpos = itempos(j,1)-1;
    
    //Rcout << "auxcad: " << auxcad << " auxpos: " << auxpos << "\n";
    for(long long int i = 0; i< qtdRespondentes; ++i)
    {
      if(vecCaderno[i] == auxcad)
      {
        respitem[i] =  vecPadrao[i].substr(auxpos,1);
      }
    }
  }
  
  return wrap(respitem);
}

/*** R

# 
# agd <- calcPBise_cpp(respostas = dadosAmo, gabarito = gabAmo, 
#                      scores = CTT_dadosTemp[[2]]$score, 
#                      #sdscore = sd(CTT_dadosTemp[[2]]$score),
#                      itempos = Fun_itemposNumeric(items15LC,50)[[1]][1:4,,drop=F],
#                      resposta_possivel = c(LETTERS[1:5],".","*"),
#                      itematual = 1,CodNaoAp="9")
# 
# CTT_dadosTemp[[1]][1,paste(rep(c("Perc","PBise"),c(7,7)),rep(c(LETTERS[1:5],".","*"),2),sep='')]
# round(agd[[1]],2)
# unlist(agd)
#agd <- GeraDummy(respostas = dadosAmo, gabarito = gabAmo, scores = acp$nacer, sdscore = sd(acp$nacer),
#                 itempos = Fun_itemposNumeric(items15LC,50)[[1]], resposta_possivel = c(LETTERS[1:5],".","*"),
#                 itematual = 1,CodNaoAp="9")
# x = RespItem(respostas = Respostas, itempos = itemposNum[[i]])
# table(x)
*/
  
  
