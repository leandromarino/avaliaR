
#include <Rcpp.h>
#include <limits>
#include <cmath>


using namespace Rcpp;
// respostas = dataframe com coluna de caderno e respostas
// gabarito = dataframe com coluna de caderno e respostas
// numcad = numero de cadernos (forms) diferentes 

// [[Rcpp::export]]
DataFrame Escore(DataFrame respostas,
                 DataFrame gabarito,
                 int NumCad,
                 int NumItens,
                 std::string CodAcer,
                 std::string CodErro,
                 std::string CodNaoAp,
                 int nblform, 
                 int tbl) {
  // acessando as colunas do dataframe
  std::vector<std::string> vecResposta = respostas["respostas"];
  std::vector<int>         vecCaderno  = respostas["caderno"];
  
  // acessando as colunas do dataframe
  std::vector<std::string> vecGabGabarito = gabarito["gabarito"];
  std::vector<int>         vecGabCaderno  = gabarito["caderno"];
  
  
  // criando o inteiro tamanho
  long long int qtdRespondentes = vecResposta.size();
  //Rcout << qtdRespondentes;
  // Para preencher o vetor de srtrings vecResp é preciso utilizar a funcao push.back...
  // o for está variando para cada uma das linhas do dataframe
  // lembrar que no C++ os loops e tudo começa no 0. entáo o numero de elementos é sempre ali iniciado
  
  std::vector<std::string>  Padroes(qtdRespondentes);
  std::vector<int>          numAcertosProva(qtdRespondentes);
  std::vector<int>          numItensResolvidos(qtdRespondentes);
  NumericVector             percAcerto(qtdRespondentes);
  DataFrame                 PontuaBlocoResult;
  
  Rcout << "Criando padroes de respostas e scores individuais \n";
  for(long long int i = 0; i < qtdRespondentes; i++)
  {
    if(i%(qtdRespondentes/50)==0){Rcout << "=";};
    if(i==qtdRespondentes-1){Rcout << "100%\n";}
    
    // criando vetor com a resposta
    std::ostringstream strPadrao;
    double nacer = 0;
    int nNaoApres = 0;
    
    for(int jItens = 0; jItens < NumItens; jItens++)
    {  
      // fazendo o loop para cada um dos cadernos existentes
      
      std::string resp="A";
      std::string strTempA = vecResposta[i].substr(jItens,1);
      std::string strTempB = vecGabGabarito[vecCaderno[i]-1].substr(jItens,1);
      
      if(strTempA == CodNaoAp)
      {
        resp = CodNaoAp;
        nNaoApres = nNaoApres + 1;
      }
      else{
        if(strTempA==strTempB)
        {
          resp = CodAcer;
          nacer+=1;
        }
        else
        {
          resp = CodErro;
        }
      }
      
      strPadrao << resp;
      
    }
    Padroes[i] = strPadrao.str();
    numAcertosProva[i] = nacer;
    numItensResolvidos[i] = (NumItens - nNaoApres);
    percAcerto[i] = nacer/(NumItens - nNaoApres)*100;
  }
  
 
  
  for(int inbl = 0;inbl < nblform;inbl++)
  {
   // Rcout << "Bloco: " << inbl << "\n";
    // calculando pontos nos blocos 
    // Para funcinoar no corrige prova é só copiar e colocar a partir daqui.
    NumericVector nAcerBloco(qtdRespondentes);
    
    Rcout << "Calculando o numero de acertos para o bloco: " << inbl+1 <<  "\n";
    
    for(long long int i = 0; i < qtdRespondentes; i++)
    {
      if(i%(qtdRespondentes/50)==0){Rcout << "=";};
      if(i==qtdRespondentes-1){Rcout << "100%\n";}
      int nacerbloco=0;
    //  Rcout << "nacerbloco \n";
      for(int itbl = 0;itbl < tbl; itbl++)
      {
        std::string aux = Padroes[i].substr((tbl*(inbl) + itbl),1);
      //  Rcout << "pos: "<<(tbl*(inbl) + itbl) << "- " << aux << " - ";
        if(aux==CodAcer)
        {
          ++nacerbloco;
    //      Rcout << "nacerbloco: " << nacerbloco << "\n";
        }
        else{
        //  Rcout << "nacerbloco: " << nacerbloco << "\n";
        }
      }
      nAcerBloco[i] = nacerbloco;
      // Rcout << nAcerBloco << "\n";
    }
    
    PontuaBlocoResult.push_back(nAcerBloco);
  }
  
 
  List returned_frame = clone(PontuaBlocoResult);

  StringVector col_names(returned_frame.length());
  for (int j = 0; j < returned_frame.length(); ++j) {
    char name[6];
   // Rcout << name;
    sprintf(&(name[0]), "nbl%02d", j+1);
    col_names(j) = name;
    
  }
  returned_frame.attr("names") = col_names;
  returned_frame.attr("class") = "data.frame";

    return DataFrame::create(
     Named("padrao") = Padroes,
    Named("nacer") = numAcertosProva,
    Named("ntried") = numItensResolvidos,
    Named("pacer") = percAcerto,
    returned_frame,
    _["stringsAsFactors"] = false);

}

// /*** R
// # ##set.seed(1)
// # # 
// # temp = apply(matrix(LETTERS[sample(1:5,30*150,replace=T)],nrow=30),2,paste0,collapse="")
// #   substr(temp,1,5)[1:10] <- "99999"
// # dados <- data.frame(caderno=rep(c(1:2),75),
// #                     respostas=temp,
// #                     stringsAsFactors = F)
// #   gabar <- data.frame(caderno=1:2,
// #                       gabarito=c(paste0(rep("A",30),collapse=""),paste0(rep("B",30),collapse="")),
// #                       stringsAsFactors = F)
// #   dados[1:5,]
// # #apply(do.call(rbind,CorrigeProva(dados,gabar,2,30)),1,paste0,collapse="")
// # a = calcScore_cpp(dados,gabar,2,30,CodAcer="2",CodErro="1",CodNaoAp="9",nbl=3,tbl=10)
// # # #a
// # str(a)
// # 
// # b = CorrigeProva(dados,gabar,2,30,CodAcer="2",CodErro="1",CodNaoAp="9",nbl=1,tbl=30)
// # str(b)
// 
// # 
// # nalu=540000
// # temp = apply(matrix(LETTERS[sample(1:5,30*nalu,replace=T)],nrow=30),2,paste0,collapse="")
// #   substr(temp,1,5)[1:10] <- "99999"
// # dadost <- data.frame(caderno=rep(c(1:2),nalu/2),
// #                      respostas=temp,
// #                      stringsAsFactors = F)
// #   gabart <- data.frame(caderno=1:2,
// #                        gabarito=c(paste0(rep("A",30),collapse=""),paste0(rep("B",30),collapse="")),
// #                        stringsAsFactors = F)
// #   dados[1:5,]
// # #apply(do.call(rbind,CorrigeProva(dados,gabar,2,30)),1,paste0,collapse="")
// # ab = CorrigeProva(dadost,gabart,2,30,CodAcer="2",CodErro="1",CodNaoAp="9",nbl=3,tbl=10)
// # #a
// #   str(ab)
// #   rm(ab)
//   
//   */
// 
