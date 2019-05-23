#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <numeric>


using namespace Rcpp;
// respostas = dataframe com coluna de caderno e respostas
// gabarito = dataframe com coluna de caderno e respostas
// numcad = numero de cadernos (forms) diferentes 

// [[Rcpp::export]]
List PontoBisserial(DataFrame respostas, 
                    std::vector<double> scores,
                    NumericMatrix itempos,
                    std::vector<std::string>   resposta_possivel,
                    std::string CodNaoAp,
                    std::vector<double> Peso,
                    int mostra_napres ) {
  
  // acessando as colunas do dataframe
  std::vector<std::string> vecResposta = respostas["respostas"];
  std::vector<int>         vecCaderno  = respostas["caderno"];
  
  int ncaditem                  = itempos.nrow();                         // numero de cadernos em que o item (itematual) aparece
  int nresppossible             = resposta_possivel.size();               // numero de respostas possiveis 
  int qtdRespondentes           = vecResposta.size();                     // numero de respondentes a prova    
  
  // Rcout << nresppossible << '\n';
  
  List dummiesalt(1);                              // lista para armazenar as dummies por alternativa
  // no primeiro elemento da lista serao armazenados as respostas ao item <<itematual>>
  // no segundo elemento serao armazenadas os scores;
  // nos demais elementos as dummies na ordem do vetor <<resposta_possivel>>
  
  // Computando o numero de respondentes para o item
  long long int qtdRespondentesItem = 0;
  for(long long int i = 0; i< qtdRespondentes; ++i)
  {
    for(int j = 0; j < ncaditem; ++j)
    {
      int auxcad = itempos(j,0);
      if(vecCaderno[i] == auxcad)
      {
        qtdRespondentesItem = qtdRespondentesItem + 1;
      }
    }
  }
  // Rcout << "RespItem: " << qtdRespondentesItem << '\n';
  
  // CRIANDO VETORES APENAS COM OS RESPONDENTES DO ITEM
  std::vector<double> scoresItem(qtdRespondentesItem);    // CRIANDO VETOR DE SCORES
  std::vector<double> PesoItem(qtdRespondentesItem);      // CRIANDO VETOR DE PESOS
  std::vector<std::string>  respItem(qtdRespondentesItem); // CRIANDO VETOR DE RESPOSTAS
  long long int contadoralu = 0; // 
  long long int qtdNaoApres=0;   // quantidade de alunos com item nao apresentado
  double somNaoApres = 0.0;
  double somaPeso = 0.0;
  
  // VARIANDO EM TODOS OS RESPONDENTES
  for(long long int i = 0; i< qtdRespondentes; ++i)
  {
    // VARIANDO EM CADA CADERNO
    for(int j = 0; j < ncaditem; ++j)
    {
      int auxcad = itempos(j,0); 
      int auxpos = itempos(j,1)-1;
      // SE O CADERNO FOR IGUAL AO CADERNO DO ITEM ANALISADO ENTAO FAÇA
      if(vecCaderno[i] == auxcad)
      {
        respItem[contadoralu] =  vecResposta[i].substr(auxpos,1); 
        scoresItem[contadoralu] = scores[i];
        PesoItem[contadoralu] = Peso[i];
        somaPeso = somaPeso + Peso[i];
        if(respItem[contadoralu] == CodNaoAp){
          qtdNaoApres = qtdNaoApres + 1;
          somNaoApres = somNaoApres + Peso[i];
        }
        contadoralu = contadoralu + 1;
      }
    }
  }
  
   // Rcout << "Nao Apres:" << qtdNaoApres << "\n";
   // Rcout << "Soma Peso Nao Apres: " << somNaoApres << "\n";
   // Rcout << "Soma Peso Item: " << somaPeso << '\n';
  long long int qtd = 0;
  if(mostra_napres == 0)
    {
    // Rcout << "dentro do if == 0 \n";
    qtd = qtdRespondentesItem - qtdNaoApres;
    }
  if(mostra_napres >  0)
    {
    // Rcout << "dentro do if != 0 \n";
    qtd = qtdRespondentesItem;
    }
  
    std::vector<double> scoresItemF(qtd);     // CRIANDO VETOR DE SCORESF
    std::vector<double> scoresPondItemF(qtd); // CRIANDO VETOR DE SCORESPONDF
    std::vector<double> PesoItemF(qtd);       // CRIANDO VETOR DE PESOSF
    std::vector<std::string> respItemF(qtd);  // CRIANDO VETOR DE RESPOSTASF

  double MedScoreAltEscolPond[nresppossible];
  double MedScoreAltOutraPond[nresppossible];
  double proporcaoPond[nresppossible];
  
  long long int contRespValido=0;   // contador de respondentes validos (ou seja, subtraindo os nao apresentados)
  
  double SomaScorefPond = 0.0;
  double SomaPesof = 0.0;
  double SomaScoref = 0.0;
  
  memset(MedScoreAltEscolPond,  0.0, sizeof(MedScoreAltEscolPond));
  memset(MedScoreAltOutraPond,  0.0, sizeof(MedScoreAltOutraPond));
  memset(proporcaoPond,  0.0, sizeof(proporcaoPond));
  
  // CRIANDO OS VETORES F (RESPOSTAS, SCORES E SCORESPONF SEM OS ITENS NAO APRESENTADOS)
  for(long long int i = 0; i < qtdRespondentesItem; ++i)
  {
    if(respItem[i] == CodNaoAp && mostra_napres == 0)
    {
    }
    else
    {
      PesoItemF[contRespValido] = PesoItem[i];
      scoresItemF[contRespValido]  = scoresItem[i];
      scoresPondItemF[contRespValido] = scoresItem[i]*PesoItem[i];
      SomaScorefPond = SomaScorefPond + scoresItem[i]*PesoItem[i];
      SomaScoref = SomaScoref + scoresItem[i];
      SomaPesof = SomaPesof + PesoItem[i];
      
      for(int k = 0; k < nresppossible; ++k)
      {
        if(respItem[i] == resposta_possivel[k])
        {
          proporcaoPond[k] = proporcaoPond[k] + PesoItemF[contRespValido];
          MedScoreAltEscolPond[k] = MedScoreAltEscolPond[k] + scoresItemF[contRespValido]*PesoItemF[contRespValido];
        }
        else
        {
          MedScoreAltOutraPond[k] = MedScoreAltOutraPond[k] + scoresItemF[contRespValido]*PesoItemF[contRespValido];
        }
      }
      contRespValido = contRespValido + 1;
    }
  }
  
  // Rcout << "Proporcao: " << proporcaoPond[0] << " + " << proporcaoPond[1] << " + " << proporcaoPond[2] << " + " << proporcaoPond[3] << " + " << proporcaoPond[4] << " + " << proporcaoPond[5] << " + " << proporcaoPond[6] << "\n";
  // Rcout << "Proporcao: " << MedScoreAltOutraPond[0] << " + " << MedScoreAltOutraPond[1] << " + " << MedScoreAltOutraPond[2] << " + " << MedScoreAltOutraPond[3] << " + " << MedScoreAltOutraPond[4] << " + " << MedScoreAltOutraPond[5] << " + " << MedScoreAltOutraPond[6] << "\n";
  // Rcout << "Proporcao: " << MedScoreAltEscolPond[0] << " + " << MedScoreAltEscolPond[1] << " + " << MedScoreAltEscolPond[2] << " + " << MedScoreAltEscolPond[3] << " + " << MedScoreAltEscolPond[4] << " + " << MedScoreAltEscolPond[5] << " + " << MedScoreAltEscolPond[6]  << "\n";
  
  // calculando o desvio 
  double MedPondScoref = 0.0;
  double SomaQuadDifePond = 0.0;
  double denomPond = 0.0;
  double variPond = 0.0;
  double sdScorePondf = 0.0;
  MedPondScoref = SomaScorefPond/SomaPesof;
  for(long long int i = 0; i < qtdRespondentesItem-qtdNaoApres; ++i)
  {
    SomaQuadDifePond = SomaQuadDifePond + PesoItemF[i]*pow(scoresItemF[i] - MedPondScoref,2.0);
  }
  denomPond = SomaPesof-1;
  variPond = SomaQuadDifePond/denomPond;
  sdScorePondf = sqrt(variPond);
  
  // Rcout << "Nume: " << SomaQuadDifePond << " Deno: " << denomPond << " Var: " << variPond << " SD: " <<sdScorePondf << '\n';
 
  std::vector<double> vecresult(nresppossible*2); // vetor de saida de coeficientes pontobisseriais e proporcao de escolha
  double dMedScoreAltEscolPond = 0.0;
  double dMedScoreAltOutraPond = 0.0;
  double dproporcaoPond = 0.0;
  double pbisePond = 0.0;
  
  for(int k = 0; k < nresppossible; ++k){
    if(proporcaoPond[k] > 0 )
    {
      dMedScoreAltEscolPond = ((double) MedScoreAltEscolPond[k])/((double) proporcaoPond[k]);
      dMedScoreAltOutraPond = ((double) MedScoreAltOutraPond[k])/((double) (SomaPesof - proporcaoPond[k]));
      dproporcaoPond = ((double) proporcaoPond[k])/((double) SomaPesof);
      pbisePond = (((dMedScoreAltEscolPond-dMedScoreAltOutraPond)/sdScorePondf) * sqrt(dproporcaoPond * (1-dproporcaoPond)));
      // Rcout << "Proporcao = 0; k = " << k << " proporcao: " << dproporcaoPond << " bise: " << pbisePond << "\n";
    }
    else
    {
      dMedScoreAltEscolPond=0;
      dMedScoreAltOutraPond=0;
      dproporcaoPond=0;
      pbisePond = NAN;
      // Rcout << "Proporcao = 0; k = " << k << " proporcao: " << dproporcaoPond << " bise: " << pbisePond << "\n";
      }
    vecresult[k] = dproporcaoPond;
    vecresult[1*nresppossible+k] = pbisePond;
  }

  dummiesalt[0] = vecresult;
  return dummiesalt;
  
}

/*** R

# PontoBisserial(respostas = Respostas,
#                    gabarito = Gabarito,
#                    scores = CTTest[[2]]$nacer,
#                    itempos = itemposNum[[itematual]],
#                                        resposta_possivel = resp_possible,
#                                        itematual = itematual,
#                                        CodNaoAp = napres,
#                                        Peso = rep(1,nrow(Respostas)))
# cols = c("PercA","PercB","PercC","PercD","Perc ","PBiseA","PBiseB","PBiseC","PBiseD","PBise ")
# i = i
# round(unlist(PontoBisserial(respostas = Respostas,
#                             scores = CTTest[[2]]$nacer,
#                             itempos = itemposNum[[i]],
#                                                 resposta_possivel = resp_possible,
#                                                 itematual = i,
#                                                 CodNaoAp = napres,
#                                                 Peso = peso,
#                                                 mostra_napres = 1)),2)
#CTTSaeb15MT052[[1]][i,cols]
  
*/



