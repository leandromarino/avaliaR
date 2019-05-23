---
title: "Como usar o `avaliaR`"
output: 
  md_document: 
    variant: gfm
vignette: >
  %\VignetteIndexEntry{como_usar_o_avaliaR}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



## Carregando o pacote
Para carregar o pacote basta utilizar o comando:

```r
library(avaliaR)
```


## Preparando as análises

Para a preparação dos dados precisamos conhecer a estrutura da prova. As 
funcionalidades atuais permitem que, por exemplo, os cadernos tenham tamanhos 
distintos. Ou seja, não necessariamente o caderno 1 tem que ter a mesma quantidade
e os mesmos tipos de itens do que o caderno 2. Em algumas avaliações, como por exemplo
no PISA, os cadernos possuem quantidade de itens diferentes.

Para fins de ilustração, vamos considerar uma avaliação que segue um planejamento
em blocos de tal forma que temos 3 blocos independente de itens cada qual com 12 itens.
Então, nesse processo avaliativo temos, ao todo, 36 itens. Esses blocos são distribuídos
em 3 cadernos seguindo a seguinte disposição:


| Sequêncial | Caderno | Parte.1  | Parte.2  | Parte.3  |
|:----------:|:-------:|:--------:|:--------:|:--------:|
|     1      |    A    | Bloco: 1 | Bloco: 2 | Bloco: 3 |
|     2      |    B    | Bloco: 2 | Bloco: 3 | Bloco: 1 |
|     3      |    C    | Bloco: 3 | Bloco: 1 | Bloco: 2 |

Essa é uma avaliação em que todos os cadernos possuem exatamente a mesma 
quantidade de itens. Assim, a disposição dos itens pelos cadernos de provas 
fica como se segue:


| Seq. | Cad. | P1 | P2 | P3 | P4 | P5 | P6 | P7 | P8 | P9 | P10 | P11 | P12 | P13 | P14 | P15 | P16 | P17 | P18 | P19 | P20 | P21 | P22 | P23 | P24 | P25 | P26 | P27 | P28 | P29 | P30 | P31 | P32 | P33 | P34 | P35 | P36 |
|:----:|:----:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
|  1   |  A   | 1  | 2  | 3  | 4  | 5  | 6  | 7  | 8  | 9  | 10  | 11  | 12  | 13  | 14  | 15  | 16  | 17  | 18  | 19  | 20  | 21  | 22  | 23  | 24  | 25  | 26  | 27  | 28  | 29  | 30  | 31  | 32  | 33  | 34  | 35  | 36  |
|  2   |  B   | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22  | 23  | 24  | 25  | 26  | 27  | 28  | 29  | 30  | 31  | 32  | 33  | 34  | 35  | 36  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  | 10  | 11  | 12  |
|  3   |  C   | 25 | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34  | 35  | 36  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  | 10  | 11  | 12  | 13  | 14  | 15  | 16  | 17  | 18  | 19  | 20  | 21  | 22  | 23  | 24  |

### Estrutura da Prova

Antes de iniciarmos os procedimentos de análises devemos, primeiramente, 
informar como se dá a distribuição de itens nos cadernos de prova. Para 
isso, iremos utilizar a função `gera_itens_caderno`.  Esta função
permite a entrada de cadernos de tamanho diferentes. Cada caderno deve 
ser um vetor com o sequencial dos itens que o compõem. 


```r
itcaderno <- gera_itens_caderno(1:36, c(13:36,1:12), c(25:36,1:24))
itcaderno
#> $cad001
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
#> [24] 24 25 26 27 28 29 30 31 32 33 34 35 36
#> 
#> $cad002
#>  [1] 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35
#> [24] 36  1  2  3  4  5  6  7  8  9 10 11 12
#> 
#> $cad003
#>  [1] 25 26 27 28 29 30 31 32 33 34 35 36  1  2  3  4  5  6  7  8  9 10 11
#> [24] 12 13 14 15 16 17 18 19 20 21 22 23 24
#> 
#> attr(,"class")
#> [1] "itens_caderno" "list"
```

A partir da criação do objeto `itcaderno` através da função `gera_itens_caderno`
podemos dar sequência na criação da estrutura da prova propriamente dita. Vamos
utilizar o objeto `gabpar05P` que contem informações sobre o item como 
it (sequencial único do item), o bloco (bl), a ordem no bloco (ob), 
código do item (codigo), gabarito (gab), sequencial do item no BilogMg (itemblg),
nome do item no BilogMg (nomeblg) e, parâmetros estimados do item (a, b, c).
O objeto `gabpar05P` é referente à uma prova de Língua Portuguesa.


```r
data("gabpar05P")
```


| it | bl | ob |codigo   | gab | itemblg|nomeblg  |       a|        b|       c|
|:--:|:--:|:--:|:--------|:---:|-------:|:--------|-------:|--------:|-------:|
| 1  | 1  | 1  |ET37VCGK |  A  |     289|P1405161 | 1.76333| -2.28170| 0.24426|
| 2  | 1  | 2  |EG3HDY2A |  C  |     181|P1405027 | 1.62790| -0.84509| 0.24234|
| 3  | 1  | 3  |S3R9J4DX |  A  |     283|P1405155 | 1.61388| -0.72356| 0.16392|
| 4  | 1  | 4  |2CQCA6BR |  A  |     224|P1405080 | 1.94336| -0.88177| 0.22066|
| 5  | 1  | 5  |7PEL7XMD |  D  |     161|P1405001 | 1.10930| -1.81344| 0.19610|
| 6  | 1  | 6  |3CEAPD2C |  D  |     162|P1405002 | 0.63156|  0.85766| 0.10988|
| 7  | 1  | 7  |YGFKP7P2 |  C  |     242|P1405102 | 0.48664|  0.61341| 0.20962|
| 8  | 1  | 8  |EJJWJ5UK |  A  |     176|P1405020 | 1.24909| -0.41949| 0.25287|
| 9  | 1  | 9  |6HLMGZCG |  B  |     206|P1405058 | 0.88217| -1.23513| 0.31195|
| 10 | 1  | 10 |EZ4UWZ9D |  A  |     180|P1405025 | 1.45490| -1.25887| 0.31770|

A função `estrutura_prova` faz uso de alguns argumentos, todos **obrigatórios**.
A seguir, uma descrição de cada um dos parâmetros usados na função.

* `itens_caderno` deve ser o objeto resultante da função `gera_itens_caderno`.
* `coditem` vetor de caracteres com os códigos dos itens.
* `respostas` lista das possíveis respostas em cada um dos itens. Para uma questão 
de múltipla escolha com cinco alternativas e com possibilidade de resposta inválida ('*'),
e em branco ('.'), o elemento da lista deveria ser: `c(LETTER[1:5],'*','.')`.
* `tipo_item` um vetor onde cada elemento deve indicar o tipo de item `'dicotomico'` ou 
`'politomico'`. O tamanho desse vetor deve ser a quantidade de itens da prova.
* `pontuacao` lista com a pontuação que deve ser dada para cada item. Essa pontuação 
será utilizada para o cálculo do escore de cada participante. 
  + Para item dicotômico: só precisa ter duas pontuações: uma para o acerto e 
  outra para o erro. Ex: `c(0, 1)` ou `c(1,0)`. A ordem não importa.
  + Para item politômico: É necessário que se declare a pontuação na ordem do 
  objeto `respostas` e todos os elementos para o item em `respostas` devem possuir 
  pontuação. Por exemplo, para uma situação em que A e B são a pontuação máxima (2),
  C (1) e D(0). Se elemento da lista de `respostas` desse item for 
  `c('A', 'B', 'C', 'D')`, então, o elemento de `pontuacao` para esse mesmo item
  será: `c(2, 2, 1, 0)`. A ordem nesse caso importa então se fosse: 
  `c('D', 'C', 'B', 'A')` entao deveria ser `c(0, 1, 2, 2)`.
* `gabarito` vetor do tamanho da quantidade de itens onde para cada item dicotômico
deve existir um gabarito definido. No caso de item politômico, o elemento do vetor 
deve ser definido como `NA`.
* `nit` número de itens na prova. Usado para checar a consistência das informações
anteriormente fornecidas.

Assim, para o caso da prova de Língua Portuguesa de 36 itens a função 
`estrutura_prova` deve ser utilizada como apresentado:


```r
str_prova <- estrutura_prova(
                itens_caderno = itcaderno,
                coditem = gabpar05P$codigo,
                respostas = rep(list(c(LETTERS[1:5], '*', ' ')), 36),
                tipo_item = rep('dicotomico', 36),
                pontuacao = rep(list(c(0,1)), 36),
                gabarito = gabpar05P$gab,
                nit = 36)
```


O resultado é um objeto da classe `estrutura_prova` que tem seu próprio método 
de impressão. 


```r
print(str_prova, len = 1)
#> ***------------------------------------------------------------*** 
#> ***------------------------------------------------------------*** 
#> ***------------------------------------------------------------*** 
#>  Item:  001   |  Codigo:   ET37VCGK 
#>  
#>  Tipo:  dicotomico 
#>  Gabarito:  A 
#> 
#>  Esquema de pontuacao: 
#>    Acerto: 1 
#>    Erro  : 0
#> 
#> 
#> 
#>  Exibidos apenas os 1 primeiros itens!
```

O objeto `str_prova` gerado é, na realidade, uma lista com dois 
elementos `itens_caderno` e `info_item` que é uma lista com `nit = 36`
itens. Dentro de `info_item` temos uma lista com os elementos:

* `coditem` vetor com 1 posição contendo o código do item 
* `tipo_item` vetor com 1 posição contendo o tipo de item (`'dicotomico'` ou `'politomico'`)
* `gabarito` data.frame contendo o número de linhas igual ao vetor de respostas possíveis e as 
colunas de pontuação e codificação.
* `posicao` data.frame com as colunas `cad` e `pos`.  O número de linhas igual à 
quantidade de cadernos em que o item aparece. `cad` identifica o sequencial do caderno
e `pos` a posição do item no caderno de provas.



```r
str_prova$info_item$it001
#> $coditem
#> [1] "ET37VCGK"
#> 
#> $tipo_item
#> [1] "dicotomico"
#> 
#> $gabarito
#>   respostas pontuacao codificacao
#> 1         A         1           1
#> 2         B         0           0
#> 3         C         0           0
#> 4         D         0           0
#> 5         E         0           0
#> 6         *         0           0
#> 7                   0           0
#> 
#> $posicao
#>        cad pos
#> cad001   1   1
#> cad002   2  25
#> cad003   3  13
```

## Obtendo os Escores

Para o cálculo dos escores dos participantes, utilizaremos a função `escore`.
Agora, precisaremos dos dados das respostas dos alunos. Junto ao pacote existe
um objeto `quest_dados` este é um objeto com contem resposta à prova e questionário, 
bem como proficiências e informações sócio-econômicas de alunos em
avaliações de Língua Portuguesa, Matemática, Ciências da Natureza e Ciências
Humanas.


```r
data("quest_dados")
dados05P <- quest_dados[!is.na(quest_dados$cad_por),
                        c('codesc', 'turma', 'id', 'cad_por', 'rsp_por', 'profi_por')]
```


|codesc |turma |id   | cad_por|rsp_por                              | profi_por|
|:------|:-----|:----|-------:|:------------------------------------|---------:|
|16     |A     |0001 |       3|DDAACBAADDBDACCADACABAAACCAADDADACAB |     183.8|
|16     |A     |0002 |       3|ADAACADABDDDACAADBCDBAAABBDDCADABBAB |     229.9|
|16     |B     |0003 |       2|BBDDCDDABDAAADAACADABADBACACDBCABAAA |     240.0|
|16     |A     |0004 |       3|ADAACADABADBACAADBCABAACBADDCDDACCAD |     320.2|
|16     |A     |0005 |       2|BADDCDDBBCAAADDACADABADBACDADBAABAAB |     240.1|
|16     |A     |0006 |       3|ADAACADABADBACAADBCABAADBADDCDDACCCD |     285.4|

A função `escore` necessida dos parâmetros:
* `respostas` vetor de caracteres com as respostas dos alunos em cada um dos itens da prova.
* `caderno` vetor com o número do caderno que o aluno respondeu
* `estrutura_prova` objeto resultante da função `estrutura_prova`.
* `nao_apres` caracter de tamanho 1 que contém o código para o item não apresentado, ex.: `'9'`.


```r
escore05P <- avaliaR::escore(respostas = dados05P$rsp_por,
    caderno  = dados05P$cad_por,
    estrutura_prova = str_prova,
    nao_apres = '9')
```



|respostas                            |padrao                               | ntot| nnao_apres| ntried| score| pacer|
|:------------------------------------|:------------------------------------|----:|----------:|------:|-----:|-----:|
|ACCADACABAAACCAADDADACABDDAACBAADDBD |110110111110000001000110011110010000 |   36|          0|     36|    17|  47.2|
|ACAADBCDBAAABBDDCADABBABADAACADABDDD |111110101110101110110010111111111010 |   36|          0|     36|    26|  72.2|
|ACACDBCABAAABBDDCDDABDAAADAACADABADB |111010111110101111110010111111111111 |   36|          0|     36|    29|  80.6|
|ACAADBCABAACBADDCDDACCADADAACADABADB |111110111111111111111111111111111111 |   36|          0|     36|    35|  97.2|
|ACDADBAABAABBADDCDDBBCAAADDACADABADB |110110011110111111100110110111111111 |   36|          0|     36|    28|  77.8|
|ACAADBCABAADBADDCDDACCCDADAACADABADB |111110111110111111111101111111111111 |   36|          0|     36|    33|  91.7|

