\name{Data_Individuos}

\alias{Data_Individuos}

\docType{data}

\title{Conjunto de Dados de Frequencia}

\description{Conjunto de Dados Categorizados por Individuos, sobre habilidades sensoriais no consumo de cafes especiais.}

\usage{data(Data_Individuos)}

\format{Conjunto de dados de uma pesquisa feita com o proposito de avaliar a concordancia entre as respostas de diferentes grupos de consumidores com diferentes habilidades sensoriais. O experimento relaciona a analise sensorial de cafes especiais definidos por (A) Bourbon Amarelo, cultivado a altitudes maiores do que 1200 m.; (D) idem a (A) diferenciando apenas no preparo das amostras; (B) Acaia cultivados a altitude menores do que 1.100m; (C)  idem ao (B) porem diferenciando o preparo das amostras. Aqui os dados estao categorizados por individuos. O exemplo dado demonstra os resultados encontrados em OSSANI et al. (2014).}

\references{
OSSANI, P. C. et al.; Multiplos fatores em analise de tabela de contingencia: Uma aplicacao na analise sensorial da qualidade de cafes especiais. in: 59 REUNIAO ANUAL DA REGIAO BRASILEIRA DA SOCIEDADE INTERNACIONAL DE BIOMETRIA (RBRAS), 59., 2014, Ouro Preto. \emph{Anais}...Ouro Preto: UFOP, 2014.

OSSANI, P. C. \emph{Qualidade de cafes especiais e nao especiais por meio da analise de multiplos fatores para tabelas de contingencias.} 2015. 107 p. Dissertacao (Mestrado em Estatistica e Experimentacao Agropecuaria) - Universidade Federal de Lavras, Lavras, 2015.}

\examples{
data(Data_Individuos) # conjundo de dados categorizados

Matriz = Data_Individuos[,2:ncol(Data_Individuos)] 

rownames(Matriz) <- as.character(t(Data_Individuos[1:nrow(Data_Individuos),1]))

GroupNames = c("Grupo 1","Grupo 2","Grupo 3","Grupo 4")

MF <- MFA(Matriz,c(16,16,16,16),c(rep("f",4)),GroupNames) # analise dos dados

print("Variancias dos Componentes Principais:")
print(round(MF$MatrixA,2))

print("Matriz das Inercias Parciais/Escores das Variaveis:")
print(round(MF$MatrixEscVar,2))

Titulos=c("Individuos","Individuos/Grupos Individuos","Inercias Grupos")

Plot.MFA(MF,Titulos,2,"n","s","n") # Imprime varios graficos da tela
}

\keyword{Conjunto de Dados}