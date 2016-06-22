\name{FA}

\alias{FA}

\title{Analise Fatorial (FA).}

\description{Realiza analise fatorial (FA) em um conjunto de dados.}

\usage{FA(Data, Method = "PC", Type = 2, NFactor = 1,
          Rotation = "None", ScoresObs = "Bartlett", 
          Screeplot = TRUE, Converg = 1e-5, 
          Iteracao = 1000, TestFit = TRUE) 
}

\arguments{
\item{Data}{Dados a serem analisados.}
\item{Method}{Tipo de analises:\cr
              "PC" - Componentes Principais (default),\cr
              "PF" - Fator Principal,\cr
              "ML" - Maxima Verossimilhanca.}
\item{Type}{1 para analise utilizando a matriz de covariancia,\cr
            2 para analise utilizando a matriz de correlacao (default).}
\item{Rotation}{Tipo de rotacao: "None" (default) e "Varimax".}
\item{NFactor}{Numero de fatores (default = 1).}
\item{ScoresObs}{Tipo de scores para as observacoes: "Bartlett" (default) ou "Regression".}
\item{Screeplot}{Gera o grafico screeplot para as variancias dos fatores (defaut = TRUE), somente para Rotation = "None".}
\item{Converg}{Valor limite para convergencia para soma do quadrado dos residuos para metodo de Maxima Verossimilhanca (default = 1e-5).}
\item{Iteracao}{Numero maximo de iteracoes para metodo de Maxima Verossimilhanca (default = 1000).}
\item{TestFit}{Testa o ajusto do modelo para o metodo de Maxima Verossimilhanca (default = TRUE).}
}

\value{
\item{MatrixMC}{Matriz de Correlacao/Covariancia.}
\item{MatrixAutoVlr}{Matriz de autovalores.}
\item{MatrixAutoVec}{Matriz de autovetores.}
\item{MatrixVar}{Matriz de variancias e proporcoes.}
\item{MatrixCarga}{Matriz de cargas fatoriais.}
\item{MatrixVarEsp}{Matriz das variancias especificas.}
\item{MatrixComuna}{Matriz das comunalidades.}
\item{MatrixResiduo}{Matriz dos residuos.}
\item{VlrSQRS}{Valor limite superior para a soma do quadrados dos residuos.}
\item{VlrSQR}{Soma dos quadrados dos residuos.}
\item{MatrixResult}{Matriz com todos os resultados associados.}
\item{MatrixScores}{Matriz com os escores das observarcoes.}
}

\references{
MINGOTI, S. A. \emph{Analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

Kaiser, H. F..\emph{The varimax criterion for analytic rotation in factor analysis.} Psychometrika 23, 187-200, 1958.

RENCHER, A.C.; \emph{Methods of Multivariate Analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data(DataQuan) # conjunto de dados

Dat <- DataQuan[,2:ncol(DataQuan)]

rownames(Dat) <- DataQuan[,1]

Resp <- FA(Dat, Method = "PC", Type = 2, NFactor = 3)

print("Matriz com todos os resultados associados"); round(Resp$MatrixResult,3)

print("Soma dos Quadrados dos Residuos"); round(Resp$VlrSQR,3)

print("Matriz das Cargas Fatoriais:"); round(Resp$MatrixCarga,3)

print("Matriz das Cargas Fatoriais das Observacoes:"); round(Resp$MatrixScores,3)

}

\keyword{Analise Fatorial}
\keyword{FA}
