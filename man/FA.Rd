\name{FA}

\alias{FA}

\title{Analise fatorial (FA).}

\description{Realiza analise fatorial (FA) em um conjunto de dados.}

\usage{FA(Data, Method = "PC", Type = 2, NFactor = 1, Rotation = "None",
   ScoresObs = "Bartlett", Converg = 1e-5, Iteracao = 1000, 
   TestFit = TRUE) 
}

\arguments{
\item{Data}{Dados a serem analisados.}
\item{Method}{Metodo de analise:\cr
              "PC" - Componentes Principais (default),\cr
              "PF" - Fator Principal,\cr
              "ML" - Maxima Verossimilhanca.}
\item{Type}{1 para analise utilizando a matriz de covariancia,\cr
            2 para analise utilizando a matriz de correlacao (default).}
\item{Rotation}{Tipo de rotacao: "None" (default) e "Varimax".}
\item{NFactor}{Numero de fatores (default = 1).}
\item{ScoresObs}{Tipo de scores para as observacoes: "Bartlett" (default) ou "Regression".}
\item{Converg}{Valor limite para convergencia para soma do quadrado dos residuos para metodo de Maxima Verossimilhanca (default = 1e-5).}
\item{Iteracao}{Numero maximo de iteracoes para metodo de Maxima Verossimilhanca (default = 1000).}
\item{TestFit}{Testa o ajuste do modelo para o metodo de Maxima Verossimilhanca (default = TRUE).}
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
\item{VlrSQRS}{Valor limite superior para a soma dos quadrados dos residuos.}
\item{VlrSQR}{Soma dos quadrados dos residuos.}
\item{MatrixResult}{Matriz com todos os resultados associados.}
\item{MatrixScores}{Matriz com os escores das observacoes.}
\item{CoefScores}{Matriz com os escores dos coeficientes dos fatores.}
}

\references{
MINGOTI, S. A. \emph{Analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

Kaiser, H. F.\emph{The varimax criterion for analytic rotation in factor analysis.} Psychometrika 23, 187-200, 1958.

RENCHER, A. C. \emph{Methods of Multivariate Analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.

FERREIRA, D. F. \emph{Estatistica Multivariada.} 2a ed. revisada e ampliada. Lavras: Editora UFLA, 2011. 676 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{Plot.FA}}}

\examples{
data(DataQuan) # conjunto de dados

Data <- DataQuan[,2:ncol(DataQuan)]

rownames(Data) <- DataQuan[,1]

Resp <- FA(Data, Method = "PC", Type = 2, NFactor = 3, Rotation = "None",
           ScoresObs = "Bartlett", Converg = 1e-5, Iteracao = 1000, 
           TestFit = TRUE) 

print("Matriz com todos os resultados associados:"); round(Resp$MatrixResult,3)

print("Soma dos Quadrados dos Residuos:"); round(Resp$VlrSQR,3)

print("Matriz das Cargas Fatoriais:"); round(Resp$MatrixCarga,3)

print("Matriz com os escores das observacoes:"); round(Resp$MatrixScores,3)

print("Matriz com os escores dos coeficientes dos fatores:"); round(Resp$CoefScores,3)

}

\keyword{Analise Fatorial}
\keyword{FA}

