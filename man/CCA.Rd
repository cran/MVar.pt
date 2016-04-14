\name{CCA}

\alias{CCA}

\title{Analise de correcacao canonica (CCA).}

\description{Realiza analise de correlcao canonica (CCA) em um conjunto de dados.}

\usage{CCA(X = NULL, Y = NULL, Type = 1, Test = "Bartlett", Sign = 0.05)}

\arguments{
\item{X}{Primeiro grupo de variaveis de um conjunto dados.}
\item{Y}{Segundo grupo de variaveis de um conjunto dados.}
\item{Type}{1 para analise utilizando a matriz de covariancia (default),\cr
            2 para analise utilizando a matriz de correlacao.}
\item{Test}{Teste de significancia da relacao entre o grupo X e Y:\cr
            "Bartlett" (default) ou "Rao".}
\item{Sign}{Grau de significancia do teste (default 5\%).}           
}

\value{
\item{Cxx    }{Matriz de Covariancia ou Correlacao Cxx.}
\item{Cyy    }{Matriz de Covariancia ou Correlacao Cyy.}
\item{Cxy    }{Matriz de Covariancia ou Correlacao Cxy.}
\item{Cyx    }{Matriz de Covariancia ou Correlacao Cyx.}
\item{Var.UV }{Matriz com autovalores (variancias) dos pares cononicos U e V.}
\item{Corr.UV}{Matriz de Correlacao dos pares cononicos U e V.}
\item{Coef.X }{Matriz dos Coeficientes canonicos do grupo X.}
\item{Coef.Y }{Matriz dos Coeficientes canonicos do grupo Y.}
\item{Coor.X }{Matriz das Correlacoes entre as variaveis canonicas e as variaveis originais do grupo X.}
\item{Coor.Y }{Matriz das Correlacoes entre as variaveis canonicas e as variaveis originais do grupo Y.}
\item{Score.X}{Matriz com os scores do grupo X.}
\item{Score.Y}{Matriz com os scores do grupo Y.}
\item{SigTest}{Restorna o teste de significancia da relacao entre o grupo X e Y: "Bartlett" (default) ou "Rao".}
}

\references{
MINGOTI, S. A. \emph{Analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

RENCHER, A.C.; \emph{Methods of Multivariate Analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.

LATTIN, J., CARROL J. D., GREEN P.E.; \emph{Analise de dados multivariados.} 1th. ed. Sao Paulo: Cengage Learning, 2011. 455 p.

}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{Plot.CCA}}}

\examples{
data(DataMix) # conjunto de dados

Dat <- DataMix[,2:ncol(DataMix)]

rownames(Dat) <- DataMix[,1]

X <- as.data.frame(NormData(Dat[,1:2],2))

Y <- as.data.frame(NormData(Dat[,5:6],2))

Resp <- CCA(X, Y, Type = 1, Test = "Bartlett", Sign = 0.05)

print("Matriz com autovalores (variancias) dos pares cononicos U e V:");
round(Resp$Var.UV,3)

print("Matriz de Correlacao dos pares cononicos U e V:"); round(Resp$Corr.UV,3)

print("Matriz dos Coeficientes canonicos do grupo X:"); round(Resp$Coef.X,3)

print("Matriz dos Coeficientes canonicos do grupo Y:"); round(Resp$Coef.Y,3)

print("Matriz das Correlacoes entre as variaveis canonicas
       e as variaveis originais do grupo X:"); round(Resp$Coor.X,3)

print("Matriz das Correlacoes entre as variaveis canonicas
       e as variaveis originais do grupo Y:"); round(Resp$Coor.Y,3)

print("Matriz com os scores do grupo X:"); round(Resp$Score.X,3)

print("Matriz com os scores do grupo Y:"); round(Resp$Score.Y,3)

print("Testes de significancia dos pares canonicos:"); Resp$SigTest
}

\keyword{Analise de correlacao canonica}
\keyword{CCA}
