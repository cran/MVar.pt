\name{CCA}

\alias{CCA}

\title{Analise de correlacao canonica (CCA).}

\description{Realiza analise de correlacao canonica (CCA) em um conjunto de dados.}

\usage{CCA(X = NULL, Y = NULL, type = 1, test = "Bartlett", sign = 0.05)}

\arguments{
  \item{X}{Primeiro grupo de variaveis de um conjunto de dados.}
  \item{Y}{Segundo grupo de variaveis de um conjunto de dados.}
  \item{type}{1 para analise utilizando a matriz de covariancia (default),\cr
              2 para analise utilizando a matriz de correlacao.}
  \item{test}{teste de significancia da relacao entre o grupo X e Y:\cr
              "Bartlett" (default) ou "Rao".}
  \item{sign}{Grau de significancia do teste (default 5\%).}           
}

\value{
  \item{Cxx    }{Matriz de Covariancia ou Correlacao Cxx.}
  \item{Cyy    }{Matriz de Covariancia ou Correlacao Cyy.}
  \item{Cxy    }{Matriz de Covariancia ou Correlacao Cxy.}
  \item{Cyx    }{Matriz de Covariancia ou Correlacao Cyx.}
  \item{var.UV }{Matriz com autovalores (variancias) dos pares cononicos U e V.}
  \item{corr.UV}{Matriz de Correlacao dos pares cononicos U e V.}
  \item{coef.X }{Matriz dos Coeficientes canonicos do grupo X.}
  \item{coef.Y }{Matriz dos Coeficientes canonicos do grupo Y.}
  \item{corr.X }{Matriz das Correlacoes entre as variaveis canonicas e as variaveis originais do grupo X.}
  \item{corr.Y }{Matriz das Correlacoes entre as variaveis canonicas e as variaveis originais do grupo Y.}
  \item{score.X}{Matriz com os scores do grupo X.}
  \item{score.Y}{Matriz com os scores do grupo Y.}
  \item{sigtest}{Retorna o teste de significancia da relacao entre o grupo X e Y: "Bartlett" (default) ou "Rao".}
}

\references{
Mingoti, S. A. \emph{Analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

Ferreira, D. F. \emph{Estatistica Multivariada.} 2a ed. revisada e ampliada. Lavras: Editora UFLA, 2011. 676 p.

Rencher, A. C. \emph{Methods of multivariate analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.

Lattin, J.; Carrol, J. D.; Green, P. E. \emph{Analise de dados multivariados.} 1th. ed. Sao Paulo: Cengage Learning, 2011. 455 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{Plot.CCA}}}

\examples{
data(DataMix) # conjunto de dados

data <- DataMix[,2:ncol(DataMix)]

rownames(data) <- DataMix[,1]

X <- data[,1:2]

Y <- data[,5:6]

res <- CCA(X, Y, type = 2, test = "Bartlett", sign = 0.05)

print("Matriz com autovalores (variancias) dos pares cononicos U e V:");
round(res$var.UV,3)

print("Matriz de correlacao dos pares cononicos U e V:"); round(res$corr.UV,3)

print("Matriz dos coeficientes canonicos do grupo X:"); round(res$coef.X,3)

print("Matriz dos coeficientes canonicos do grupo Y:"); round(res$coef.Y,3)

print("Matriz das correlacoes entre as variaveis canonicas
       e as variaveis originais do grupo X:"); round(res$corr.X,3)

print("Matriz das correlacoes entre as variaveis canonicas
       e as variaveis originais do grupo Y:"); round(res$corr.Y,3)

print("Matriz com os scores do grupo X:"); round(res$score.X,3)

print("Matriz com os scores do grupo Y:"); round(res$score.Y,3)

print("teste de significancia dos pares canonicos:"); res$sigtest
}

\keyword{Analise de correlacao canonica}
\keyword{CCA}
