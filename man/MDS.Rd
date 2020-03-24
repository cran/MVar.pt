\name{MDS}

\alias{MDS}

\title{Escalonamento multidimensional (MDS).}

\description{Realiza o escalonamento multidimensional (MDS) em um conjunto de dados.}

\usage{MDS(data, distance = "euclidean", title = NA, xlabel = NA,  
    ylabel = NA, posleg = 2, boxleg = TRUE, axes = TRUE, 
    size = 1.1, grid = TRUE, color = TRUE, linlab = NA, 
    class = NA, classcolor = NA)}

\arguments{
  \item{data}{Dados a serem analizados.}
  \item{distance}{Metrica das distancias: "euclidean" (default), "maximum", "manhattan", "canberra", "binary" ou "minkowski".}
  \item{title}{Titulo do grafico, se nao for definido assume texto padrao.}
  \item{xlabel}{Nomeia o eixo X, se nao for definido assume texto padrao.}
  \item{ylabel}{Nomeia o eixo Y, se nao for definido assume texto padrao.}
  \item{posleg}{0 sem legenda,\cr
                1 para legenda no canto superior esquerdo,\cr
                2 para legenda no canto superior direito (default),\cr
                3 para legenda no canto inferior direito,\cr
                4 para legenda no canto inferior esquerdo.}
  \item{boxleg}{Coloca moldura na legenda (default = TRUE).}
  \item{axes}{Coloca eixos nos graficos (default = TRUE).}
  \item{size}{Tamanho dos pontos no grafico .}
  \item{grid}{Coloca grade nos graficos (default = TRUE).}
  \item{color}{Graficos coloridos (default = TRUE).}
  \item{linlab}{Vetor com os rotulos para as observacoes.}
  \item{class}{Vetor com os nomes das classes dos dados.}
  \item{classcolor}{Vetor com as cores das classes.}
}

\value{{Grafico de escalonamento multidimensional.}
\item{mtxD}{Matriz das distancias.}
}

\references{
MINGOTI, S. A. \emph{Analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

RENCHER, A. C. \emph{Methods of multivariate analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data(iris) # conjunto de dados

data <- iris[,1:4]

cls <- iris[,5] # classe dos dados

MD <- MDS(data = data, distance = "euclidean", title = NA, xlabel = NA,  
          ylabel = NA, posleg = 2, boxleg = TRUE, axes = TRUE, color = TRUE,  
          linlab = NA, class = cls, classcolor = c("goldenrod3","gray53","red"))

print("Matriz das distancias:"); MD$mtxD
}

\keyword{Escalonamento Multidimensional}
\keyword{MDS}
