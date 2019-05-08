\name{MDS}

\alias{MDS}

\title{Escalonamento multidimensional (MDS).}

\description{Realiza o escalonamento multidimensional (MDS) em um conjunto de dados.}

\usage{MDS(Data, Distance = "euclidean", Title = NA, xlabel = NA,  
    ylabel = NA, PosLeg = 2, BoxLeg = TRUE, Axis = TRUE, 
    Color = TRUE, LinLab = NA, Class = NA)}

\arguments{
  \item{Data}{Dados a serem analizados.}
  \item{Distance}{Metrica das distancias: "euclidean" (default), "maximum", "manhattan", "canberra", "binary" ou "minkowski".}
  \item{Title}{Titulo do grafico, se nao for definido assume texto padrao.}
  \item{xlabel}{Nomeia o eixo X, se nao for definido assume texto padrao.}
  \item{ylabel}{Nomeia o eixo Y, se nao for definido assume texto padrao.}
  \item{PosLeg}{0 sem legenda,\cr
                1 para legenda no canto superior esquerdo,\cr
                2 para legenda no canto superior direito (default),\cr
                3 para legenda no canto inferior direito,\cr
                4 para legenda no canto inferior esquerdo.}
  \item{BoxLeg}{Coloca moldura na legenda (default = TRUE).}
  \item{Axis}{Coloca eixos nos graficos (default = TRUE).}
  \item{Color}{Graficos coloridos (default = TRUE).}
  \item{LinLab}{Vetor com os rotulos para as observacoes.}
  \item{Class}{Vetor com os nomes das classes dos dados.}
}

\value{{Grafico de escalonamento multidimensional.}
\item{MatrixD}{Matriz das distancias.}
}

\references{
MINGOTI, S. A. \emph{Analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

RENCHER, A. C. \emph{Methods of Multivariate Analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data(iris) # conjunto de dados

Data <- iris[,1:4]

cls <- iris[,5] # classe dos dados

MD <- MDS(Data, Distance = "euclidean", Title = NA, xlabel = NA,  
          ylabel = NA, PosLeg = 2, BoxLeg = TRUE, Axis = TRUE, 
          Color = TRUE, LinLab = NA, Class = cls)

print("Matriz das distancias:"); MD$MatrixD
}

\keyword{Escalonamento Multidimensional}
\keyword{MDS}
