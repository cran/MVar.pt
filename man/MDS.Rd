\name{MDS}

\alias{MDS}

\title{Escalonamento multidimensional (MDS).}

\description{Realiza o escalonamento multidimensional (MDS) em um conjunto de dados.}

\usage{MDS(Data, Distance = "euclidean", Axis = TRUE, Title = NA,
    xlabel = NA, ylabel = NA, Color = TRUE, LinLab = NA)}

\arguments{
\item{Data}{Dados a serem analizados.}
\item{Distance}{Metrica das distancias: "euclidean" (default), "maximum", "manhattan", "canberra", "binary" ou "minkowski".}
\item{Color}{Graficos coloridos (default = TRUE).}
\item{Axis}{Coloca eixos nos graficos (default = TRUE).}
\item{Title}{Titulo do grafico, se nao for definido assume texto padrao.}
\item{xlabel}{Nomeia o eixo X, se nao for definido assume texto padrao.}
\item{ylabel}{Nomeia o eixo Y, se nao for definido assume texto padrao.}

\item{LinLab}{Vetor com os rotulos para as observacoes, se nao for definido assume texto padrao.}
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
data(DataQuan) # conjunto de dados quantitativos

Dat <- DataQuan[,2:8]

rownames(Dat) <- DataQuan[1:nrow(DataQuan),1]

MD <- MDS(Dat, Distance = "euclidean", Axis = TRUE, Title = NA,
          xlabel = NA, ylabel = NA, Color = TRUE, LinLab = NA)

print("Matriz das distancias:"); MD$MatrixD
}

\keyword{Escalonamento Multidimensional}
\keyword{MDS}
