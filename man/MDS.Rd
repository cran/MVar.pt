\name{MDS}

\alias{MDS}

\title{Escalonamento multidimensional (MDS).}

\description{Realiza o escalonamento multidimensional (MDS) em um conjunto de dados.}

\usage{MDS(Data, Distance = "euclidean", Eixos = TRUE, Title = NA,
    xlabel = NA, ylabel = NA, Color = TRUE, LinLab = NA)}

\arguments{
\item{Data}{Dados a serem a analizados.}
\item{Distance}{Metrica das distancias: "euclidean" (default), "maximum", "manhattan", "canberra", "binary" ou "minkowski".}
\item{Color}{Graficos coloridos (default = TRUE).}
\item{Eixos}{Coloca eixos no grafico (default = TRUE).}
\item{Title}{Titulo do grafico, se nulo retorna padrao.}
\item{xlabel}{Nomeia o eixo X, se nulo retorna padrao.}
\item{ylabel}{Nomeia o eixo Y, se nulo retorna padrao.}

\item{LinLab}{Vetor com o rotulo para as linhas, se nao informado retorna padrao.}
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

MD <- MDS(Dat, Distance = "euclidean")

print("Matriz das distancias:"); MD$MatrixD
}

\keyword{Escalonamento Multidimensional}
\keyword{MDS}
