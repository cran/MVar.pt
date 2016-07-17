\name{MDS}

\alias{MDS}

\title{Escalonamento multidimensional (MDS).}

\description{Realiza o escalonamento multidimensional (MDS) em um conjunto de dados.}

\usage{MDS(Data, Distance = "euclidean", Eixos = "S", LabelX = NULL,
    LabelY = NULL, Title = NULL, Color = "S", LinLab = NULL)}

\arguments{
\item{Data}{Dados a serem a analizados.}
\item{Distance}{Metrica das distancias: "euclidean" (default), "maximum", "manhattan", "canberra", "binary" ou "minkowski".}
\item{Color}{"s" para grafico colorido (default),\cr
             "n" para grafico em preto e branco.}
\item{Eixos}{"s" coloca eixos no grafico (default),\cr
             "n" nao coloca eixos no grafico.}
\item{LabelX}{Nomeia o eixo X, se nulo retorna padrao.}
\item{LabelY}{Nomeia o eixo Y, se nulo retorna padrao.}
\item{Title}{Titulo do grafico, se nulo retorna padrao.}
\item{LinLab}{Vetor com o rotulo para as linhas, se nao informado retorna o padrao dos dados.}
}

\value{{Grafico de escalonamento multidimensional.}
\item{MatrixD}{Matriz das distancias.}
}

\references{
MINGOTI, S. A. \emph{Analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

RENCHER, A.C.; \emph{Methods of Multivariate Analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data(DataQuan) # conjunto de dados quantitativos

Dat <- DataQuan[,2:8]

rownames(Dat) <- DataQuan[1:nrow(DataQuan),1]

MD <- MDS(Dat, Distance = "euclidean", Eixos = "S", LabelX = NULL,
          LabelY = NULL, Title = NULL, Color = "s", LinLab = NULL)

print("Matriz das distancias:"); MD$MatrixD
}

\keyword{Escalonamento Multidimensional}
\keyword{MDS}
