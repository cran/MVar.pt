\name{CA}

\alias{CA}

\title{Analise de correspondencia (CA).}

\description{Realiza analise de correspondencia simples (CA) e multipla (MCA) em um conjunto de dados.}

\usage{CA(data, typdata = "f", typmatrix = "I")}

\arguments{
\item{data}{Dados a serem analisados (tabela de contingencia).}
\item{typdata}{"f" para dados de frequencia (default),\cr
               "c" para dados qualitativos.}
\item{typmatrix}{Matriz usada para calculos quando typdata = "c".\cr
                 "I" para matriz indicadora (default),\cr
                 "B" para matriz de Burt.}
}

\value{
\item{depdata}{Verifica se as linhas e colunas sao dependentes, ou independentes pelo teste Qui-quadrado, a nivel 5\% de significancia.}
\item{typdata}{Tipo de dados: "F" frequencia ou "C" qualitativo.}
\item{numcood}{Numero de coordenadas principais.}
\item{mtxP}{Matriz da frequencia relativa.}
\item{vtrR}{Vetor com as somas das linhas.}
\item{vtrC}{Vetor com as somas das colunas.}
\item{mtxPR}{Matriz com perfil das linhas.}
\item{mtxPC}{Matriz com perfil das colunas.}
\item{mtxZ}{Matriz Z.}
\item{mtxU}{Matriz com os autovetores U.}
\item{mtxV}{Matriz com os autovetores V.}
\item{mtxL}{Matriz com os autovalores.}
\item{mtxX}{Matriz com as coordenadas principais das linhas.}
\item{mtxY}{Matriz com as coordenadas principais das colunas.}
\item{mtxAutvlr}{Matriz das inercias (variancias), com as proporcoes e proporcoes acumuladas.}
}

\references{
MINGOTI, S. A. \emph{Analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

RENCHER, A. C. \emph{Methods of multivariate analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{Plot.CA}}}

\examples{
data(DataFreq) # conjunto de dados de frequencia

data <- DataFreq[,2:ncol(DataFreq)]

rownames(data) <- as.character(t(DataFreq[1:nrow(DataFreq),1]))

Resp <- CA(data = data, "f") # realiza CA

print("Existe dependencia entre as linhas e as colunas?"); Resp$depdata

print("Numero de coordenadas principais:"); Resp$numcood

print("Coordenadas principais das Linhas:"); round(Resp$mtxX,2)

print("Coordenadas principais das Colunas:"); round(Resp$mtxY,2)

print("Inercias das componentes principais:"); round(Resp$mtxAutvlr,2)
}

\keyword{Analise de correspondencia simples}
\keyword{Analise de correspondencia multipla} 
\keyword{CA}
\keyword{MCA}
