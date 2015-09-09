\name{CA}

\alias{CA}

\title{Analise de correspondencia}

\description{Realiza analise de correspondencia simples (CA) e multipla (MCA) em um conjunto de dados.}

\usage{CA(Data, TypData, TypMatrix = "I")}

\arguments{
\item{Data}{Dados a serem analisados, isto e, tabela de contingencia.}
\item{TypData}{"f" para dados de frequencia,\cr
               "c" para dados qualitativos.}
\item{TypMatrix}{Matriz usada para calculos quanto TypData = "c".\cr
                 "I" para matriz indicadora - Default,\cr
                 "B" para matriz de Burt.}
}

\value{
\item{DepData}{Verifica se os dados sao dependentes ou independentes a nivel 5\% de significancia.}
\item{TypData}{Tipo de dados: "F" frequencia ou "C" qualitativo.}
\item{NumCood}{Numero de coordenadas principais.}
\item{MatrixP}{Matriz da frequencia relativa.}
\item{VectorR}{Vetor com as somas das linhas.}
\item{VectorC}{Vetor com as somas das colunas.}
\item{MatrixPR}{Matriz com perfil das linhas.}
\item{MatrixPC}{Matriz com perfil das colunas.}
\item{MatrixZ}{Matriz Z.}
\item{MatrixU}{Matriz com os autovetores U.}
\item{MatrixV}{Matriz com os autovetores V.}
\item{MatrixL}{Matriz com os autovalores.}
\item{MatrixX}{Matriz com as coordenadas principais das linhas.}
\item{MatrixY}{Matriz com as coordenadas principais das colunas.}
\item{MatrixAutoVlr}{Matriz das inercias (variancias), com as proporcoes e proporcoes acumuladas.}
}

\references{
MINGOTI, S. A. \emph{Analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

RENCHER, A.C.; \emph{Methods of Multivariate Analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{Plot.CA}}}

\examples{
data(DataFreq) # conjunto de dados de frequencia

Dat <- DataFreq[,2:ncol(DataFreq)]

rownames(Dat) <- as.character(t(DataFreq[1:nrow(DataFreq),1]))

Resp <- CA(Dat, "f") # realiza CA

print("Existe dependencia entre as linhas e as colunas?"); print(Resp$DepData)

print("Numero de coordenadas principais:"); print(Resp$NumCood)

print("Coordenadas principais das Linhas:"); round(Resp$MatrixX,2)

print("Coordenadas principais das Colunas:"); round(Resp$MatrixY,2)

print("Inercias das componentes principais:"); round(Resp$MatrixAutoVlr,2)
}

\keyword{Analise de correspondencia simples}
\keyword{Analise de correspondencia multipla} 
\keyword{CA}
\keyword{MCA}
