\name{CA}

\alias{CA}

\title{Analise de Correspondencia }

\description{Realiza Analise de Correspondencia Simples (CA) e Multipla (MCA) em um conjunto de dados.}

\usage{CA(Data, TypData, TypMatrix = "I")}

\arguments{
\item{Data}{Dados a serem analisados, isto e, Tabela de Contingencia}
\item{TypData}{"f" para Dados de Frequencia\cr
               "c" para Dados Qualitativos}
\item{TypMatrix}{Matriz usada para calculos quanto TypData = "c"\cr
                 "I" para Matriz Indicadora - Default\cr
                 "B" para Matriz de Burt}
}

\value{
\item{DepData}{Verificacao se os dados sao Dependentes ou Independentes a nivel 5\% de significancia}
\item{TypData}{Tipo de dados: "F" Frequencia ou "C" Qualitativo}
\item{NumCood}{Numero de coordenadas principais}
\item{MatrixX}{Matriz com as coordenadas principais das Linhas}
\item{MatrixY}{Matriz com as coordenadas principais das Colunas}
\item{MatrixAutoVlr}{Matriz das inercias (variancias), com as proporcoes e proporcoes acumuladas}
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

Resp <- CA(Dat,"f") # realiza CA

print("Existe dependencia entre as linhas e as colunas?"); print(Resp$DepData)

print("Numero de coordenadas principais:"); print(Resp$NumCood)

print("Coordenadas principais das Linhas:"); round(Resp$MatrixX,2)

print("Coordenadas principais das Colunas:"); round(Resp$MatrixY,2)

print("Inercias das componentes principais:"); round(Resp$MatrixAutoVlr,2)
}

\keyword{Analise de Correspondencia Simples}
\keyword{Analise de Correspondencia Multipla} 
\keyword{CA}
\keyword{MCA}
