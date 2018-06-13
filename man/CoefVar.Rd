\name{CoefVar}

\alias{CoefVar}

\title{Coeficiente de variacao dos dados.}

\description{Encontra o coeficiente de variacao dos dados, global ou por coluna.}

\usage{CoefVar(Data, Type = 1)}

\arguments{
\item{Data}{Dados a serem analisados.}
\item{Type}{1 Coeficiente de variacao global (default),\cr
            2 Coeficiente de variacao por coluna.}
}

\value{{Coeficiente de variacao, global ou por coluna.}}

\references{
FERREIRA, D. F.; \emph{Estatistica Basica.} 2 ed. rev. Lavras: UFLA, 2009. 664 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data(DataQuan) # conjunto de dados 

Dat <- DataQuan[,2:8]

Resp <- CoefVar(Dat, Type = 1) # coeficiente de variacao global
round(Resp,2)

Resp <- CoefVar(Dat, Type = 2) # coeficiente de variacao por coluna
round(Resp,2)
}

\keyword{Coeficiente de variacao.}
\keyword{CoefVar}
