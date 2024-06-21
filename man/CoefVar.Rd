\name{CoefVar}

\alias{CoefVar}

\title{Coeficiente de variacao dos dados.}

\description{Encontra o coeficiente de variacao dos dados, global ou por coluna.}

\usage{CoefVar(data, type = 1)}

\arguments{
\item{data}{Dados a serem analisados.}
\item{type}{1 Coeficiente de variacao global (default),\cr
            2 Coeficiente de variacao por coluna.}
}

\value{{Coeficiente de variacao, global ou por coluna.}}

\references{
Ferreira, D. F. \emph{Estatistica Basica.} 2 ed. rev. Lavras: UFLA, 2009. 664 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data(DataQuan) # conjunto de dados 

data <- DataQuan[,2:8]

res <- CoefVar(data = data, type = 1) # coeficiente de variacao global
round(res,2)

res <- CoefVar(data = data, type = 2) # coeficiente de variacao por coluna
round(res,2)
}

\keyword{Coeficiente de variacao.}
\keyword{CoefVar}
