\name{NormData}

\alias{NormData}

\title{Normaliza os dados.}

\description{Funcao que normaliza os dados globalmente, ou por coluna.}

\usage{NormData(data, type = 1)}
\arguments{
  \item{data}{Dados a serem normalizados.}
  \item{type}{1 normaliza global (default),\cr
              2 normaliza por coluna.}
}

\value{\item{dataNorm}{Dados normalizados.}}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data(DataQuan) # conjunto de dados quantitativos

data <- DataQuan[,2:8]

res <- NormData(data, type = 1) # normaliza os dados globalmente

res # dados normalizados globalmente

sd(res)   # desvio padrao global

mean(res) # media global


res <- NormData(data, type = 2) # normaliza os dados por coluna

res # dados normalizados por coluna

apply(res, 2, sd) # desvio padrao por coluna

colMeans(res)     # medias das colunas
}

\keyword{Normaliza dados}

