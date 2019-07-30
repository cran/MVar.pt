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

Resp = NormData(data, type = 1) # normaliza os dados globalmente

Resp # dados normalizados globalmente

sd(Resp)   # desvio padrao global

mean(Resp) # media global


Resp = NormData(data, type = 2) # normaliza os dados por coluna

Resp # dados normalizados por coluna

apply(Resp, 2, sd) # desvio padrao por coluna

colMeans(Resp)     # medias das colunas
}

\keyword{Normaliza dados}

