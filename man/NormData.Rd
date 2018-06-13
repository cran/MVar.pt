\name{NormData}

\alias{NormData}

\title{Normaliza os dados.}

\description{Funcao que normaliza os dados globalmente, ou por coluna.}

\usage{NormData(Data, Type = 1)}
\arguments{
  \item{Data}{Dados a serem normalizados.}
  \item{Type}{1 normaliza global (default),\cr
              2 normaliza por coluna.}
}

\value{\item{DataNorm}{Dados normalizados.}}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data(DataQuan) # conjunto de dados quantitativos

Dat <- DataQuan[,2:8]

Resp = NormData(Dat, Type = 1) # normaliza os dados globalmente

Resp # dados normalizados globalmente

sd(Resp)   # desvio padrao global

mean(Resp) # media global


Resp = NormData(Dat, Type = 2) # normaliza os dados por coluna

Resp # dados normalizados por coluna

apply(Resp, 2, sd) # desvio padrao por coluna

colMeans(Resp)     # medias das colunas
}

\keyword{Normaliza dados}

