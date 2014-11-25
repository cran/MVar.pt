\name{IM}

\alias{IM}

\title{Matriz Indicadora}

\description{
Na Matriz Indicadora os elementos estao dispostos na forma de variaveis \emph{dummy}, em outras palavras, 1 para uma categoria escolhida como variavel resposta e 0 para as outras categorias de uma mesma variavel.}

\usage{IM(Data, Names = "s")}

\arguments{
\item{Data}{Dados Categoricos}
\item{Names}{"s" para incluir os nomes das variaveis nos niveis da Matriz Indicadora - default\cr
             "n" nao inclui}
}

\value{\item{Dados}{Retorna dados convertidos em Matriz Indicadora.}}

\references{RENCHER, A.C.; \emph{Methods of Multivariate Analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{

Date=matrix(c("S","S","N","N",1,2,3,4,"N","S","T","N"),nrow=4,ncol=3)

IM(Date,"n")

data(DataQuali) # Conjunto de dados qualitativos

IM(DataQuali)
}

\keyword{Variaveis Dummy}
\keyword{Matriz Indicadora}
