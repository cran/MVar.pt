\name{NormTest}

\alias{NormTest}

\title{Teste de normalidade dos dados.}

\description{Verificar a normalidade dos dados, baseado no teste de coeficiente de assimetria.}

\usage{NormTest(Data, Sign = 0.05)}

\arguments{
\item{Data}{Dados a serem analisados.}
\item{Sign}{Grau de significancia do teste (default 5\%).}
}

\value{{Resultados do teste.}}

\references{
MINGOTI, S. A. \emph{Analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

RENCHER, A.C.; \emph{Methods of Multivariate Analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
Dat <- cbind(rnorm(100,2,3), rnorm(100,1,2))

NormTest(Dat)

plot(density(Dat))


Dat <- cbind(rexp(200,3), rexp(200,3))

NormTest(Dat, Sign = 0.01)

plot(density(Dat))
}

\keyword{Teste de normalidade dos dados.}
\keyword{NormTest}
