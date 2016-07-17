\name{NormTest}

\alias{NormTest}

\title{Teste de normalidade dos dados.}

\description{Verificar a normalidade dos dados, baseado no teste de coeficiente de assimetria.}

\usage{NormTest(Data, Sign = 0.05)}

\arguments{
\item{Data}{Dados a serem analisados.}
\item{Sign}{Grau de significancia do teste (default 5\%).}
}

\value{
\item{Statistic  }{Valor Chi-quadrado observado, ou seja, a estatistica do teste.}
\item{ChiQuadrado}{Valor Chi-quadrado calculado.}
\item{GL         }{Grau de liberdade.}
\item{p.Value    }{Valor p.}
}

\references{
MINGOTI, S. A. \emph{Analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

RENCHER, A.C.; \emph{Methods of Multivariate Analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.

FERREIRA, D. F.; \emph{Estatistica Multivariada.} 2a ed. revisada e ampliada. Lavras: Editora UFLA, 2011. 676 p.
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
