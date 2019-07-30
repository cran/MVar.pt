\name{NormTest}

\alias{NormTest}

\title{Teste de normalidade dos dados.}

\description{Verificar a normalidade dos dados, baseado no teste de coeficiente de assimetria.}

\usage{NormTest(data, sign = 0.05)}

\arguments{
\item{data}{Dados a serem analisados.}
\item{sign}{Grau de significancia do teste (default 5\%).}
}

\value{
\item{statistic}{Valor Chi-quadrado observado, ou seja, a estatistica do teste.}
\item{chisquare}{Valor Chi-quadrado calculado.}
\item{gl       }{Grau de liberdade.}
\item{p.value  }{Valor p.}
}

\references{
MINGOTI, S. A. \emph{Analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

RENCHER, A. C. \emph{Methods of Multivariate Analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.

FERREIRA, D. F. \emph{Estatistica Multivariada.} 2a ed. revisada e ampliada. Lavras: Editora UFLA, 2011. 676 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data <- cbind(rnorm(100,2,3), rnorm(100,1,2))

NormTest(data)

plot(density(data))


data <- cbind(rexp(200,3), rexp(200,3))

NormTest(data, sign = 0.01)

plot(density(data))
}

\keyword{Teste de normalidade dos dados.}
\keyword{NormTest}
