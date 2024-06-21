\name{GSVD}

\alias{GSVD}

\title{Decomposicao de valor singular generalizada (GSVD).}

\description{Dada a matriz \eqn{A} de ordem \eqn{nxm}, a decomposicao de valor singular generalizada (GSVD), envolve a utilizacao de dois conjuntos de matrizes quadradas positivas de ordem \eqn{nxn}  e \eqn{mxm}, respectivamente. Estas duas matrizes expressam restricoes impostas, respectivamente, nas linhas e colunas de \eqn{A}.}

\usage{GSVD(data, plin = NULL, pcol = NULL)}
\arguments{
  \item{data}{Matriz usada para a decomposicao.}
  \item{plin}{Peso para as linhas.}
  \item{pcol}{Peso para as colunas.}
}

\details{Se nao for utilizado plin ou pcol, sera calculada como a decomposicao em valor singular usual.}

\value{
 \item{d}{Autovalores, isto e, vector linha com os valores singulares da decomposicao.}
 \item{u}{Autovetores referentes as linhas.}
 \item{v}{Autovetores referentes as colunas.}
}

\references{Abdi, H. Singular Value Decomposition (SVD) and Generalized Singular Value Decomposition (GSVD). In: SALKIND, N. J. (Ed.). \emph{Encyclopedia of measurement and statistics.} Thousand Oaks: Sage, 2007. p. 907-912.}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data <- matrix(c(1,2,3,4,5,6,7,8,9,10,11,12), nrow = 4, ncol = 3)

svd(data)  # decomposicao de valor singular usual

GSVD(data) # GSVD com os mesmos resultados anteriores

# GSVD com pesos para linhas e colunas
GSVD(data, plin = c(0.1,0.5,2,1.5), pcol = c(1.3,2,0.8)) 
}

\keyword{Decomposicao de valor singular generalizada}
\keyword{GSVD}
