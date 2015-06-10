\name{GSVD}

\alias{GSVD}

\title{Decomposicao de Valor Singular Generalizada (GSVD)}

\description{Dada a matriz \eqn{A} de ordem \eqn{nxm}, a Decomposicao de Valor Singular Generalizada (GSVD), envolve a utilizacao de dois conjuntos de matrizes quadradas positivas de ordem \eqn{nxn}  e \eqn{mxm}, respectivamente. Estas duas matrizes expressam restricoes impostas, respectivamente, nas linhas e colunas de \eqn{A}.}

\usage{GSVD(Data, PLin = NULL, PCol = NULL)}
\arguments{
  \item{Data}{Matriz usada para a decomposicao}
  \item{PLin}{Peso para as linhas}
  \item{PCol}{Peso para as colunas}
}

\details{Se nao for utilizado Plin ou PCOL, sera calculada como a decomposicao em valor singular usual.}

\value{
 \item{d}{Autovalores, isto e, vector linha com os valores singulares da decomposicao}
 \item{u}{Autovetores referentes das linhas}
 \item{v}{Autovetores referentes das colunas}
}

\references{ABDI, H. Singular Value Decomposition (SVD) and Generalized Singular Value Decomposition (GSVD). In: SALKIND, N. J. (Ed.). \emph{Encyclopedia of measurement and statistics.} Thousand Oaks: Sage, 2007. p. 907-912.}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
M = matrix(c(1,2,3,4,5,6,7,8,9,10,11,12),nrow=4,ncol=3)

svd(M)  # Decomposicao de Valor Singular usual

GSVD(M) # GSVD com os mesmos resultados anteriores

# GSVD com pesos para linhas e colunas
GSVD(M,PLin=(c(0.1,0.5,2,1.5)),PCol=c(1.3,2,0.8)) 
}

\keyword{Decomposicao de Valor Singular Generalizada}
\keyword{GSVD}
