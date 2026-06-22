\name{CI}

\alias{CI}

\title{Intervalos de Confianca Uni e Multivariado.}

\description{Encontra os intervalos de confianca uni e multivado das variaveis de um conjunto de dados.}

\usage{CI(data, sigma = NA, conf = 0.95, type = "T")}

\arguments{
  \item{data }{Dados para se encontrar os intervalos de confianca.}
  \item{sigma}{Matriz de variancia e covariancia, caso contrario o IC sera pela variancia amostral (default sigma = NA).}
  \item{conf }{Nivel de confianca do IC (default conf = 95\% ).}
  \item{type }{Tipo do intervalo: 'T' para \eqn{T^2} de Hotelling ou 'B' para Bonferroni.}
}

\value{
  \item{ciu}{Intervalo de confianca univariado com 'sigma' de confianca.}
  \item{cim}{Intervalo de confianca multivariado com 'sigma' de confianca.}
}

\references{
Ferreira, D. F. \emph{Estatistica Multivariada.} 2a ed. revisada e ampliada. Lavras: Editora UFLA, 2011. 676 p.

Rencher, A. C. \emph{Methods of multivariate analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.. 708 p.
}

\author{
Paulo Cesar Ossani
}

\seealso{\code{\link{Plot.CI}}}

\examples{
data(iris) # conjunto de dados

# Intervalo com variancia desconhecida da populacao
res <- CI(data = iris[,1:4], sigma = NA, conf = 0.95, type = 'T') 
# res <- CI(data = iris[,1:4], sigma = NA, conf = 0.95, type = 'B') 

res$ciu # intervalo univariado
res$cim # intervalo mutivariado


# Intervalo com variancia conhecida da populacao
sig <- matrix(c(5.286, 0.942, 1.274, 4.516, # exemplo de matriz de variancia conhecida
                0.942, 3.690, 0.730, 1.122,
                1.274, 0.730, 7.116, 1.296,
                4.516, 1.122, 1.296, 4.581), 
                nrow = 4, byrow = TRUE)
           
# res <- CI(data = iris[,1:4], sigma = sig, conf = 0.95, type = 'T')
res <- CI(data = iris[,1:4], sigma = sig, conf = 0.95, type = 'B')

res$ciu # intervalo univariado
res$cim # intervalo mutivariado
}

\keyword{Intervalo de Confianca}
\keyword{CI}
