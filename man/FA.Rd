\name{FA}

\alias{FA}

\title{Analise fatorial (FA).}

\description{Realiza analise fatorial (FA) em um conjunto de dados.}

\usage{FA(data, method = "PC", type = 2, nfactor = 1, rotation = "None",
   scoresobs = "Bartlett", converg = 1e-5, iteracao = 1000, 
   testfit = TRUE) 
}

\arguments{
\item{data}{Dados a serem analisados.}
\item{method}{Metodo de analise:\cr
              "PC" - Componentes Principais (default),\cr
              "PF" - Fator Principal,\cr
              "ML" - Maxima Verossimilhanca.}
\item{type}{1 para analise utilizando a matriz de covariancia,\cr
            2 para analise utilizando a matriz de correlacao (default).}
\item{rotation}{Tipo de rotacao: "None" (default), "Varimax" e "Promax".}
\item{nfactor}{Numero de fatores (default = 1).}
\item{scoresobs}{Tipo de scores para as observacoes: "Bartlett" (default) ou "Regression".}
\item{converg}{Valor limite para convergencia para soma do quadrado dos residuos para metodo de Maxima Verossimilhanca (default = 1e-5).}
\item{iteracao}{Numero maximo de iteracoes para metodo de Maxima Verossimilhanca (default = 1000).}
\item{testfit}{Testa o ajuste do modelo para o metodo de Maxima Verossimilhanca (default = TRUE).}
}

\value{
\item{mtxMC}{Matriz de Correlacao/Covariancia.}
\item{mtxAutvlr}{Matriz de autovalores.}
\item{mtxAutvec}{Matriz de autovetores.}
\item{mtxvar}{Matriz de variancias e proporcoes.}
\item{mtxcarga}{Matriz de cargas fatoriais.}
\item{mtxvaresp}{Matriz das variancias especificas.}
\item{mtxcomuna}{Matriz das comunalidades.}
\item{mtxresidue}{Matriz dos residuos.}
\item{vlrsqrs}{Valor limite superior para a soma dos quadrados dos residuos.}
\item{vlrsqr}{Soma dos quadrados dos residuos.}
\item{mtxresult}{Matriz com todos os resultados associados.}
\item{mtxscores}{Matriz com os escores das observacoes.}
\item{coefscores}{Matriz com os escores dos coeficientes dos fatores.}
}

\references{
MINGOTI, S. A. \emph{Analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

Kaiser, H. F.\emph{The varimax criterion for analytic rotation in factor analysis.} Psychometrika 23, 187-200, 1958.

RENCHER, A. C. \emph{Methods of multivariate analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.

FERREIRA, D. F. \emph{Estatistica Multivariada.} 2a ed. revisada e ampliada. Lavras: Editora UFLA, 2011. 676 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{Plot.FA}}}

\examples{
data(DataQuan) # conjunto de dados

data <- DataQuan[,2:ncol(DataQuan)]

rownames(data) <- DataQuan[,1]

res <- FA(data = data, method = "PC", type = 2, nfactor = 3, rotation = "None",
          scoresobs = "Bartlett", converg = 1e-5, iteracao = 1000, 
          testfit = TRUE) 

print("Matriz com todos os resultados associados:"); round(res$mtxresult,3)

print("Soma dos Quadrados dos Residuos:"); round(res$vlrsqr,3)

print("Matriz das Cargas Fatoriais:"); round(res$mtxcarga,3)

print("Matriz com os escores das observacoes:"); round(res$mtxscores,3)

print("Matriz com os escores dos coeficientes dos fatores:"); round(res$coefscores,3)

}

\keyword{Analise Fatorial}
\keyword{FA}

