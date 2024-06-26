\name{PCA}

\alias{PCA}

\title{Analise de componentes principais (PCA).}

\description{Realiza analise de componentes principais (PCA) em um conjunto de dados.}

\usage{PCA(data, type = 1)}

\arguments{
\item{data}{Dados a serem analizados.}
\item{type}{1 para analise utilizando a matriz de covariancia (default),\cr
            2 para analise utilizando a matriz de correlacao.}
}

\value{
\item{mtxC      }{Matriz de covariancia ou de correlacao conforme "type".}
\item{mtxAutvlr }{Matriz de autovalores (variancias) com as proporcoes e proporcoes acumuladas.}
\item{mtxAutvec }{Matriz de autovetores - componentes principais.}
\item{mtxVCP     }{Matriz da covariancia dos componentes principais com as variaveis originais.}
\item{mtxCCP     }{Matriz da correlacao dos componentes principais com as variaveis originais.}
\item{mtxscores     }{Matriz com os escores dos componentes principais.}
}

\references{
Hotelling, H. Analysis of a complex of statistical variables into principal components. \emph{Journal of Educational Psychology}, Arlington, v. 24, p. 417-441, Sept. 1933.

Mingoti, S. A. \emph{Analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

Ferreira, D. F. \emph{Estatistica Multivariada.} 2a ed. revisada e ampliada. Lavras: Editora UFLA, 2011. 676 p.

Rencher, A. C. \emph{Methods of multivariate analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.. 708 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{Plot.PCA}}}

\examples{
data(DataQuan) # conjunto de dados quantitativos

data <- DataQuan[,2:8]

rownames(data) <- DataQuan[1:nrow(DataQuan),1]

pc <- PCA(data = data, 2) # executa o PCA

print("Matriz de Covariancia/Correlacao:"); round(pc$mtxC,2)

print("Componentes Principais:"); round(pc$mtxAutvec,2)

print("Variancias dos Componentes Principais:"); round(pc$mtxAutvlr,2)

print("Covariancia dos Componentes Principais:"); round(pc$mtxVCP,2)

print("Correlacao dos Componentes Principais:"); round(pc$mtxCCP,2)

print("Escores dos Componentes Principais:"); round(pc$mtxscores,2)
}

\keyword{Analise de componentes principais}
\keyword{PCA}
