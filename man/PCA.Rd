\name{PCA}

\alias{PCA}

\title{Analise de componentes principais (PCA).}

\description{Realiza analise de componentes principais (PCA) em um conjunto de dados.}

\usage{PCA(Data, Type = 1)}

\arguments{
\item{Data}{Dados a serem a analizados.}
\item{Type}{1 para analise utilizando a matriz de covariancia (default),\cr
            2 para analise utilizando a matriz de correlacao.}
}

\value{
\item{MatrixMC      }{Matriz de covariancia ou de correlacao conforme "Type".}
\item{MatrixAutoVlr }{Matriz de autovalores (variancias) com as proporcoes e proporcoes acumuladas.}
\item{MatrixAutoVec }{Matriz de autovetores - componentes principais.}
\item{MatrixVCP     }{Matriz da covariancia dos componentes principais com as variaveis originais.}
\item{MatrixCCP     }{Matriz da correlacao dos componentes principais com as variaveis originais.}
\item{MatrixEsc     }{Matriz com os escores dos componentes principais.}
}

\references{
HOTELLING, H. Analysis of a complex of statistical variables into principal components. \emph{Journal of Educational Psychology}, Arlington, v. 24, p. 417-441, Sept. 1933.

MINGOTI, S. A. \emph{Analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

RENCHER, A.C.; \emph{Methods of Multivariate Analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{Plot.PCA}}}

\examples{
data(DataQuan) # conjunto de dados quantitativos

Dat <- DataQuan[,2:8]

rownames(Dat) <- as.character(t(DataQuan[1:nrow(DataQuan),1]))

PC <- PCA(Dat, 2) # Executa o PCA

print("Matriz de Covariancia/Correlacao:"); round(PC$MatrixMC,2)

print("Componentes Principais:"); round(PC$MatrixAutoVec,2)

print("Variancias dos Componentes Principais"); round(PC$MatrixAutoVlr,2)

print("Covariancia dos Componentes Principais"); round(PC$MatrixVCP,2)

print("Correlacao dos Componentes Principais"); round(PC$MatrixCCP,2)

print("Escores dos Componentes Principais"); round(PC$MatrixEsc,2)
}

\keyword{Analise de componentes principais}
\keyword{PCA}
