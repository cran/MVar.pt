\name{PCA}

\alias{PCA}

\title{Analise de Componentes Principais (PCA)}

\description{Realiza Analise de Componentes Principais (PCA) em um conjunto de dados.}

\usage{PCA(Data, Type = 1)}

\arguments{
\item{Data}{Dados a serem a analizados}
\item{Type}{1 para analise utilizando a matriz de covariancia - default\cr
            2 para analise utilizando a matriz de correlacao}
}

\value{
\item{MatrixMC      }{Matriz de Covariancia ou de  Correlacao conforme Type}
\item{MatrixAutoVlr }{Matriz de Autovalores (Variancias) com as proporcoes e proporcoes acumuladas}
\item{MatrixAutoVec }{Matriz de Autovetores - Componentes Principais}
\item{MatrixVCP     }{Matriz da Covariancia dos Componentes Principais com as Variaveis Originais}
\item{MatrixCCP     }{Matriz da Correlacao dos Componentes Principais com as Variaveis Originais}
\item{MatrixEsc     }{Matriz com os escores dos Componentes Principais}
}

\references{
HOTELLING, H.; Analysis of a complex of statistical variables into principal components. \emph{Journal of Educational Psychology}, v. 24, P. 417-441, set. 1933:

MINGOTI, SUELI APARECIDA; \emph{Analise de dados atraves de metodos de estatistica multivariada: uma abordagem aplicada}. Belo Horizonte: Editora UFMG, 2005. 297 p.

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

PC <- PCA(Dat, 1) # Executa o PCA

print("Matriz de Covariancia/Correlacao:"); round(PC$MatrixMC,2)

print("Componentes Principais:"); round(PC$MatrixAutoVec,2)

print("Variancias dos Componentes Principais"); round(PC$MatrixAutoVlr,2)

print("Covariancia dos Componentes Principais"); round(PC$MatrixVCP,2)

print("Correlacao dos Componentes Principais"); round(PC$MatrixCCP,2)

print("Escores dos Componentes Principais"); round(PC$MatrixEsc,2)
}

\keyword{Analise de Componentes Principais}
\keyword{PCA}
