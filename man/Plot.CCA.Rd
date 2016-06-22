\name{Plot.CCA}

\alias{Plot.CCA}

\title{Graficos da analise de correlacao canonica (CCA).}

\description{Graficos da analise de correlacao canonica (CCA).}

\usage{Plot.CCA(CCA, Titles = matrix(NA,1,4), Axis = NULL, Color = "s")}

\arguments{
\item{CCA   }{Dados da funcao CCA.}
\item{Titles}{Titulos para os graficos. Se nao for definido assume texto padrao.}
\item{Axis}{Titulos para os eixos dos graficos.}
\item{Color }{"s" para graficos coloridos (default),\cr
              "n" para graficos em preto e branco.}           
}

\value{Retorna varios graficos.}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{CCA}}}

\examples{
data(DataMix) # conjunto de dados

Dat <- DataMix[,2:ncol(DataMix)]

rownames(Dat) <- DataMix[,1]

X <- as.data.frame(NormData(Dat[,1:2],2))

Y <- as.data.frame(NormData(Dat[,5:6],2))

Resp <- CCA(X, Y, Type = 1, Test = "Bartlett", Sign = 0.05) # Analise de correlacao canonica

Titulos = c("Scree-plot","Correlacoes","Scores do grupo X","Scores do grupo Y")

Eixos = c("Eixo X","Eixo Y")

Plot.CCA(Resp, Titulos, Eixos, Color = "S") 
}

\keyword{Analise de correlacao canonica}
\keyword{CCA}