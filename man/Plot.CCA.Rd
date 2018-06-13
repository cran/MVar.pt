\name{Plot.CCA}

\alias{Plot.CCA}

\title{Graficos da analise de correlacao canonica (CCA).}

\description{Graficos da analise de correlacao canonica (CCA).}

\usage{Plot.CCA(CCA, Titles = NA, xlabel = NA, ylabel = NA,
         Color = TRUE, Casc = TRUE)}

\arguments{
\item{CCA   }{Dados da funcao CCA.}
\item{Titles}{Titulos para os graficos, se nao for definido assume texto padrao.}
\item{xlabel}{Nomeia o eixo X, se nao for definido assume texto padrao.}
\item{ylabel}{Nomeia o eixo Y, se nao for definido assume texto padrao.}
\item{Color }{Graficos coloridos (default = TRUE).}
\item{Casc}{Efeito cascata na apresentacao dos graficos (default = TRUE).}
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

Tit = c("Scree-plot","Correlacoes","Scores do grupo X","Scores do grupo Y")

Plot.CCA(Resp, Titles = Tit, xlabel = NA, ylabel = NA,
         Color = TRUE, Casc = TRUE)
}

\keyword{Analise de correlacao canonica}
\keyword{CCA}
