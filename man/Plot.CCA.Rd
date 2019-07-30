\name{Plot.CCA}

\alias{Plot.CCA}

\title{Graficos da analise de correlacao canonica (CCA).}

\description{Graficos da analise de correlacao canonica (CCA).}

\usage{Plot.CCA(CCA, titles = NA, xlabel = NA, ylabel = NA,
         size = 1.1, grid = TRUE, color = TRUE, casc = TRUE)}

\arguments{
\item{CCA   }{Dados da funcao CCA.}
\item{titles}{Titulos para os graficos, se nao for definido assume texto padrao.}
\item{xlabel}{Nomeia o eixo X, se nao for definido assume texto padrao.}
\item{ylabel}{Nomeia o eixo Y, se nao for definido assume texto padrao.}
\item{size}{Tamanho dos pontos no grafico.}
\item{grid}{Coloca grade nos graficos (default = TRUE).}
\item{color}{Graficos coloridos (default = TRUE).}
\item{casc}{Efeito cascata na apresentacao dos graficos (default = TRUE).}
}

\value{Retorna varios graficos.}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{CCA}}}

\examples{
data(DataMix) # conjunto de dados

Data <- DataMix[,2:ncol(DataMix)]

rownames(Data) <- DataMix[,1]

X <- as.data.frame(NormData(Data[,1:2],2))

Y <- as.data.frame(NormData(Data[,5:6],2))

Resp <- CCA(X, Y, type = 1, test = "Bartlett", sign = 0.05) # Analise de correlacao canonica

Tit = c("Scree-plot","Correlacoes","Scores do grupo X","Scores do grupo Y")

Plot.CCA(Resp, titles = Tit, xlabel = NA, ylabel = NA,
         color = TRUE, casc = TRUE)
}

\keyword{Analise de correlacao canonica}
\keyword{CCA}
