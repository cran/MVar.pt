\name{Plot.CCA}

\alias{Plot.CCA}

\title{Graficos da analise de correlacao canonica (CCA).}

\description{Graficos da analise de correlacao canonica (CCA).}

\usage{Plot.CCA(CCA, titles = NA, xlabel = NA, ylabel = NA,
         size = 1.1, grid = TRUE, color = TRUE, savptc = FALSE, 
         width = 3236, height = 2000, res = 300, casc = TRUE)}

\arguments{
\item{CCA   }{Dados da funcao CCA.}
\item{titles}{Titulos para os graficos, se nao for definido assume texto padrao.}
\item{xlabel}{Nomeia o eixo X, se nao for definido assume texto padrao.}
\item{ylabel}{Nomeia o eixo Y, se nao for definido assume texto padrao.}
\item{size}{Tamanho dos pontos no grafico.}
\item{grid}{Coloca grade nos graficos (default = TRUE).}
\item{color}{Graficos coloridos (default = TRUE).}
\item{savptc}{Salva as imagens dos graficos em arquivos (default = FALSE).}   
\item{width}{Largura do grafico quanto savptc = TRUE (defaul = 3236).}
\item{height}{Altura do grafico quanto savptc = TRUE (default = 2000).}
\item{res}{Resolucao nominal em ppi do grafico quanto savptc = TRUE (default = 300).}
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

data <- DataMix[,2:ncol(DataMix)]

rownames(data) <- DataMix[,1]

X <- data[,1:2]

Y <- data[,5:6]

res <- CCA(X, Y, type = 2, test = "Bartlett", sign = 0.05) # Analise de correlacao canonica

tit <- c("Scree-plot","Correlacoes","Scores do grupo X","Scores do grupo Y")

Plot.CCA(res, titles = tit, xlabel = NA, ylabel = NA,
         color = TRUE, savptc = FALSE, width = 3236, 
         height = 2000, res = 300, casc = TRUE)
}

\keyword{Analise de correlacao canonica}
\keyword{CCA}
