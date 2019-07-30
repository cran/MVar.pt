\name{Plot.PCA}

\alias{Plot.PCA}

\title{Graficos da analise de componentes principais (PCA).}

\description{Graficos da analise de componentes principais (PCA).}

\usage{Plot.PCA(PC, titles = NA, xlabel = NA, ylabel = NA,
         size = 1.1, grid = TRUE, color = TRUE, linlab = NA, 
         casc = TRUE)}

\arguments{
\item{PC}{Dados da funcao PCA.}
\item{titles}{Titulos para os graficos, se nao for definido assume texto padrao.}
\item{xlabel}{Nomeia o eixo X, se nao for definido assume texto padrao.}
\item{ylabel}{Nomeia o eixo Y, se nao for definido assume texto padrao.}
\item{size}{Tamanho dos pontos no grafico.}
\item{grid}{Coloca grade nos graficos (default = TRUE).}
\item{color}{Graficos coloridos (default = TRUE).}
\item{linlab}{Vetor com os rotulos para as observacoes.}
\item{casc}{Efeito cascata na apresentacao dos graficos (default = TRUE).}
}

\value{Retorna varios graficos.}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{PCA}}}

\examples{
data(DataQuan) # conjunto de dados quantitativos

Data <- DataQuan[,2:8]

rownames(Data) <- DataQuan[1:nrow(DataQuan),1]

PC <- PCA(Data, 2) # executa o PCA

Tit = c("Scree-plot","Grafico das Observacoes","Circulo de Correlacoes")

Plot.PCA(PC, titles = Tit, xlabel = NA, ylabel = NA,
         color = TRUE, linlab = NA, casc = TRUE)
}

\keyword{Analise de componentes principais}
\keyword{PCA}
