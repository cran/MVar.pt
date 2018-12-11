\name{Plot.PCA}

\alias{Plot.PCA}

\title{Graficos da analise de componentes principais (PCA).}

\description{Graficos da analise de componentes principais (PCA).}

\usage{Plot.PCA(PC, Titles = NA, xlabel = NA, ylabel = NA,
         Color = TRUE, LinLab = NA, Casc = TRUE)}

\arguments{
\item{PC}{Dados da funcao PCA.}
\item{Titles}{Titulos para os graficos, se nao for definido assume texto padrao.}
\item{xlabel}{Nomeia o eixo X, se nao for definido assume texto padrao.}
\item{ylabel}{Nomeia o eixo Y, se nao for definido assume texto padrao.}
\item{Color}{Graficos coloridos (default = TRUE).}
\item{LinLab}{Vetor com os rotulos para as observacoes, se nao for definido assume texto padrao.}
\item{Casc}{Efeito cascata na apresentacao dos graficos (default = TRUE).}
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

Plot.PCA(PC, Titles = Tit, xlabel = NA, ylabel = NA,
         Color = TRUE, LinLab = NA, Casc = TRUE)
}

\keyword{Analise de componentes principais}
\keyword{PCA}
