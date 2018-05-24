\name{Plot.PCA}

\alias{Plot.PCA}

\title{Graficos da analise de componentes principais (PCA).}

\description{Graficos da analise de componentes principais (PCA).}

\usage{Plot.PCA(PC, Titles = matrix(NA,1,3), xlabel = NA, ylabel = NA,
         Color = TRUE, LinLab = NA)}

\arguments{
\item{PC}{Dados da funcao PCA.}
\item{Titles}{Titulos para os graficos.}
\item{xlabel}{Nomeia o eixo X, se nao definido retorna padrao.}
\item{ylabel}{Nomeia o eixo Y, se nao definido retorna padrao.}
\item{Color}{Graficos coloridos (default = TRUE).}
\item{LinLab}{Vetor com o rotulo para as linhas, se nao informado retorna padrao.}
}

\value{Retorna varios graficos.}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{PCA}}}

\examples{
data(DataQuan) # conjunto de dados quantitativos

Dat <- DataQuan[,2:8]

rownames(Dat) <- DataQuan[1:nrow(DataQuan),1]

PC <- PCA(Dat, 2) # Executa o PCA

Titles = c("Scree-plot","Grafico das Observacoes", "Circulo de Correlacoes")

Plot.PCA(PC, Titles, Color = TRUE)
}

\keyword{Analise de componentes principais}
\keyword{PCA}
