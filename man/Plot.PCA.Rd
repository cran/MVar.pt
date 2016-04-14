\name{Plot.PCA}

\alias{Plot.PCA}

\title{Graficos da analise de componentes principais (PCA).}

\description{Graficos da analise de componentes principais (PCA).}

\usage{Plot.PCA(PC, Titles = matrix(NA,1,2), Color = "s", LinLab = NULL)}

\arguments{
\item{PC}{Dados da funcao PCA.}
\item{Titles}{Titulos para os graficos.}
\item{Color}{"s" para graficos coloridos (default),\cr
             "n" para graficos em preto e branco.}
\item{LinLab}{Vetor com o rotulo para as linhas, se nao
              informado retorna o padrao dos dados.}
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

rownames(Dat) <- as.character(t(DataQuan[1:nrow(DataQuan),1]))

PC <- PCA(Dat, 2) # Executa o PCA

Titles = c("Grafico das Observacoes", "Circulo de Correlacoes")

Plot.PCA(PC, Titles, Color = "S")
}

\keyword{Analise de componentes principais}
\keyword{PCA}
