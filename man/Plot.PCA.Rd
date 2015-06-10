\name{Plot.PCA}

\alias{Plot.PCA}

\title{Desenha graficos da Analise de Componentes Principais (PCA)}

\description{Desenha graficos da Analise de Componentes Principais (PCA)}

\usage{Plot.PCA(PC, Titles = matrix(NA,1,2), Color = "s")}

\arguments{
\item{PC}{Dados da funcao PCA}
\item{Titles}{Titulos para os graficos}
\item{Color}{"s" para graficos coloridos - default\cr
             "n" para graficos em preto e branco}
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

PC <- PCA(Dat, 1) # Executa o PCA

Titles = c("Grafico das Observacoes","Circulo de Correlacoes")

Plot.PCA(PC,Titles,Color="S")
}

\keyword{Analise de Componentes Principais}
\keyword{PCA}
