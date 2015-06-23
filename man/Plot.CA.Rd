\name{Plot.CA}

\alias{Plot.CA}

\title{Desenha graficos da Analise de Correspondencia (CA) simples e multipla}

\description{Desenha graficos da Analise de Correspondencia (CA) simples e multipla.}

\usage{Plot.CA(AC, Titles = matrix(NA,1,3), Color = "s")}

\arguments{
\item{AC}{Dados da funcao CA}
\item{Titles}{Titulos para os graficos}
\item{Color}{"s" para graficos coloridos - default\cr
             "n" para graficos em preto e branco}
}

\value{Retorna varios graficos.}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{CA}}}

\examples{
data(DataFreq) # conjunto de dados de frequencia

Dat <- DataFreq[,2:ncol(DataFreq)]

rownames(Dat) <- as.character(t(DataFreq[1:nrow(DataFreq),1]))

Resp <- CA(Dat, "f") # realiza CA

Titles = c("Observacoes", "Variaveis", "Observacoes/Variaveis")

Plot.CA(Resp, Titles, Color = "s")


data(DataQuali) # Conjunto de dados qualitativos

Dat <- DataQuali[,2:ncol(DataQuali)]

rownames(Dat) <- as.character(t(DataQuali[1:nrow(DataQuali),1]))

Resp <- CA(Dat, "c", "b") # realiza CA

Titles = c("","Graficos das Variaveis")

Plot.CA(Resp, Titles, Color = "s")

}

\keyword{Analise de Correspondencia}
\keyword{CA}
