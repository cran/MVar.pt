\name{Plot.CA}

\alias{Plot.CA}

\title{Graficos da analise de correspondencia (CA) simples e multipla.}

\description{Graficos da analise de correspondencia (CA) simples e multipla.}

\usage{Plot.CA(AC, Titles = matrix(NA,1,4), Color = "s", LinLab = NULL)}

\arguments{
\item{AC}{Dados da funcao CA.}
\item{Titles}{Titulos para os graficos.}
\item{Color}{"s" para graficos coloridos (default),\cr
             "n" para graficos em preto e branco.}
\item{LinLab}{Vetor com o rotulo para as linhas, para dados de frequencia.\cr
              Se nao informado retorna o padrao dos dados.}
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

rownames(Dat) <- DataFreq[1:nrow(DataFreq),1]

Resp <- CA(Dat, "f") # realiza CA

Titles = c("Scree-plot","Observacoes", "Variaveis", "Observacoes/Variaveis")

Plot.CA(Resp, Titles, Color = "s")


data(DataQuali) # Conjunto de dados qualitativos

Dat <- DataQuali[,2:ncol(DataQuali)]

Resp <- CA(Dat, "c", "b") # realiza CA

Titles = c("","","Graficos das Variaveis")

Plot.CA(Resp, Titles, Color = "s")

}

\keyword{Analise de correspondencia}
\keyword{CA}
