\name{Plot.FA}

\alias{Plot.FA}

\title{Graficos da analise fatorial (FA).}

\description{Graficos da analise fatorial (FA).}

\usage{Plot.FA(FA, Titles = matrix(NA,1,4), xlabel = NA, ylabel = NA,
        Color = TRUE, LinLab = NA)}

\arguments{
\item{FA}{Dados da funcao FA.}
\item{Titles}{Titulos para os graficos.}
\item{xlabel}{Nomeia o eixo X, se nao definido retorna padrao.}
\item{ylabel}{Nomeia o eixo Y, se nao definido retorna padrao.}
\item{Color}{Graficos coloridos (default = TRUE).}
\item{LinLab}{Vetor com o rotulo para as linhas, se nao
              informado retorna padrao.}
}

\value{Retorna varios graficos.}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{FA}}}

\examples{
data(DataQuan) # conjunto de dados

Dat <- DataQuan[,2:ncol(DataQuan)]

rownames(Dat) <- DataQuan[,1]

Resp <- FA(Dat, Method = "PC", Type = 2, NFactor = 3)

Titles = c("Scree-plot","Scores das observacoes","Cargas Fatoriais","Biplot")

Plot.FA(Resp, Titles, Color = TRUE, LinLab = rep("", nrow(Dat)))

}

\keyword{Analise Fatorial}
\keyword{FA}
