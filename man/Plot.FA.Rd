\name{Plot.FA}

\alias{Plot.FA}

\title{Graficos da analise fatorial (FA).}

\description{Graficos da analise fatorial (FA).}

\usage{Plot.FA(FA, Titles = NA, xlabel = NA, ylabel = NA,
        Color = TRUE, LinLab = NA, Casc = TRUE)}

\arguments{
\item{FA}{Dados da funcao FA.}
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

\seealso{\code{\link{FA}}}

\examples{
data(DataQuan) # conjunto de dados

Data <- DataQuan[,2:ncol(DataQuan)]

rownames(Data) <- DataQuan[,1]

Resp <- FA(Data, Method = "PC", Type = 2, NFactor = 3)

Tit = c("Scree-plot","Scores das observacoes","Cargas Fatoriais","Biplot")

Plot.FA(Resp, Titles = Tit, xlabel = NA, ylabel = NA,
        Color = TRUE, LinLab = rep("", nrow(Data)),
        Casc = TRUE)

}

\keyword{Analise Fatorial}
\keyword{FA}
