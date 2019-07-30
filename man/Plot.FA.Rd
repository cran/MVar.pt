\name{Plot.FA}

\alias{Plot.FA}

\title{Graficos da analise fatorial (FA).}

\description{Graficos da analise fatorial (FA).}

\usage{Plot.FA(FA, titles = NA, xlabel = NA, ylabel = NA,
        size = 1.1, grid = TRUE, color = TRUE, linlab = NA,
        casc = TRUE)}

\arguments{
\item{FA}{Dados da funcao FA.}
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

\seealso{\code{\link{FA}}}

\examples{
data(DataQuan) # conjunto de dados

Data <- DataQuan[,2:ncol(DataQuan)]

rownames(Data) <- DataQuan[,1]

Resp <- FA(Data, method = "PC", type = 2, nfactor = 3)

Tit = c("Scree-plot","Scores das observacoes","Cargas Fatoriais","Biplot")

Plot.FA(Resp, titles = Tit, xlabel = NA, ylabel = NA,
        color = TRUE, linlab = rep("", nrow(Data)),
        casc = TRUE)

}

\keyword{Analise Fatorial}
\keyword{FA}
