\name{Plot.CA}

\alias{Plot.CA}

\title{Graficos da analise de correspondencia (CA) simples e multipla.}

\description{Graficos da analise de correspondencia (CA) simples e multipla.}

\usage{Plot.CA(CA, titles = NA, xlabel = NA, ylabel = NA,
        size = 1.1, grid = TRUE, color = TRUE, linlab = NA, 
        casc = TRUE)}

\arguments{
\item{CA}{Dados da funcao CA.}
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

\seealso{\code{\link{CA}}}

\examples{
data(DataFreq) # conjunto de dados de frequencia

Data <- DataFreq[,2:ncol(DataFreq)]

rownames(Data) <- DataFreq[1:nrow(DataFreq),1]

Resp <- CA(Data, "f") # realiza CA

Tit = c("Scree-plot","Observacoes", "Variaveis", "Observacoes/Variaveis")

Plot.CA(Resp, titles = Tit, xlabel = NA, ylabel = NA,
        color = TRUE, linlab = rownames(Data), casc = FALSE)


data(DataQuali) # conjunto de dados qualitativos

Data <- DataQuali[,2:ncol(DataQuali)]

Resp <- CA(Data, "c", "b") # realiza CA

Tit = c("","","Grafico das Variaveis")

Plot.CA(Resp, titles = Tit, xlabel = NA, ylabel = NA,
        color = TRUE, linlab = NA, casc = FALSE)

}

\keyword{Analise de correspondencia}
\keyword{CA}
