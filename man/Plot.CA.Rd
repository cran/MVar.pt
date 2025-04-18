\name{Plot.CA}

\alias{Plot.CA}

\title{Graficos da analise de correspondencia (CA) simples e multipla.}

\description{Graficos da analise de correspondencia (CA) simples e multipla.}

\usage{Plot.CA(CA, titles = NA, xlabel = NA, ylabel = NA, size = 1.1, 
        grid = TRUE, color = TRUE, linlab = NA, axes = TRUE,
        savptc = FALSE, width = 3236, height = 2000, res = 300, 
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
\item{axes}{Plota os eixos X e Y (default = TRUE).}
\item{savptc}{Salva as imagens dos graficos em arquivos (default = FALSE).}   
\item{width}{Largura do grafico quanto savptc = TRUE (defaul = 3236).}
\item{height}{Altura do grafico quanto savptc = TRUE (default = 2000).}
\item{res}{Resolucaoo nominal em ppi do grafico quanto savptc = TRUE (default = 300).}
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

data <- DataFreq[,2:ncol(DataFreq)]

rownames(data) <- DataFreq[1:nrow(DataFreq),1]

res <- CA(data, "f") # realiza CA

tit <- c("Scree-plot","Observacoes", "Variaveis", "Observacoes/Variaveis")

Plot.CA(res, titles = tit, xlabel = NA, ylabel = NA, axes = TRUE,
        color = TRUE, linlab = rownames(data), savptc = FALSE,
        width = 3236, height = 2000, res = 300, casc = FALSE)


data(DataQuali) # conjunto de dados qualitativos

data <- DataQuali[,2:ncol(DataQuali)]

res <- CA(data, "c", "b") # realiza CA

tit <- c("","","Grafico das Variaveis")

Plot.CA(res, titles = tit, xlabel = NA, ylabel = NA,
        color = TRUE, linlab = NA, savptc = FALSE, 
        width = 3236, height = 2000, res = 300, 
        axes = TRUE, casc = FALSE)

}

\keyword{Analise de correspondencia}
\keyword{CA}
