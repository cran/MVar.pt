\name{Plot.FA}

\alias{Plot.FA}

\title{Graficos da analise fatorial (FA).}

\description{Graficos da analise fatorial (FA).}

\usage{Plot.FA(FA, titles = NA, xlabel = NA, ylabel = NA,
        size = 1.1, grid = TRUE, color = TRUE, linlab = NA,
        savptc = FALSE, width = 3236, height = 2000, res = 300,
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
\item{savptc}{Salva as imagens dos graficos em arquivos (default = FALSE).}   
\item{width}{Largura do grafico quanto savptc = TRUE (defaul = 3236).}
\item{height}{Altura do grafico quanto savptc = TRUE (default = 2000).}
\item{res}{Resolucao nominal em ppi do grafico quanto savptc = TRUE (default = 300).}
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

data <- DataQuan[,2:ncol(DataQuan)]

rownames(data) <- DataQuan[,1]

res <- FA(data, method = "PC", type = 2, nfactor = 3)

tit <- c("Scree-plot","Scores das observacoes","Cargas Fatoriais","Biplot")

Plot.FA(res, titles = tit, xlabel = NA, ylabel = NA,
        color = TRUE, linlab = rep("", nrow(data)),
        savptc = FALSE, width = 3236, height = 2000, 
        res = 300, casc = TRUE)

}

\keyword{Analise Fatorial}
\keyword{FA}
