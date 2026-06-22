\name{Plot.CI}

\alias{Plot.CI}

\title{Grafico para Intervalos de Confiança Uni e Multivariado.}

\description{Grafico para Intervalos de Confiança Uni e Multivariado.}

\usage{Plot.CI(ci, title = NA, xlabel = NA, ylabel = NA, size = 0.9, 
        grid = TRUE, color = TRUE, savptc = FALSE, width = 3236,
        height = 2000, res = 300, casc = FALSE)}

\arguments{
\item{ci}{Dados da funcao CI, ou com a mesma estrutura.}
\item{title}{Titulo para o grafico, se nao for definido assume texto padrao.}
\item{xlabel}{Nomeia o eixo X, se nao for definido assume texto padrao.}
\item{ylabel}{Nomeia o eixo Y, se nao for definido assume texto padrao.}
\item{size}{Tamanho dos pontos no grafico.}
\item{grid}{Coloca grade no grafico (default = TRUE).}
\item{color}{Grafico colorido (default = TRUE).}
\item{savptc}{Salva a imagen do grafico em arquivo (default = FALSE).}   
\item{width}{Largura do grafico quanto savptc = TRUE (defaul = 3236).}
\item{height}{Altura do grafico quanto savptc = TRUE (default = 2000).}
\item{res}{Resolucao nominal em ppi do grafico quanto savptc = TRUE (default = 300).}
\item{casc}{Efeito cascata na apresentacao dos graficos (default = FALSE).}
}

\value{Retorna grafico.}

\author{
Paulo Cesar Ossani
}

\seealso{\code{\link{CI}}}

\examples{
data(iris) # conjunto de dados

res <- CI(data = iris[,1:4], sigma = NA, conf = 0.95, type = "T")

tit <- "Intervalo de Conficanca\n Univariado"

Plot.CI(ci = res$ciu$CIu, title = tit, xlabel = NA, ylabel = '', 
        size = 1, grid = TRUE, color = TRUE, savptc = FALSE,
        width = 3236, height = 2000, res = 300, casc = FALSE) 
        
        
tit <- "Intervalo de Conficanca\n Multivariado"

Plot.CI(ci = res$cim$CIm, title = tit, xlabel = NA, ylabel = '', 
        size = 1, grid = TRUE, color = TRUE, savptc = FALSE,
        width = 3236, height = 2000, res = 300, casc = FALSE) 

}

\keyword{Intervalos de Confianca}
\keyword{CI}
