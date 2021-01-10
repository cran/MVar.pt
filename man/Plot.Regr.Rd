\name{Plot.Regr}

\alias{Plot.Regr}

\title{Graficos dos resultados da regressao linear.}

\description{Graficos dos resultados da regressao linear.}

\usage{Plot.Regr(Reg, typegraf = "Scatterplot", title = NA, xlabel = NA, 
          ylabel = NA, namevary = NA, namevarx = NA, size = 1.1, 
          grid = TRUE, color = TRUE, intconf = TRUE, intprev = TRUE, 
          savptc = FALSE, width = 3236, height = 2000, res = 300,
          casc = TRUE)}

\arguments{
\item{Reg}{Dados da funcao de regressao.}
\item{typegraf}{Tipo de grafico:\cr
                "Scatterplot" - Grafico de dispersao 2 a 2,\cr
                "Regression"  - Grafico da regressao linear,\cr
                "QQPlot"      - Grafico de probabilidade normal dos residuos,\cr
                "Histogram"   - Histograma dos residuos,\cr
                "Fits"        - Grafico dos valores ajustados versus os residuos,\cr
                "Order"       - Grafico da ordem das observacoes versus os residuos.}
\item{title}{Titulos para os graficos, se nao for definido assume texto padrao.}
\item{xlabel}{Nomeia o eixo X, se nao for definido assume texto padrao.}
\item{ylabel}{Nomeia o eixo Y, se nao for definido assume texto padrao.}
\item{namevary}{Nome da variavel Y, se nao for definido assume texto padrao.}
\item{namevarx}{Nome da variavel X, ou variaveis X, se nao for definido assume texto padrao.}
\item{size}{Tamanho dos pontos no grafico.}
\item{grid}{Coloca grade nos graficos (default = TRUE).}
\item{color}{Graficos coloridos (default = TRUE).}
\item{intconf}{Caso typegraf = "Regression". Graficos com intervalo de confianca (default = TRUE).}
\item{intprev}{Caso typegraf = "Regression". Graficos com intervalo de previsao (default = TRUE)}
\item{savptc}{Salva as imagens dos graficos em arquivos (default = FALSE).}   
\item{width}{Largura do grafico quanto savptc = TRUE (defaul = 3236).}
\item{height}{Altura do grafico quanto savptc = TRUE (default = 2000).}
\item{res}{Resolucao nominal em ppi do grafico quanto savptc = TRUE (default = 300).}
\item{casc}{Efeito cascata na apresentacao dos graficos (default = TRUE).}
}

\value{Retorna varios graficos.}

\author{
Paulo Cesar Ossani
}

\seealso{\code{\link{Regr}}}

\examples{
data(DataMix)

Y <- DataMix[,2]

X <- DataMix[,7]

name.y <- "Media das notas"

name.x <- "Cafes comerciais"

res <- Regr(Y, X, namevarx = name.x ,intercepts = TRUE, sigf = 0.05)

tit <- c("Scatterplot")
Plot.Regr(res, typegraf = "Scatterplot", title = tit,
          namevary = name.y, namevarx = name.x, color = TRUE, 
          savptc = FALSE, width = 3236, height = 2000, res = 300)

tit <- c("Grafico de Dispersao com a \n reta ajustada")
Plot.Regr(res, typegraf = "Regression", title = tit, 
          xlabel = name.x, ylabel = name.y, color = TRUE,
          intconf = TRUE, intprev = TRUE, savptc = FALSE, 
          width = 3236, height = 2000, res = 300)

dev.new() # necessario para nao sobrepor os graficos seguintes ao grafico anterior

par(mfrow = c(2,2)) 

Plot.Regr(res, typegraf = "QQPlot", casc = FALSE)
Plot.Regr(res, typegraf = "Histogram", casc = FALSE)
Plot.Regr(res, typegraf = "Fits", casc = FALSE)
Plot.Regr(res, typegraf = "Order", casc = FALSE)
}

\keyword{Regressao}
