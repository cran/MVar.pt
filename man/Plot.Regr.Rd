\name{Plot.Regr}

\alias{Plot.Regr}

\title{Graficos dos resultados da regressao linear.}

\description{Graficos dos resultados da regressao linear.}

\usage{Plot.Regr(Reg, TypeGraf = "Scatterplot", Title = NA,
          xlabel = NA, ylabel = NA, NameVarY = NA, 
          NameVarX = NA, Color = TRUE, IntConf = TRUE, 
          IntPrev = TRUE, Casc = TRUE)}

\arguments{
\item{Reg}{Dados da funcao de regressao.}
\item{TypeGraf}{Tipo de grafico:\cr
                "Scatterplot" - Grafico de dispersao 2 a 2,\cr
                "Regression"  - Grafico da regressao linear,\cr
                "QQPlot"      - Grafico de probabilidade normal dos residuos,\cr
                "Histogram"   - Histograma dos residuos,\cr
                "Fits"        - Grafico dos valores ajustados versus os residuos,\cr
                "Order"       - Grafico da ordem das observacoes versus os residuos.}
\item{Title}{Titulos para os graficos, se nao for definido assume texto padrao.}
\item{xlabel}{Nomeia o eixo X, se nao for definido assume texto padrao.}
\item{ylabel}{Nomeia o eixo Y, se nao for definido assume texto padrao.}
\item{NameVarY}{Nome da variavel Y, se nao for definido assume texto padrao.}
\item{NameVarX}{Nome da variavel, ou variaveis X, se nao for definido assume texto padrao.}
\item{Color}{Graficos coloridos (default = TRUE).}
\item{IntConf}{Caso TypeGraf = "Regression". Graficos com intervalo de confianca (default = TRUE). }
\item{IntPrev}{Caso TypeGraf = "Regression". Graficos com intervalo de previsao (default = TRUE)}
\item{Casc}{Efeito cascata na apresentacao dos graficos (default = TRUE).}
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

NomeY <- "Media das notas"

NomeX <- "Cafes comerciais"

Resp <- Regr(Y, X, NameVarX = NomeX ,Intercepts = TRUE, SigF = 0.05)

Tit <- c("Scatterplot")
Plot.Regr(Resp, TypeGraf = "Scatterplot", Title = Tit,
          NameVarY = NomeY, NameVarX = NomeX, Color = TRUE)

Tit <- c("Grafico de Dispersao com a \n reta ajustada")
Plot.Regr(Resp, TypeGraf = "Regression", Title = Tit, 
          xlabel = NomeX, ylabel = NomeY, Color = TRUE,
          IntConf = TRUE, IntPrev = TRUE)

dev.new() # necessario para nao sobrepor os graficos seguintes ao grafico anterior

par(mfrow = c(2,2)) 

Plot.Regr(Resp, TypeGraf = "QQPlot", Casc = FALSE)
Plot.Regr(Resp, TypeGraf = "Histogram", Casc = FALSE)
Plot.Regr(Resp, TypeGraf = "Fits", Casc = FALSE)
Plot.Regr(Resp, TypeGraf = "Order", Casc = FALSE)
}

\keyword{Regressao}
