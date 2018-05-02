\name{Regressao}

\alias{Regressao}

\title{Regressao linear.}

\description{Realiza a regressao linear em um conjunto de dados.}

\usage{Regressao(Y, X, NameVarX = NULL, Intercepts = TRUE, SigF = 0.05)}

\arguments{
\item{Y}{Respotas.}
\item{X}{Variaveis regressoras.}
\item{NameVarX}{Nome da variavel, ou variaveis X, se omitido retorna padrao.}
\item{Intercepts}{Considerar o intercepto na regressao (default = TRUE).}
\item{SigF}{Nivel de significancia dos testes dos residuos (default = 5\%).}
}

\value{
\item{Betas}{Coeficientes da regressao.}
\item{CovBetas}{Matriz de covariancias dos coeficientes da regressao.}
\item{ICc}{Intervalo de confianca dos coeficientes da regressao.}
\item{Hip.Test}{Teste de hipoteses dos coeficientes da regressao.}
\item{ANOVA}{Analise de variancia da regressao.}
\item{R}{Coeficiente de determinacao.}
\item{Rc}{Coeficiente de determinacao corrigido.}
\item{Ra}{Coeficiente de determinacao ajustado.}
\item{QME}{Variancia dos residuos.}
\item{ICQME}{Intervalo de confianca da variancia dos residuos.}
\item{Prev}{Previsao do ajuste da regressao.}
\item{IPp}{Intervalo das previsoes.}
\item{ICp}{Intervalo de confianca das previsoes.}
\item{Error}{Residuos do ajuste da regressao.}
\item{Error.Test}{Retorna a 5\% de significancia o teste de independencia, de   normalidade e de homogeneidade da variancia dos residuos.}
}

\references{
CHARNET, R.; at al.. \emph{Analise de modelos de regressao lienar,} 2a ed. Campinas: Editora da Unicamp, 2008. 357 p.

RENCHER, A. C. \emph{Methods of Multivariate Analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.
}

\author{
Paulo Cesar Ossani
}

\seealso{\code{\link{Plot.Regressao}}}

\examples{
data(DataMix)

Y <- DataMix[,2]

X <- DataMix[,6:7]

NomeY <- "Medias notas"

NomeX <- c("Cafes Expeciais", "Cafes Comerciais")

Res <- Regressao(Y, X, NameVarX = NomeX , Intercepts = TRUE, SigF = 0.05)

print("Coeficientes da Regressao:"); round(Res$Betas,4)
print("Analise de Variancia"); Res$ANOVA
print("Teste de Hipoteses dos Coeficientes da Regressao:"); round(Res$Hip.Test,4)
print("Coeficiente de Determinacao:"); round(Res$R,4)
print("Coeficiente de Determinacao Corrigido:"); round(Res$Rc,4) 
print("Coeficiente de Determinacao Ajustado:"); round(Res$Ra,4)
print("Testes dos Residuos:"); Res$Error.Test
}

\keyword{Regressao}
