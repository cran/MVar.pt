\name{Biplot}

\alias{Biplot}

\title{Grafico Biplot.}

\description{Realiza o grafico Biplot.}

\usage{Biplot(data, alpha = 0.5, title = NA, xlabel = NA, ylabel = NA,
       size = 1.1, grid = TRUE, color = TRUE, var = TRUE,
       obs = TRUE, linlab = NA, class = NA, classcolor = NA,
       posleg = 2, boxleg = TRUE, axes = TRUE, savptc = FALSE, 
       width = 3236, height = 2000, res = 300)}

\arguments{
\item{data}{Dados para plotagem.}
\item{alpha}{Representatividade dos individuos (alpha),
            representatividade das variaveis (1 - alpha).
            Sendo 0.5 o default.}
\item{title}{Titulo para o grafico, se nao for definido assume texto padrao.}
\item{xlabel}{Nomeia o eixo X, se nao for definido assume texto padrao.}
\item{ylabel}{Nomeia o eixo Y, se nao for definido assume texto padrao.}
\item{size}{Tamanho dos pontos no grafico.}
\item{grid}{Coloca grade nos graficos (default = TRUE).}
\item{color}{Graficos coloridos (default = TRUE).}
\item{var}{Acrescenta as projecoes das variaveis ao grafico (default = TRUE).}
\item{obs}{Acrescenta as observacoes ao grafico (default = TRUE).}
\item{linlab}{Vetor com os rotulos para as observacoes.}
\item{class}{Vetor com os nomes das classes dos dados.}
\item{classcolor}{Vetor com as cores das classes.}
\item{posleg}{0 sem legenda,\cr
              1 para legenda no canto superior esquerdo,\cr
              2 para legenda no canto superior direito (default),\cr
              3 para legenda no canto inferior direito,\cr
              4 para legenda no canto inferior esquerdo.}
\item{boxleg}{Coloca moldura na legenda (default = TRUE).}
\item{axes    }{Plota os eixos X e Y (default = TRUE).}
\item{savptc}{Salva as imagens dos graficos em arquivos (default = FALSE).}   
\item{width}{Largura do grafico quanto savptc = TRUE (defaul = 3236).}
\item{height}{Altura do grafico quanto savptc = TRUE (default = 2000).}
\item{res}{Resolucao nominal em ppi do grafico quanto savptc = TRUE (default = 300).}
}

\value{
\item{Biplot}{Grafico Biplot.}
\item{Md}{Matriz autovalores.}
\item{Mu}{Matriz U (autovetores).}
\item{Mv}{Matriz V (autovetores).}
\item{coorI}{Coordenadas dos individuos.}
\item{coorV}{Coordenadas das variaveis.}
\item{pvar}{Proporcao dos componentes principais.}
}

\references{
Rencher, A. C. \emph{Methods of multivariate analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data(iris) # conjunto de dados

data <- iris[,1:4]

Biplot(data)

cls <- iris[,5]

res <- Biplot(data, alpha = 0.6, title = "Biplot dos Dados\n valorizando os individuos",
              class = cls, classcolor = c("goldenrod3","gray56","red"),
              posleg = 2, boxleg = FALSE, axes = TRUE, savptc = FALSE, 
              width = 3236, height = 2000, res = 300)
print(res$pvar)


res <- Biplot(data, alpha = 0.4, title = "Grafico valorizando as variaveis",
              xlabel = "", ylabel = "", color = FALSE, obs = FALSE, 
              savptc = FALSE, width = 3236, height = 2000, res = 300) 
print(res$pvar)       
       
}

\keyword{Biplot}
