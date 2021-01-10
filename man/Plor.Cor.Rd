\name{Plot.Cor}

\alias{Plot.Cor}

\title{Grafico das correlacoes entre as variaveis.}

\description{Realiza as correlacoes entre as variaveis de uma base de dados e apresenta em forma de grafico.}

\usage{Plot.Cor(data, title = NA, grid = TRUE, leg = TRUE, boxleg = FALSE, 
         text = FALSE, arrow = TRUE, color = TRUE, namesvar = NA,
         savptc = FALSE, width = 3236, height = 2000, res = 300)}
          

\arguments{
\item{data}{Conjunto de dados numericos.}
\item{title}{Titulo para o grafico, se nao for definido assume texto padrao.}
\item{grid}{Coloca grade no grafico (default = TRUE).}
\item{leg}{Coloca a legenda no grafico (default = TRUE)}
\item{boxleg}{Coloca moldura na legenda (default = FALSE).}
\item{text}{Coloca os valores das correlacoes nos circulos (default = FALSE).}
\item{arrow}{Setas das correlacoes positivas (para cima) e negativas (para baixo) (default = TRUE).}
\item{color}{Graficos coloridos (default = TRUE).}
\item{namesvar}{Vetor com os nomes das variaveis, se omitido assume os nomes em 'data'.}
\item{savptc}{Salva as imagens dos graficos em arquivos (default = FALSE).}   
\item{width}{Largura do grafico quanto savptc = TRUE (defaul = 3236).}
\item{height}{Altura do grafico quanto savptc = TRUE (default = 2000).}
\item{res}{Resolucao nominal em ppi do grafico quanto savptc = TRUE (default = 300).}
}

\value{{Grafico com as correlacoes entre as variaveis em 'data'}
}


\author{
Paulo Cesar Ossani
}

\examples{
data(iris) # conjunto de dados

Plot.Cor(data = iris[,1:4], title = NA, grid = TRUE, leg = TRUE, boxleg = FALSE, 
         text = FALSE, arrow = TRUE, color = TRUE, namesvar = NA, savptc = FALSE, 
         width = 3236, height = 2000, res = 300)
         
Plot.Cor(data = iris[,1:4], title = NA, grid = TRUE, leg = TRUE, boxleg = FALSE, 
         text = TRUE, arrow = TRUE, color = TRUE, namesvar = c("A1","B2","C3","D4"),
         savptc = FALSE, width = 3236, height = 2000, res = 300)
         
}

\keyword{Plot.Cor}
