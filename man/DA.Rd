\name{DA}

\alias{DA}

\title{Analise discriminante linear (LDA) e quadratica (QDA).}

\description{Realiza analise discriminante linear e quadratica.}

\usage{DA(data, class = NA, type = "lda", validation = "learning", 
   method = "moment", prior = NA, testing = NA)}

\arguments{
  \item{data}{Dados a serem a classificados.}
  \item{class}{Vetor com os nomes das classes dos dados.}
  \item{type}{"lda": analise discriminante linear (default), ou \cr
              "qda": analise discriminante quadratica.}
  \item{validation}{Tipo de validacao:\cr
                    "learning" - treinamento dos dados (default), ou\cr
                    "testing" - classifica os dados do vetor "testing".}
  \item{method}{Metodo de classificacao:\cr 
                "mle" para MLEs,\cr
                "mve" para usar cov.mv, \cr
                "moment" (default) para estimadores padrao da media e variancia, ou \cr
                "t" para estimativas robustas baseadas em uma distribuicao t.}
  \item{prior}{Probabilidades de ocorrencia das classes. Se nao especificado, tomara as proporcoes das classes. Se especificado, as probabilidades devem seguir a ordem dos niveis dos fatores.}
  \item{testing}{Vetor com os indices que serao utilizados em data como teste. Para validation = "learning", tem-se testing = NA.}
}

\value{
\item{confusion}{Tabela de confusao.}
\item{error.rate}{Proporcao global de erro.}
\item{prior}{Probabilidade das classes.}
\item{type}{Tipo de analise discriminante.}
\item{validation}{Tipo de validacao.}
\item{num.class}{Numero de classes.}
\item{class.names}{Nomes das classes}
\item{method}{Metodo de classificacao.}
\item{num.correct}{Numero de observacoes corretas.}
\item{results}{Matriz com resultados comparativos das classificacoes.}
}

\references{
FERREIRA, D. F. \emph{Estatistica Multivariada.} 2a ed. revisada e ampliada. Lavras: Editora UFLA, 2011. 676 p.

MINGOTI, S. A. \emph{Analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

RENCHER, A. C. \emph{Methods of multivariate analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.

RIPLEY, B. D. \emph{Pattern Recognition and Neural Networks.} Cambridge University Press, 1996.

VENABLESs, W. N. and RIPLEY, B. D. \emph{Modern Applied Statistics with S.} Fourth edition. Springer, 2002. 
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data(iris) # conjunto de dados

data  = iris[,1:4] # dados a serem classificados
class = iris[,5]   # classe dos dados
prior = c(1,1,1)/3 # probabilidade a priori das classes

res <- DA(data, class, type = "lda", validation = "learning", 
          method = "mle", prior = prior, testing = NA)

print("Tabela de confusao:"); res$confusion
print("Proporcao global de acerto:"); 1 - res$error.rate
print("Probabilidade das classes:"); res$prior
print("Metodo de classificacao:"); res$method
print("Tipo analise discriminante:"); res$type
print("Nomes das classes:"); res$class.names
print("Numero de classes:"); res$num.class
print("Tipo de validacao:"); res$validation
print("Numero de observacoes corretas:"); res$num.correct
print("Matriz com os resultados da classificacao:"); res$results


### validacao cruzada ###
amostra   = sample(2, nrow(data), replace = TRUE, prob = c(0.7,0.3))
datatrain = data[amostra == 1,] # dados para treino
datatest  = data[amostra == 2,] # dados para teste

dim(datatrain) # dimensao dados treino
dim(datatest)  # dimensao dados teste

testing  = as.integer(rownames(datatest)) # indice dos dados teste

res <- DA(data, class, type = "qda", validation = "testing", 
          method = "moment", prior = NA, testing = testing)

print("Tabela de confusao:"); res$confusion
print("Proporcao global de acerto:"); 1 - res$error.rate
print("Numero de observacoes corretas:"); res$num.correct
print("Matriz com os resultados da classificacao:"); res$results
}

\keyword{Analise discriminante linear e quadratica}
\keyword{LDA}
\keyword{QDA}