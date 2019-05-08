\name{DA}

\alias{DA}

\title{Analise discriminante linear (LDA) e quadratica (QDA).}

\description{Realiza analise discriminante linear e quadratica.}

\usage{DA(Data, Class = NA, Type = "lda", Validation = "Learning", 
   Method = "moment", Prior = NA, Testing = NA)}

\arguments{
  \item{Data}{Dados a serem a classificados.}
  \item{Class}{Vetor com os nomes das classes dos dados.}
  \item{Type}{"lda": analise discriminante linear (default), ou \cr
              "qda": analise discriminante quadratica.}
  \item{Validation}{Tipo de validacao:\cr
                    "Learning" - treinamento dos dados (default), ou\cr
                    "Testing" - classifica os dados do vetor "Testing".}
  \item{Method}{Metodo de classificacao:\cr 
                "mle" para MLEs,\cr
                "mve" para usar cov.mv, \cr
                "moment" (default) para estimadores padrao da media e variancia, ou \cr
                "t" para estimativas robustas baseadas em uma distribuicao t.}
  \item{Prior}{Probabilidades de ocorrencia das classes. Se nao especificado, tomara as proporcoes das classes. Se especificado, as probabilidades devem seguir a ordem dos niveis dos fatores.}
  \item{Testing}{Vetor com os indices que serao utilizados em Data como teste. Para Validation = "Learning", tem-se Testing = NA.}
}

\value{
\item{Confusion}{Tabela de confusao.}
\item{Error.rate}{Proporcao global de erro.}
\item{Prior}{Probabilidade das classes.}
\item{Type}{Tipo de analise discriminante.}
\item{Validation}{Tipo de validacao.}
\item{Num.Class}{Numero de classes.}
\item{Class.Names}{Nomes das classes}
\item{Method}{Metodo de classificacao.}
\item{Num.Correct}{Numero de observacoes corretas.}
\item{Results}{Matriz com resultados comparativos das classificacoes.}
}

\references{
FERREIRA, D. F. \emph{Estatistica Multivariada.} 2a ed. revisada e ampliada. Lavras: Editora UFLA, 2011. 676 p.

MINGOTI, S. A. \emph{Analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

RENCHER, A. C. \emph{Methods of Multivariate Analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.

RIPLEY, B. D. \emph{Pattern Recognition and Neural Networks.} Cambridge University Press, 1996.

VENABLESs, W. N. and RIPLEY, B. D. \emph{Modern Applied Statistics with S.} Fourth edition. Springer, 2002. 
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data(iris) # conjunto de dados

Data  = iris[,1:4] # dados a serem classificados
Class = iris[,5]   # classe dos dados
Prior = c(1,1,1)/3 # probabilidade a priori das classes

Res <- DA(Data, Class, Type = "lda", Validation = "Learning", 
          Method = "mle", Prior = Prior, Testing = NA)

print("Tabela de confusao:"); Res$Confusion
print("Proporcao global de acerto:"); 1 - Res$Error.rate
print("Probabilidade das classes:"); Res$Prior
print("Metodo de classificacao:"); Res$Method
print("Tipo analise discriminante:"); Res$Type
print("Nomes das classes:"); Res$Class.Names
print("Numero de classes:"); Res$Num.Class
print("Tipo de validacao:"); Res$Validation
print("Numero de observacoes corretas:"); Res$Num.Correct
print("Matriz com os resultados da classificacao:"); Res$Results


### validacao cruzada ###
amostra   = sample(2, nrow(Data), replace = TRUE, prob = c(0.7,0.3))
Datatrain = Data[amostra == 1,] # dados para treino
Datatest  = Data[amostra == 2,] # dados para teste

dim(Datatrain) # dimensao dados treino
dim(Datatest)  # dimensao dados teste

Testing  = as.integer(rownames(Datatest)) # indice dos dados teste

Res <- DA(Data, Class, Type = "qda", Validation = "testing", 
          Method = "moment", Prior = NA, Testing = Testing)

print("Tabela de confusao:"); Res$Confusion
print("Proporcao global de acerto:"); 1 - Res$Error.rate
print("Numero de observacoes corretas:"); Res$Num.Correct
print("Matriz com os resultados da classificacao:"); Res$Results
}

\keyword{Analise discriminante linear e quadratica}
\keyword{LDA}
\keyword{QDA}