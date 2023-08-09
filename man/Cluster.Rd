\name{Cluster}

\alias{Cluster}

\title{analysis de cluster.}

\description{Realiza analysis de cluster hierarquico e nao hierarquico em um conjunto de dados.}

\usage{Cluster(data, titles = NA, hierarquic = TRUE, analysis = "Obs",  
        cor.abs = FALSE, normalize = FALSE, distance = "euclidean",  
        method = "complete", horizontal = FALSE, num.groups = 0,
        lambda = 2, savptc = FALSE, width = 3236, height = 2000, 
        res = 300, casc = TRUE) }
        
\arguments{
\item{data}{Dados a serem analizados.}
\item{titles}{Titulos para os graficos, se nao for definido assume texto padrao.}
\item{hierarquic}{Agrupamentos hierarquicos (default = TRUE), para agrupamentos nao hierarquicos (method K-Means), somente para caso 'analysis' = "Obs".}
\item{analysis}{"Obs" para analises nas observacoes (default), "Var" para analises nas variaveis.}
\item{cor.abs}{Matriz de correlacao absoluta caso 'analysis' = "Var" (default = FALSE).}
\item{normalize}{Normaliza os dados somente para caso 'analysis' = "Obs" (default = FALSE).}
\item{distance}{Metrica das distancias caso agrupamentos hierarquicos: "euclidean" (default), "maximum", "manhattan", "canberra", "binary" ou "minkowski". Caso analysis = "Var" a metrica sera a matriz de correlacao, conforme cor.abs.}
\item{method}{Metodo para analises caso agrupamentos hierarquicos: "complete" (default), "ward.D", "ward.D2", "single", "average", "mcquitty", "median" ou "centroid".}
\item{horizontal}{Dendrograma na horizontal (default = FALSE).}
\item{num.groups}{Numero de grupos a formar.}
\item{lambda}{Valor usado na distancia de minkowski.}
\item{savptc}{Salva as imagens dos graficos em arquivos (default = FALSE).}   
\item{width}{Largura do grafico quanto savptc = TRUE (defaul = 3236).}
\item{height}{Altura do grafico quanto savptc = TRUE (default = 2000).}
\item{res}{Resolucao nominal em ppi do grafico quanto savptc = TRUE (default = 300).}
\item{casc}{Efeito cascata na apresentacao dos graficos (default = TRUE).}
}

\value{{Varios graficos.}
\item{tab.res}{Tabela com as similaridades e distancias dos grupos formados.}
\item{groups}{Dados originais com os grupos formados.}
\item{res.groups}{Resultados dos grupos formados.}
\item{R.sqt}{Resultado do R quadrado.}
\item{sum.sqt}{Soma do quadrado total.}
\item{mtx.dist}{Matriz das distancias.}
}

\references{
MINGOTI, S. A. \emph{analysis de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

FERREIRA, D. F. \emph{Estatistica Multivariada.} 2a ed. revisada e ampliada. Lavras: Editora UFLA, 2011. 676 p.

RENCHER, A. C. \emph{Methods of multivariate analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data(DataQuan) # conjunto de dados quantitativos

data <- DataQuan[,2:8]

rownames(data) <- DataQuan[1:nrow(DataQuan),1]

res <- Cluster(data, titles = NA, hierarquic = TRUE, analysis = "Obs",
               cor.abs = FALSE, normalize = FALSE, distance = "euclidean", 
               method = "ward.D", horizontal = FALSE, num.groups = 2,
               savptc = FALSE, width = 3236, height = 2000, res = 300, 
               casc = FALSE)

print("R quadrado:"); res$R.sqt
# print("Soma do quadrado total:"); res$sum.sqt
print("Grupos formados:"); res$groups
# print("Tabela com as similaridades e distancias:"); res$tab.res
# print("Tabela com os resultados dos grupos:"); res$res.groups
# print("Matriz de distancias:"); res$mtx.dist 

write.table(file=file.path(tempdir(),"SimilarityTable.csv"), res$tab.res, sep=";",
            dec=",",row.names = FALSE) 
write.table(file=file.path(tempdir(),"GroupData.csv"), res$groups, sep=";",
            dec=",",row.names = TRUE) 
write.table(file=file.path(tempdir(),"GroupResults.csv"), res$res.groups, sep=";",
            dec=",",row.names = TRUE) 
}

\keyword{analysis de Cluster}
\keyword{Cluster}
