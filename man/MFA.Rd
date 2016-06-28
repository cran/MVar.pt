\name{MFA}

\alias{MFA}

\title{Analise de multiplos fatores (MFA).}

\description{Realiza analise de multiplos fatores (MFA) em grupos de variaveis. Os grupos de variaveis podem ser dados quantitativos, qualitativos, de frequencia (MFACT), ou dados mistos.}

\usage{MFA(Data, Grupo, TipoGrupo = rep("n", length(Grupo)), NomeGrupos = NULL)}
\arguments{
  \item{Data}{Dados a serem analisados.}
  \item{Grupo}{Numero de colunas para cada grupo em ordem seguindo a ordem dos dados de "Data".}
  \item{TipoGrupo}{"n" para dados numericos (default),\cr
                   "c" para dados categoricos,\cr
                   "f" para dados de frequencia.}
  \item{NomeGrupos}{Nomes para cada grupo.}
}

\value{
\item{MatrixG}{Matriz com os tamanhos de cada grupo.}
\item{MatrixNG}{Matriz com os nomes de cada grupo.}
\item{MatrixPLin}{Matriz com os valores usados para balancear as linhas da matriz Z.}
\item{MatrixPCol}{Matriz com os valores usados para balancear as colunas da matriz Z.}
\item{MatrixZ}{Matriz concatenada e balanceada.}
\item{MatrixA}{Matriz com autovalores (variancias).}
\item{MatrixU}{Matriz U da decomposicao singular da matriz Z.}
\item{MatrixV}{Matriz V da decomposicao singular da matriz Z.}
\item{MatrixF}{Matriz global dos escores dos fatores onde as linhas sao as observacoes e as colunas os componentes.}
\item{MatrixEFG}{Matriz dos escores dos fatores por grupo.}
\item{MatrixCCP}{Matriz de correlacao dos componentes principais com as variaveis originais.}
\item{MatrixEscVar}{Matriz das inercias parciais/escores das variareis.}
}

\references{
ABDESSEMED, L. and ESCOFIER, B.; Analyse factorielle multiple de tableaux de frequencies: comparaison avec l'analyse canonique des correspondences. \emph{Journal de la Societe de Statistique de Paris}, Paris, v. 137, n. 2, p. 3-18, 1996..

ABDI, H. Singular Value Decomposition (SVD) and Generalized Singular Value Decomposition (GSVD). In: SALKIND, N. J. (Ed.). \emph{Encyclopedia of measurement and statistics.} Thousand Oaks: Sage, 2007. p. 907-912.

ABDI, H.; VALENTIN, D. Multiple factor analysis (MFA). In: SALKIND, N. J. (Ed.). \emph{Encyclopedia of measurement and statistics.} Thousand Oaks: Sage, 2007. p. 657-663.

ABDI, H.; WILLIAMS, L. Principal component analysis. \emph{WIREs Computational Statatistics}, New York, v. 2, n. 4, p. 433-459, July/Aug. 2010.

ABDI, H.; WILLIAMS, L.; VALENTIN, D. Multiple factor analysis: principal component analysis for multitable and multiblock data sets. \emph{WIREs Computational Statatistics}, New York, v. 5, n. 2, p. 149-179, Feb. 2013.


BECUE-BERTAUT, M.; PAGES, J. A principal axes method for comparing contingency tables: MFACT. \emph{Computational Statistics & Data Analysis}, New York, v. 45, n. 3, p. 481-503, Feb. 2004

BECUE-BERTAUT, M.; PAGES, J. Multiple factor analysis and clustering of a mixture of quantitative, categorical and frequency data. \emph{Computational Statistics & Data Analysis}, New York, v. 52, n. 6, p. 3255-3268, Feb. 2008.

BENZECRI, J. Analyse de l'inertie intraclasse par l'analyse d'un tableau de contingence: intra-classinertia analysis through the analysis of a contingency table. \emph{Les Cahiers de l'Analyse des Donnees}, Paris, v. 8, n. 3, p. 351-358, 1983.

ESCOFIER, B. Analyse factorielle en reference a un modele: application a l'analyse d'un tableau d'echanges. \emph{Revue de Statistique Appliquee}, Paris, v. 32, n. 4, p. 25-36, 1984.

ESCOFIER, B.; DROUET, D. Analyse des differences entre plusieurs tableaux de frequence. \emph{Les Cahiers de l'Analyse des Donnees}, Paris, v. 8, n. 4, p. 491-499, 1983.

ESCOFIER, B.; PAGES, J. \emph{Analyse factorielles simples et multiples.} Paris: Dunod, 1990. 267 p.

ESCOFIER, B.; PAGES, J. \emph{Analyses factorielles simples et multiples:} objectifs, methodes et interpretation. 4th ed. Paris: Dunod, 2008. 318 p.

ESCOFIER, B.; PAGES, J. \emph{Comparaison de groupes de variables definies sur le meme ensemble d'individus:} un exemple d'applications. Le Chesnay: Institut National de Recherche en Informatique et en Automatique, 1982. 121 p.

ESCOFIER, B.; PAGES, J. Multiple factor analysis (AFUMULT package). \emph{Computational Statistics & Data Analysis}, New York, v. 18, n. 1, p. 121-140, Aug. 1994

GREENACRE, M.; BLASIUS, J. \emph{Multiple correspondence analysis and related methods.} New York: Taylor and Francis, 2006. 607 p.

PAGES, J. Analyse factorielle multiple appliquee aux variables qualitatives et aux donnees mixtes. \emph{Revue de Statistique Appliquee}, Paris, v. 50, n. 4, p. 5-37, 2002.

PAGES, J. Multiple factor analysis: main features and application to sensory data. \emph{Revista Colombiana de Estadistica}, Bogota, v. 27, n. 1, p. 1-26, 2004.

}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{Plot.MFA}}}

\examples{
data(DataMix) # conjunto de dados mistos

Dat <- DataMix[,2:ncol(DataMix)] 

rownames(Dat) <- DataMix[1:nrow(DataMix),1]

GroupNames = c("Notas Cafes/Trabalho", "Formacao/Dedicacao", "Cafes")

MF <- MFA(Dat, c(2,2,2), TipoGrupo = c("n","c","f"), GroupNames) # realiza MFA

print("Variancias dos Componentes Principais:");
round(MF$MatrixA,2)

print("Matriz das Inercias Parciais/Escores das Variaveis:");
round(MF$MatrixEscVar,2)
}

\keyword{Analise de multiplos fatores}
\keyword{MFA}
\keyword{MFACT}

