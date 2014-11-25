\name{MFA}

\alias{MFA}

\title{Analise de Multiplos Fatores (MFA)}

\description{Realiza Analise de Multiplos Fatores (MFA) em grupos de variaveis. Os grupos de variaveis podem ser dados Quantitativos, Qualitativos, de Frequencia (MFACT) ou dados Mistos.}

\usage{MFA(Data, Grupo, TipoGrupo = rep("n", length(Grupo)), NomeGrupos = NULL)}
\arguments{
  \item{Data}{Dados a serem analisados}
  \item{Grupo}{Numero de colunas para cada Grupo em ordem seguindo a ordem dos Dados de 'Data'}
  \item{TipoGrupo}{"n" para dados Numericos - default\cr
                   "c" para dados Categoricos\cr
                   "f" para dados de Frequencia}
  \item{NomeGrupos}{Nomes para cada Grupo}
}

\value{
\item{MatrixG}{Matriz com os tamanhos de cada grupo}
\item{MatrixNG}{Matriz com os nomes de cada grupo}
\item{MatrixPLin}{Matriz com os valores usados para balancear as linhas da Matriz Z}
\item{MatrixPCol}{Matriz com os valores usados para balancear as colunas da Matriz Z}
\item{MatrixZ}{Matriz Concatenada e Balanceada}
\item{MatrixA}{Matriz com Autovalores (Variancias)}
\item{MatrixU}{Matriz U da decomposicao singular da Matriz Z}
\item{MatrixV}{Matriz V da decomposicao singular da Matriz Z}
\item{MatrixF}{Matriz Global dos Escores dos Fatores onde cada linhas sao as observacoes e as colunas os componentes}
\item{MatrixEFG}{Matriz dos Escores dos Fatores por Grupo}
\item{MatrixCCP}{Matriz de Correlacao dos Componentes Principais com as Variaveis Originais}
\item{MatrixEscVar}{Matriz das Inercias Parciais/Escores das Variareis}
}

\references{
ABDESSEMED, L. and ESCOFIER, B.; Analyse factorielle multiple de tableaux de frequencies: comparaison avec l'analyse canonique des correspondences. \emph{Journal de la Societe de Statistique de Paris}, 137, p. 3-18, 1996.

ABDI, H; Singular Value Decomposition (SVD) and Generalized Singular Value Decomposition (GSVD). In:Salkind NJ, \emph{ed. Encyclopedia of Measurement and Statistics}. Thousand Oaks, CA: Sage; 2007.

ABDI, H; VALENTIN, D; Multiple factor analysis (MFA). In:Salkind NJ, \emph{ed. Encyclopedia of Measurement and Statistics}. Thousand Oaks, CA: Sage; 2007, 657-663.

ABDI, H; WILLIAMS, L; Principal Component Analysis.\emph{WIREs Comp Stat} 2010 2 433-459 DOI: 10.1002/wics.101

ABDI, H; WILLIAMS, L; VALENTIN, D; Multiple factor analysis: principal component analysis for multitable and multiblock data sets. \emph{WIREs Comput Stat} 2013. doi: 10.1002/wics.1246

BECUE-BERTAUT, M.; PAGES,J.; A principal axes method for comparing contingency tables: MFACT. \emph{Computational Statistics & Data Analysis}, v. 45, p. 481-503, 2004.

BECUE-BERTAUT, M.; PAGES,J.; Multiple factor analysis and clustering of a mixture of quantitative, categorical and frequency data. \emph{Computational Statistics & Data Analysis}, v. 52, p. 3255-3268, 2008.

ESCOFIER, B,; PAGES,J.; \emph{Analyse Factorielles Simples et Multiples}. Paris: Dunod., 1990.

ESCOFIER, B,; PAGES,J.; \emph{Analyses factorielles simples et multiples: objectifs, methodes et interpretation}. 4th. ed. Paris: Dunod, 2008. 318 p.

ESCOFIER, B,; PAGES,J.; Multiple Factor Analysis (AFUMULT package) \emph{Computational Statistics & Data Analysis}, v. 18, p. 121-140, 1994.

ESCOFIER, B. and DROUET, D.; Analyse des differences entre plusieurs tableaux de frequence. \emph{Les Cahiers de l'Analyse des Donnees},8 n. 4, p. 491-499, 1983.

ESCOFIER, B; Analyse factorielle en reference a un module: application a l'analyse d'un tableau d'echanges. \emph{Revue de Statistique Appliquee}, vol. 32, n. 4, p. 25-36, 1984.

GREENACRE, M.; BLASIUS, J.; \emph{Multiple Correspondence Analysis and Related Methods}. 1th. ed. New York: Taylor and Francis Group, 2006. 607 p.

PAGES, J. Analyse factorielle multiple appliquee e aux variables qualitatives et aux donnees mixtes. \emph{Revue de Statistique Appliquee}. 50 (4), 5-37, 2002.

PAGES, J; Multiple Factor Analysis: main features and application to sensory data. \emph{Revista Colombiana de Estadistica}. vol. 27 n. 1, p. 1-26, 2004.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{Plot.MFA}}}

\examples{

data(DataMix) # conjunto de dados mistos

Matriz = DataMix[,2:ncol(DataMix)] 

rownames(Matriz) <- as.character(t(DataMix[1:nrow(DataMix),1]))

GroupNames=c("Notas Cafes/Trabalho","Formacao/Dedicacao","Cafes")

MF <- MFA(Matriz, c(2,2,2),TipoGrupo=c("n","c","f"), GroupNames) # realiza MFA

print("Variancias dos Componentes Principais:")
print(round(MF$MatrixA,2))

print("Matriz das Inercias Parciais/Escores das Variaveis:")
print(round(MF$MatrixEscVar,2))
}

\keyword{Analise de Multiplos Fatores}
\keyword{MFA}
\keyword{MFACT}

