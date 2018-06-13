\name{Data_Cafes}

\alias{Data_Cafes}

\docType{data}

\title{Conjunto de dados de frequencia.}

\description{Conjunto de dados categorizados por cafes, sobre habilidades sensoriais no consumo de cafes especiais.}

\usage{data(Data_Cafes)}

\format{Conjunto de dados de uma pesquisa feita com o proposito de avaliar a concordancia entre as respostas de diferentes grupos de consumidores com diferentes habilidades sensoriais. O experimento relaciona a analise sensorial de cafes especiais definidos por (A) Bourbon Amarelo, cultivado a altitudes maiores do que 1200 m.; (D) idem a (A) diferenciando apenas no preparo das amostras; (B) Acaia cultivados a altitude menores do que 1.100m; (C)  idem ao (B) porem diferenciando o preparo das amostras. Aqui os dados estao categorizados por cafes. O exemplo dado demonstra os resultados encontrados em OSSANI et al. (2017).}


\references{
OSSANI, P. C.; CIRILLO, M. A.; BOREM, F. M.; RIBEIRO, D. E.; CORTEZ, R. M.. Quality of specialty coffees: a sensory evaluation by consumers using the MFACT technique. \emph{Revista Ciencia Agronomica (UFC. Online)}, v. 48, p. 92-100, 2017.

OSSANI, P. C. \emph{Qualidade de cafes especiais e nao especiais por meio da analise de multiplos fatores para tabelas de contingencias.} 2015. 107 p. Dissertacao (Mestrado em Estatistica e Experimentacao Agropecuaria) - Universidade Federal de Lavras, Lavras, 2015.
}


\examples{
data(Data_Cafes) # conjunto de dados categorizados

Matriz = Data_Cafes[,2:ncol(Data_Cafes)] 

rownames(Matriz) <- as.character(t(Data_Cafes[1:nrow(Data_Cafes),1]))

GroupNames = c("Cafe A", "Cafe B", "Cafe C", "Cafe D")

MF <- MFA(Matriz, c(16,16,16,16), c(rep("f",4)), GroupNames) 

print("Variancias dos Componentes Principais:"); round(MF$MatrixA,2)

print("Matriz das Inercias Parciais/Escores das Variaveis:"); round(MF$MatrixEscVar,2)

Tit = c("Scree-plot","Individuos", "Individuos/Tipos Cafes", "Inercias Grupos")

Plot.MFA(MF, Titles = Tit, xlabel = NA, ylabel = NA,
         PosLeg = 2, BoxLeg = FALSE, Color = TRUE, 
         NamArr = FALSE, LinLab = NA, Casc = FALSE) # Imprime varios graficos da tela

}

\keyword{Conjunto de dados}

