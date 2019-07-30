NormTest <- function(data, sign = 0.05) {
  # Executa teste para verificar a normalidade dos dados baseado
  # no teste de coeficiente de assimetria
  # Desenvolvida por Paulo Cesar Ossani em 22/06/2013
  # Ver Livro Daniel Furtado pg. 115 e Rencher pg. 114
  
  # Entrada
  # data - Dados a serem analisados
  # sign - Grau de significancia do teste (default 5%)
  
  # Retorna:
  # statistic - Valor Chi-quadrado observado, ou seja, a estatistica do teste.
  # chisquare - Valor Chi-quadrado calculado.
  # gl        - Grau de liberdade.
  # p.value   - Valor p.
  
  if (!is.data.frame(data) && !is.matrix(data)) 
     stop("Entrada 'data' esta incorreta, deve ser do tipo dataframe ou matriz. Verifique!")
  
  if (!is.numeric(sign)) 
     stop("Entrada para 'sign' esta incorreta, deve ser numerica com valores entre 0 e 1. Verifique!")
  
  if (sign<=0 || sign>1) 
     stop("Entrada para 'sign' esta incorreta, deve ser valores entre 0 e 1. Verifique!")
  
  n <- ncol(data)*nrow(data) # numero de elementos amostrais
  
  p <- ncol(data)  # numero de parametros
  
  gl =  p*(p+1)*(p+2)/6 # grau de liberdade
  
  Media = as.vector(apply(data,2,mean))  # data medias das colunas
  
  G     = t(t(data)-Media)%*%solve(cov(data))%*%(t(data)-Media)
  
  B1p   = sum((diag(G))^3/n^2)
  
  Chi.Quad.Observado <- n*B1p/6 # Estatistica do Teste
  
  qt = qchisq(1-sign,gl,ncp=0) # Valor Qui-quadrado calculado
  
  pVal <- pchisq(Chi.Quad.Observado,gl,ncp=0, lower.tail = F)
  
#  cat(paste("Grau de liberdade observado:", round(gl,7)),"\n")

#  cat(paste("Valor da estatistica do teste Qui-quadrado (Chiq1):", round(Chi.Quad.Observado,7)),"\n")
  
#  cat(paste("Valor Qui-quadrado calculado (Chiq2) com", sign*100,"% de significancia:", round(qt,7)),"\n")
  
#  if (Chi.Quad.Observado<=qt) cat("Como Chiq1 <= Chiq2, VERIFICA-SE a normalidade dos dados.\n")
  
#  if (Chi.Quad.Observado>qt) cat("Como Chiq1 > Chiq2, NAO VERIFICA-SE a normalidade dos dados.\n")
  
#  cat("Valor-p:", pVal)
  
  Lista <- list(statistic = Chi.Quad.Observado, chisquare = qt, gl = gl, p.value = pVal)
  
  returnValue(Lista)
  
}