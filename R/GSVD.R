GSVD <- function(data, plin = NULL, pcol = NULL) {
  # Funcao que executa a Decomposicao dos Valores 
  # Singulares Generalizados de um matriz (data)
  
  # Entrada:
  # data - Matriz usada para a decomposicao
  # plin - Peso para as linhas
  # pcol - Peso para as colunas
  
  # Retorna:
  # d - Vector linha com os valores singulares da decomposicao
  # u - Autovetores referentes das linhas
  # v - Autovetores referentes das colunas
  
  if (is.null(pcol)) pcol <- rep(1, ncol(data))
  
  if (is.null(plin)) plin <- rep(1, nrow(data))
  
  else if (is.numeric(plin)) plin = plin / sum(plin)
  
  if (!is.numeric(plin))
     stop("Entrada para 'plin' deve ser um vetor numerico. Verifique!")
  
  if (!is.numeric(pcol))
     stop("Entrada para 'pcol' deve ser um vetor numerico. Verifique!")
  
  if (nrow(data) != length(plin))
     stop("O numero de elementos em 'plin' deve ser igual ao numero de linhas de 'data'. Verifique!")
  
  if (ncol(data) != length(pcol))
     stop("O numero de elementos em 'pcol' deve ser igual ao numero de colunas de 'data'. Verifique!")
  
  plin <- as.vector(plin)
  
  pcol <- as.vector(pcol)
  
  ncv <- min(nrow(data)-1,ncol(data)) # numero de colunas validas
  
  AA = sweep(data, 2, sqrt(pcol), FUN = "*")
  
  AA = sweep(AA, 1, sqrt(plin), FUN = "*")
  
  MSVD <- svd(AA)
  d <- MSVD$d
  P <- MSVD$u
  Q <- MSVD$v
  
  MU <- diag(sqrt(1/plin))%*%P
  
  MV <- diag(sqrt(1/pcol))%*%Q
  
  Resp <- list(d = d[1:ncv], u = MU[,1:ncv], v = MV[,1:ncv])
  
  return(Resp)
}