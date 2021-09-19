# Function to performance measures ---------------------------- 

medidas_desempenho <- function(teste, classi){
  matriz_conf <- table(classi, teste)
  sens <- matriz_conf[2, 2] / sum(matriz_conf[ , 2])
  esp <- matriz_conf[1, 1] / sum(matriz_conf[ , 1])
  vpp <- matriz_conf[2, 2] / sum(matriz_conf[2, ])
  vpn <- matriz_conf[1, 1] / sum(matriz_conf[1, ])
  acc <- (matriz_conf[2, 2] + matriz_conf[1, 1]) / sum(matriz_conf)
  return(list(se = sens, es = esp, vpp = vpp, vpn = vpn, acc = acc))
}

