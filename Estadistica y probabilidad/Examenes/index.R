ventas <- data.frame(
  I_i = c(0, 10, 20, 30, 40, 50, 60, 70),
  I_s = c(10, 20, 30, 40, 50, 60, 70, 80),
  operaciones = c(10, 20, 30, 25, 15, 10, 5, 5)
)

ventas$m_clase <- (ventas$I_s + ventas$I_i) / 2

ventas$subtotal <- ventas$m_clase * ventas$operaciones

# a) A partir de esta distribución de frecuencias,
# calcule el número total de galones vendidos la semana pasada. (2 puntos)

total_galones <- sum(ventas$subtotal)
cat("Total de galones vendidos la semana pasada:", total_galones * 1000)

# b) Determina la media de los galones vendidos en cada operación.
cat("La media es: ", total_galones / 12 * 1000)

# c) ¿La moda se encuentra por debajo o por arriba de los 25 000 galones?
# ¿Cómo lo sabe? (1 punto)

if (ventas$subtotal[which.max(ventas$subtotal)] * 1000 > 25000) {
  print("Esta por encima")
} else {
  print("Esta por debajo")
}

# d) Calcule la mediana de las ventas. (1 punto)
ventas$o_acumulada <- cumsum(ventas$operaciones)

for (i in 1:length(ventas$o_acumulada)) {
  cat(i)
  if (ventas$o_acumulada[i] >= 60) {
    median_intervalo <- i
    break
  }
}
