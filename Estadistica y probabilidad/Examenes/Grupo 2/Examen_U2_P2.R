datos_empresas <- c(73, 95, 61, 46, 70, 55, 87, 65, 75, 48, 69, 75, 75, 39, 63, 82, 
           58, 43, 38, 64, 69, 79, 47, 63, 63, 81, 59, 77, 84, 34, 75, 93, 
           67, 89, 66, 52, 59, 36, 62, 43, 75, 52, 59, 87, 74, 30, 95, 38, 
           50, 72, 44, 53, 68, 72, 82, 63)

# a) Elaborar la tabla de frecuencias usando la regla de Sturges. (2 puntos)

R = max(datos_empresas) - min(datos_empresas)
rango = range(datos_empresas)
K = ceiling(1 + 3.322 * log10(length(datos_empresas)))
A = ceiling(R/K)
n = length(datos_empresas)

rule = seq(rango[1], rango[2] + A, by = A)
rule_1 = seq(rango[1], rango[2], by = A)
marca_de_clase = (rule[-length(rule)] + rule[-1]) / 2 
i = cut(datos_empresas, breaks = rule, right = FALSE)
tabla <- table(i)
intervalos <- levels(i)
f_absoluta <- as.numeric(tabla)
f_a_acumulada <- cumsum(f_absoluta)
f_relativa <- f_absoluta / n
f_r_acumulada <- cumsum(f_relativa)
f_porcentual <- f_relativa * 100 
f_p_acumulada <- cumsum(f_porcentual)

frecuencia_empresas <-data.frame(
  intervalos,
  f_absoluta,
  f_a_acumulada,
  f_relativa,
  f_r_acumulada,
  f_porcentual,
  f_p_acumulada
)

# b) Interpretar: f5, h2. (2 puntos)

f_5 = frecuencia_empresas$f_absoluta[5]
cat('tal .... es ', f_5)
h_2 = frecuencia_empresas$f_relativa[2]
cat('tal .... es ', h_2)

#c) Elaborar el diagrama escalonado y la ojiva respectiva. (2 puntos)
plot(rule_1, frecuencia_empresas$f_a_acumulada, 
     type = "s", xlab = "Número de trabajadores", 
     ylab = "Frecuencia", 
     main = "Diagrama Escalonado con Ojiva", 
     col = "blue", lwd = 2)

lines(marca_de_clase, 
      tabla_frecuencia$f_a_acumulada, 
      type = "b", col = "red", pch = 16, lwd = 2
      )
text(marca_de_clase, 
     tabla_frecuencia$f_a_acumulada, 
     labels = tabla_frecuencia$f_a_acumulada, 
     pos = 3, col = "black")


# D. Calcular la media aritm´etica (¯x) y la mediana (Me)
#para datos agrupadosy no agrupados. (2 puntos)

#e) Calcular la moda (Mo) para datos agrupados y no agrupados.


# No agrupadas 

media_empresas = mean(datos_empresas)
mediana_empresas = median(datos_empresas)

moda_empresas =  as.numeric(names(sort(table(datos), decreasing = TRUE)[1]))

# Agrupados

media_agrupada = sum(marca_de_clase * frecuencia_empresas$f_absoluta) / n

intervalo_modal = which.max(frecuencia_empresas$f_absoluta)

Li =  rule_1[intervalo_modal]
fi = frecuencia_empresas$f_absoluta[intervalo_modal]
fi_menos_1 = frecuencia_empresas$f_absoluta[intervalo_modal-1]
fi_mas_1 = frecuencia_empresas$f_absoluta[intervalo_modal+1]

moda_agrupada = Li + A*((fi - fi_menos_1)/((fi - fi_menos_1)+(fi - fi_mas_1)))

cat('La moda es:', moda_agrupada)

# mediana agrupada

intervalo_medinal = which(frecuencia_empresas$f_a_acumulada >= n/2)[1]
Li = rule_1[intervalo_medinal]
fi = frecuencia_empresas$f_absoluta[intervalo_medinal]
Fi_1 = frecuencia_empresas$f_a_acumulada[intervalo_medinal-1]

mediana = Li + A/fi * (n/2 - Fi_1)
