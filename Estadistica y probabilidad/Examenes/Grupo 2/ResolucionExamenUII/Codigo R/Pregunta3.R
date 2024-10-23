
# Obtener el valor de las variables
calculate_sturges_params <- function(data) {
  R <- diff(range(data))
  K <- ceiling(1 + 3.322 * log10(length(data)))
  A <- ceiling(R / K)

  list(
    range = range(data),
    num_classes = K,
    class_width = A
  )
}

create_frequency_table <- function(data) {
  n <- length(data)
  params <- calculate_sturges_params(data)

  # Crear le secuencia de limites
  rule <- seq(params$range[1],
    params$range[2] + params$class_width,
    by = params$class_width
  )

  # Calular el ancho de clase
  marca_de_clase <- (rule[-length(rule)] + rule[-1]) / 2

  # Crear los intervalos
  i <- cut(data, breaks = rule, right = FALSE)
  tabla <- table(i)

  # Calcular las frecuencias
  f_absoluta <- as.numeric(tabla)
  f_a_acumulada <- cumsum(f_absoluta)
  f_relativa <- f_absoluta / n
  f_r_acumulada <- cumsum(f_relativa)
  f_porcentual <- f_relativa * 100
  f_p_acumulada <- cumsum(f_porcentual)

  # Create data frame
  data.frame(
    intervalos = levels(i),
    f_absoluta = f_absoluta,
    f_a_acumulada = f_a_acumulada,
    f_relativa = f_relativa,
    f_r_acumulada = f_r_acumulada,
    f_porcentual = f_porcentual,
    f_p_acumulada = f_p_acumulada,
    marca_de_clase = marca_de_clase
  )
}

calculate_grouped_statistics <- function(freq_table, data, class_width) {
  n <- length(data)

  # Media para datos agrupados
  media_agrupada <- sum(freq_table$marca_de_clase * freq_table$f_absoluta) / n

  # Moda para datos agrupados
  intervalo_modal <- which.max(freq_table$f_absoluta)

  Li <- as.numeric(
    sub("\\[(.+),.*", "\\1", freq_table$intervalos[intervalo_modal])
  )
  fi <- freq_table$f_absoluta[intervalo_modal]
  fi_menos_1 <- if (intervalo_modal > 1) {
    freq_table$f_absoluta[intervalo_modal - 1]
  } else {
    0
  }
  fi_mas_1 <- if (intervalo_modal < nrow(freq_table)) {
    freq_table$f_absoluta[intervalo_modal + 1]
  } else {
    0
  }
  moda_agrupada <- Li + class_width *
    ((fi - fi_menos_1) / ((fi - fi_menos_1) + (fi - fi_mas_1)))

  # Mediana para datos agrupados
  intervalo_medinal <- which(freq_table$f_a_acumulada >= n / 2)[1]
  Li <- as.numeric(
    sub("\\[(.+),.*", "\\1", freq_table$intervalos[intervalo_medinal])
  )
  fi <- freq_table$f_absoluta[intervalo_medinal]
  Fi_1 <- if (intervalo_medinal > 1) {
    freq_table$f_a_acumulada[intervalo_medinal - 1]
  } else {
    0
  }
  mediana_agrupada <- Li + class_width / fi * (n / 2 - Fi_1)

  list(
    media = media_agrupada,
    mediana = mediana_agrupada,
    moda = moda_agrupada
  )
}

plot_frequency_diagram <- function(freq_table, class_width) {
  # Extraer los intervalos inferiores
  lower_bounds <- as.numeric(sub("\\[(.+),.*", "\\1", freq_table$intervalos))

  # Crear diagrama
  plot(lower_bounds, freq_table$f_a_acumulada,
    type = "s",
    xlab = "Número de trabajadores",
    ylab = "Frecuencia acumulada",
    main = "Diagrama Escalonado con Ojiva",
    col = "blue",
    lwd = 2
  )

  # Agregar ogiva
  lines(freq_table$marca_de_clase,
    freq_table$f_a_acumulada,
    type = "b",
    col = "red",
    pch = 16,
    lwd = 2
  )

  # Agregar etiquetas
  text(freq_table$marca_de_clase,
    freq_table$f_a_acumulada,
    labels = freq_table$f_a_acumulada,
    pos = 3,
    col = "black"
  )
}

# Funcion principal para el analisis de datos
analyze_data <- function(data) {
  # Obtener las variables
  params <- calculate_sturges_params(data)

  # Crear la tabla de frecuencia
  freq_table <- create_frequency_table(data)

  # Calcular los estadisticos para datos no agrupados
  ungrouped_stats <- list(
    media = mean(data),
    mediana = median(data),
    moda = as.numeric(names(sort(table(data), decreasing = TRUE)[1]))
  )

  # Calcular los estadisticos para datos  agrupados
  grouped_stats <- calculate_grouped_statistics(
    freq_table, data, params$class_width
  )

  # Retornar los resultados
  list(
    frequency_table = freq_table,
    ungrouped_statistics = ungrouped_stats,
    grouped_statistics = grouped_stats,
    plot = function() {
      plot_frequency_diagram(freq_table, params$class_width)
    }
  )
}

# Example usage
datos_empresas <- c(
  73, 95, 61, 46, 70, 55, 87, 65,
  75, 48, 69, 75, 75, 39, 63, 82,
  58, 43, 38, 64, 69, 79, 47, 63,
  63, 81, 59, 77, 84, 34, 75, 93,
  67, 89, 66, 52, 59, 36, 62, 43,
  75, 52, 59, 87, 74, 30, 95, 38,
  50, 72, 44, 53, 68, 72, 82, 63
)

results <- analyze_data(datos_empresas)

print(results$frequency_table)


# Print specific frequencies requested
cat("f5:", results$frequency_table$f_absoluta[5], "\n")
cat("h2:", results$frequency_table$f_relativa[2], "\n")

# Media aritmetica agrupada
print(results$grouped_statistics$media)
# Media aritmetica no agrupada
print(results$ungrouped_statistics$media)

# Mediana agrupada
print(results$grouped_statistics$mediana)
# Mediana no agrupada
print(results$ungrouped_statistics$mediana)

# Moda agrupada
print(results$grouped_statistics$moda)
# Moda no agrupada
print(results$ungrouped_statistics$moda)


results$plot()