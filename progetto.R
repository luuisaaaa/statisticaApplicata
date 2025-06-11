# Calcolo frequenze per classi
frequenze_classi <- function(x, k) {
  breaks <- seq(min(x), max(x), length.out = k + 1)
  classi <- cut(x, breaks = breaks, include.lowest = TRUE, right = FALSE)
  frequenze <- table(classi)
  return(list(frequenze = frequenze, breaks = breaks, classi = classi))
}

# Centri delle classi
centri_classi <- function(breaks) {
  return((head(breaks, -1) + tail(breaks, -1)) / 2)
}

frequenzaRelativa <- function(frequenze, n) {
  return(frequenze / n)
}

frequenzaCumulativa <- function(frequenze) {
  return(cumsum(frequenze))
}

frequenzaRelativaCumulativa <- function(freq_cum, n) {
  return(freq_cum / n)
}

intervalloInterquartile <- function(x) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  return(c(inf = q1 - 1.5 * iqr, sup = q3 + 1.5 * iqr))
}

# Librerie necessarie
library(ggplot2)
library(GGally)
library(corrplot)
library(psych)


#Caricamento e setup iniziale
data <- read.csv("DataSet_gruppo8.csv", header=TRUE)
View(data)

n <- nrow(data)
variabili <- data[, 1:7]  # tutte le x numeriche

# Numero classi con regola di Sturges
k <- round(1 + 3.3 * log10(n))


#hsh
summary(data)

#
for (nome in names(variabili)) {
  cat("\nAnalisi descrittiva per", nome, "\n")
  
  x <- variabili[[nome]]
  risultati <- frequenze_classi(x, k)
  frequenze <- risultati$frequenze
  breaks <- risultati$breaks
  centri <- centri_classi(breaks)
  fr <- frequenzaRelativa(frequenze, length(x))
  fc <- frequenzaCumulativa(frequenze)
  frc <- frequenzaRelativaCumulativa(fc, length(x))
  outliers <- intervalloInterquartile(x)
  
  print(frequenze)
  print(centri)
  print(fr)
  print(fc)
  print(frc)
  cat("Intervallo interquartile (limiti outlier):\n"); print(outliers)
}

#Boxplot e istogrammi
# Boxplot unico

boxplot(data$y_VideoQuality,
        main = "Boxplot della variabile Y",
        las = 2,
        cex.axis = 0.9) #y


boxplot(data[, 2:8],
        main = "Boxplot delle variabili X",
        las = 2,
        cex.axis = 0.8)

# Istogrammi per ciascuna variabile
par(mfrow = c(3, 3))
for (nome in names(variabili)) {
  hist(variabili[[nome]], 
       main = paste("Istogramma di", nome), 
       xlab = nome, 
       col = "skyblue", 
       breaks = k)
}
par(mfrow = c(3, 3))

# Verifica normalità variabili (Q-Q plot + Shapiro)
qqnorm(data$y_VideoQuality); qqline(data$y_VideoQuality, col = "red")
shapiro.test(data$y_VideoQuality)
# Il p-value > 0.05, quindi non si rifiuta l’ipotesi di normalità.
#  Y è sufficientemente normale, compatibile con l’uso della regressione lineare.

# Ciclo opzionale sulle X
for (var in names(data)[1:7]) {
  cat("\n", var, "\n")
  qqnorm(data[[var]]); qqline(data[[var]], col = "green")
  print(shapiro.test(data[[var]]))
}
#Tutti i p-value < 0.05 → normalità rifiutata per tutte le X (con Shapiro–Wilk).

# Correlazione e scatter matrix
corr_matrix <- cor(data[sapply(data, is.numeric)])
corrplot.mixed(corr_matrix, order = "original", number.cex = 1, upper = "ellipse")
corPlot(corr_matrix, cex = 1.1, show.legend = TRUE, main = "Correlazione variabili")
ggpairs(data, upper = NULL)

# Scatter plot individuali (base R)
par(mfrow = c(2, 4), oma = c(0, 0, 4, 0))
plot(data$x1_ISO, data$y_VideoQuality, xlab = "ISO", ylab = "Video Quality")
plot(data$x2_FRatio, data$y_VideoQuality, xlab = "FRatio", ylab = "Video Quality")
plot(data$x3_TIME, data$y_VideoQuality, xlab = "TIME", ylab = "Video Quality")
plot(data$x4_MP, data$y_VideoQuality, xlab = "MP", ylab = "Video Quality")
plot(data$x5_CROP, data$y_VideoQuality, xlab = "CROP", ylab = "Video Quality")
plot(data$x6_FOCAL, data$y_VideoQuality, xlab = "FOCAL", ylab = "Video Quality")
plot(data$x7_PixDensity, data$y_VideoQuality, xlab = "PixDensity", ylab = "Video Quality")
mtext("Relazione tra variabili indipendenti e y", outer = TRUE, cex = 1.5, line = 0.5)
par(mfrow = c(1, 1))

# Scatter ggplot2 con regressione
nomi_variabili <- names(data)[names(data) != "y_VideoQuality"]
for (nome in nomi_variabili) {
  p <- ggplot(data, aes_string(x = nome, y = "y_VideoQuality")) +
    geom_point(color = "steelblue", size = 2) +
    geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
    labs(
      title = paste("Scatter plot tra", nome, "e y_VideoQuality"),
      x = nome,
      y = "y_VideoQuality"
    ) +
    theme_minimal(base_size = 14)
  print(p)
}
