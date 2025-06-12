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

#modello completo, che include tutte le variabili indipendenti
model_full <- lm(y_VideoQuality ~ ., data = data)

#modello ridotto, costruito includendo solo le variabili
#risultano piu significativamente associate a y VideoQuality.
model_reduced <- lm(y_VideoQuality ~ x1_ISO + x2_FRatio + x5_CROP + x3_TIME, data = data)


#Stima e intervalli di confidenza dei parametri
# Modello completo
summary(model_full)
confint(model full, level = 0.95)

# Modello ridotto
summary(model_reduced)
confint(model reduced, level = 0.95)



# PUNTO 4 - Stima dei coefficienti e intervalli di confidenza al 95%
# l'ho fatto sia per il completo che per il ridotto (non so se andava fatto su entrambi)
# Per tutti i parametri calcolo l'intervallo di confidenza al 95% sulla base
# della t di Student. Ci stanno grafici a barre orizzontali per visualizzare
# l'intervallo di confidenza dei coefficienti e, per il modello ridotto anche
# grafici delle regressioni con intervalli di confidenza per ciascuna variabile indipendent, sul modello completo
#i grafici delle singole variabili non li ho messi perchè quelli delle variabili significative stanno già nel ridotto


# MODELLO COMPLETO

# Estrazione dei coefficienti stimati e degli errori standard
parametri_full <- summary(model_full)$coefficients

# Parametri globali per il calcolo degli IC
n_sample_full <- nrow(data)  # numero di osservazioni
k_param_full <- length(coef(model_full))  # numero di parametri stimati (inclusa l'intercetta)
alpha <- 0.05  # livello di significatività
t_score_full <- qt(1 - alpha/2, df = n_sample_full - k_param_full)  # quantile t-student

cat("Intervalli di Confidenza al 95% (Modello Completo):\n\n")

# Calcolo manuale degli intervalli di confidenza per ogni parametro
for (param in rownames(parametri_full)) {
  beta <- parametri_full[param, "Estimate"]
  errore_std <- parametri_full[param, "Std. Error"]
  IC_basso <- beta - t_score_full * errore_std
  IC_alto <- beta + t_score_full * errore_std
  cat(sprintf("%-15s: [%.4f , %.4f]\n", param, IC_basso, IC_alto))
}

# Costruzione del data frame per il grafico
nomi_parametri_full <- rownames(parametri_full)
stima_full <- parametri_full[, "Estimate"]
errore_std_full <- parametri_full[, "Std. Error"]
IC_min_full <- stima_full - t_score_full * errore_std_full
IC_max_full <- stima_full + t_score_full * errore_std_full

df_ic_full <- data.frame(
  parametro = nomi_parametri_full,
  stima = stima_full,
  IC_basso = IC_min_full,
  IC_alto = IC_max_full
)

# Grafico ggplot: Intervalli di confidenza 95% (Modello Completo)
ggplot(df_ic_full, aes(x = stima, y = reorder(parametro, stima))) +
  geom_point(color = "darkgreen", size = 3) +
  geom_errorbarh(aes(xmin = IC_basso, xmax = IC_alto), height = 0.2, color = "gray40") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Intervalli di Confidenza al 95% (Modello Completo)",
    x = "Valore stimato ± IC",
    y = "Parametro"
  ) +
  theme_minimal(base_size = 14)


# MODELLO RIDOTTO

# Estrazione dei coefficienti stimati e degli errori standard
parametri_stimati <- summary(model_reduced)$coefficients

# Parametri globali per il calcolo degli IC
n_sample <- nrow(data)
k_param <- length(coef(model_reduced))
alpha <- 0.05
t_score <- qt(1 - alpha/2, df = n_sample - k_param)

cat("Intervalli di Confidenza al 95% (Modello Ridotto):\n\n")

# Calcolo manuale degli intervalli di confidenza per ogni parametro
for (param in rownames(parametri_stimati)) {
  beta <- parametri_stimati[param, "Estimate"]
  errore_std <- parametri_stimati[param, "Std. Error"]
  IC_basso <- beta - t_score * errore_std
  IC_alto <- beta + t_score * errore_std
  cat(sprintf("%-12s: [%.4f , %.4f]\n", param, IC_basso, IC_alto))
}

# Costruzione del data frame per il grafico
nomi_parametri <- rownames(parametri_stimati)
stima <- parametri_stimati[, "Estimate"]
errore_std <- parametri_stimati[, "Std. Error"]
IC_min <- stima - t_score * errore_std
IC_max <- stima + t_score * errore_std

df_ic <- data.frame(
  parametro = nomi_parametri,
  stima = stima,
  IC_basso = IC_min,
  IC_alto = IC_max
)

# Grafico ggplot: Intervalli di confidenza 95% (Modello Ridotto)
ggplot(df_ic, aes(x = stima, y = reorder(parametro, stima))) +
  geom_point(color = "blue", size = 3) +
  geom_errorbarh(aes(xmin = IC_basso, xmax = IC_alto), height = 0.2, color = "darkgray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Intervalli di Confidenza al 95% (Modello Ridotto)",
    x = "Valore stimato ± IC",
    y = "Parametro"
  ) +
  theme_minimal(base_size = 14)


# GRAFICI VARIAZIONE SINGOLA VARIABILE 

# Funzione per costruire il grafico di regressione e intervallo di confidenza per una singola variabile
grafico_ic_singola_variabile <- function(var_nome, model, data) {
  x_seq <- seq(min(data[[var_nome]]), max(data[[var_nome]]), length.out = 100)
  
  # Calcolo delle medie delle altre variabili nel modello ridotto
  media_vars <- colMeans(data[c("x1_ISO", "x2_FRatio", "x3_TIME", "x5_CROP")])
  
  # Creo 100 righe identiche inizializzate con le medie
  newdata <- as.data.frame(matrix(rep(media_vars, each = 100), nrow = 100))
  colnames(newdata) <- names(media_vars)
  
  # Sovrascrivo la variabile che voglio far variare
  newdata[[var_nome]] <- x_seq
  
  # Predizione dei valori attesi con intervallo di confidenza
  pred <- predict(model, newdata = newdata, interval = "confidence", level = 0.95)
  
  # Costruzione del data frame da usare in ggplot
  df_plot <- data.frame(
    x = x_seq,
    y_fit = pred[, "fit"],
    y_min = pred[, "lwr"],
    y_max = pred[, "upr"]
  )
  
  # Grafico con linea di regressione e fascia di confidenza
  ggplot(df_plot, aes(x = x, y = y_fit)) +
    geom_line(color = "blue", linewidth = 1.2) +
    geom_ribbon(aes(ymin = y_min, ymax = y_max), alpha = 0.2, fill = "blue") +
    geom_point(data = data, aes_string(x = var_nome, y = "y_VideoQuality"), color = "black", size = 1.5) +
    labs(
      title = paste("Regressione + IC 95% per", var_nome),
      x = var_nome,
      y = "y_VideoQuality"
    ) +
    theme_minimal(base_size = 14)
}

# Esecuzione della funzione per ciascuna variabile del modello ridotto
grafico_ic_singola_variabile("x1_ISO", model_reduced, data)
grafico_ic_singola_variabile("x2_FRatio", model_reduced, data)
grafico_ic_singola_variabile("x3_TIME", model_reduced, data)
grafico_ic_singola_variabile("x5_CROP", model_reduced, data)


#punto 5
#Calcolo R², R² aggiustato e RMSE per i due modelli 

# Per il modello completo
summary_full <- summary(model_full)
r2_full <- summary_full$r.squared                    # Coefficiente di determinazione
adj_r2_full <- summary_full$adj.r.squared            # R² aggiustato
rmse_full <- sqrt(mean(summary_full$residuals^2))    # Root Mean Squared Error (errore quadratico medio)

# Per il modello ridotto
summary_red <- summary(model_reduced)
r2_red <- summary_red$r.squared
adj_r2_red <- summary_red$adj.r.squared
rmse_red <- sqrt(mean(summary_red$residuals^2))

#Stampa dei risultati a confronto
cat("Confronto tra modello completo e ridotto:\n\n")
cat(sprintf("MODELLO COMPLETO:\nR² = %.4f | R² aggiustato = %.4f | RMSE = %.4f\n\n", 
            r2_full, adj_r2_full, rmse_full))
cat(sprintf("MODELLO RIDOTTO:\nR² = %.4f | R² aggiustato = %.4f | RMSE = %.4f\n\n", 
            r2_red, adj_r2_red, rmse_red))


# Grafico comparativo tra le metriche dei due modelli
library(ggplot2)

# Costruzione del data frame per il grafico
df_confronto <- data.frame(
  Modello = rep(c("Completo", "Ridotto"), each = 3),
  Metrica = rep(c("R²", "R² aggiustato", "RMSE"), times = 2),
  Valore = c(r2_full, adj_r2_full, rmse_full, r2_red, adj_r2_red, rmse_red)
)

# Grafico a barre affiancate
ggplot(df_confronto, aes(x = Metrica, y = Valore, fill = Modello)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Valore, 3)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 4) +
  labs(
    title = "Confronto tra modello completo e ridotto",
    x = "Metrica",
    y = "Valore",
    fill = "Modello"
  ) +
  theme_minimal(base_size = 14)


# Grafici diagnostici per i modelli lineari 

# Visualizzazione dei 4 grafici diagnostici base di R:
# 1. Residui vs Valori adattati
# 2. QQ Plot dei residui
# 3. Scale-Location
# 4. Residui vs Leverage

# Per il modello completo
par(mfrow = c(2, 2))
plot(model_full, main = "Diagnostica - Modello Completo")

# Per il modello ridotto
par(mfrow = c(2, 2))
plot(model_reduced, main = "Diagnostica - Modello Ridotto")

# Reset layout grafico
par(mfrow = c(1, 1))

