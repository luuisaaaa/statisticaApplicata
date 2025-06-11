# Analisi descrittiva

# Caricamento del dataset
dati=read.csv("DataSet_gruppo8.csv", header=TRUE)
View(dati)

# STATISTICHE DESCRITTIVE 
# Variabile target
y <- dati$y_VideoQuality
x1_ISO <- dati$x1_ISO
x2_FRatio <- dati$x2_FRatio
x3_TIME <- dati$x3_TIME
x4_MP <- dati$x4_MP
x5_CROP <- dati$x5_CROP
x6_FOCAL <- dati$x6_FOCAL
x7_PixDensity <- dati$x7_PixDensity

# Moda 
tabulate(round(y))  # si approssima la moda con valori interi
moda <- which.max(tabulate(round(y))); moda

# Mediana e media
mediana <- median(y); mediana
media <- mean(y); media

# Robustezza agli outlier
media_con_outlier <- mean(c(1,2,3,10,13000)); media_con_outlier
mediana_con_outlier <- median(c(1,2,3,10,13000)); mediana_con_outlier

# Quartili e decili
quartili <- quantile(y, probs=c(0.25, 0.5, 0.75)); quartili
decili <- quantile(y, probs=(1:9)/10); decili

# Range e escursione campionaria
range_y <- range(y); range_y
escursione <- diff(range(y)); escursione

# Summary completo 
summary(y)

# Boxplot della variabile target
boxplot(y, main="Boxplot di y_VideoQuality", col="orange")

# Istogramma
hist(y, breaks=15, col="lightblue", main="Istogramma di y_VideoQuality", xlab="y")