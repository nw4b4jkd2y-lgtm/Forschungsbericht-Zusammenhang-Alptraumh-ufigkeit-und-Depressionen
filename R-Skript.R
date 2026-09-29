#Datenanalyse Forschungsbericht: Alpträume und Depressionen 

install.packages("rio")
library(rio)

data_Erhebung <- import("/Users/emmadette/Downloads/Albtraeume_Depression_Analysedatensatz.xlsx") 
print(data_Erhebung)

#Poweranalyse
install.packages("pwr")
library(pwr)

pwr.r.test(r = 0.20,
           sig.level = 0.05,
           power = 0.80,
           alternative = "greater")
# 152,416 Personen werden für die stichprobe mindestens benötigt, also ≈153

#deskriptive Analyse 
data_Erhebung$Geschlecht <- factor(
  data_Erhebung$Geschlecht,
  levels = c(1, 2),
  labels = c("männlich", "weiblich")
)
table(data_Erhebung$Geschlecht, data_Erhebung$Geschlecht)

table(data_Erhebung$Geschlecht)
#männlich weiblich 
#259 241 

range(data_Erhebung$Alter, na.rm = TRUE)
#Altersrange von 18-65
table(data_Erhebung$Geschlecht)
round(prop.table(table(data_Erhebung$Geschlecht))*100,1)
#Anteile 
#männlich weiblich 
#51.8     48.2 


#Mittelwerte
#Sd
#Range?
# PHQ-9
mean(data_Erhebung$PHQ9, na.rm = TRUE)
sd(data_Erhebung$PHQ9, na.rm = TRUE)
median(data_Erhebung$PHQ9, na.rm = TRUE)
min(data_Erhebung$PHQ9, na.rm = TRUE)
max(data_Erhebung$PHQ9, na.rm = TRUE)
#mean: 12.85
#sd:5.391998
#median:13
#min:0
#max:27


# Albtraumhäufigkeit
mean(data_Erhebung$MADRE_Albtraumhaeufigkeit, na.rm = TRUE)
sd(data_Erhebung$MADRE_Albtraumhaeufigkeit, na.rm = TRUE)
median(data_Erhebung$MADRE_Albtraumhaeufigkeit, na.rm = TRUE)
min(data_Erhebung$MADRE_Albtraumhaeufigkeit, na.rm = TRUE)

class(data_Erhebung$MADRE_Albtraumhaeufigkeit)
summary(data_Erhebung$MADRE_Albtraumhaeufigkeit)
#mean:4.21
#sd:1.035396
#median:4
#min:0
#max: 7




  #Fehlende Werte
sum(is.na(data_Erhebung$MADRE_Albtraumhaeufigkeit))
sum(is.na(data_Erhebung$PHQ9_Score))

# Fehlende Werte im gesamten Datensatz
sum(is.na(data_Erhebung))

# Fehlende Werte getrennt nach Variablen
colSums(is.na(data_Erhebung))
#->keine fehlenden Wert

# Wertebereich PHQ-9
range(data_Erhebung$PHQ9_Score, na.rm = TRUE)

# Unplausible PHQ-9-Werte anzeigen
data_Erhebung$PHQ9_Score[
  data_Erhebung$PHQ9_Score < 0 |
    data_Erhebung$PHQ9_Score > 27
]
numeric(0)

# Wertebereich MADRE (0-2)
range(data_Erhebung$MADRE_Albtraumhaeufigkeit, na.rm = TRUE)

# Werte außerhalb von 0 bis 7 
data_Erhebung$MADRE_Albtraumhaeufigkeit[
  data_Erhebung$MADRE_Albtraumhaeufigkeit < 0 |
    data_Erhebung$MADRE_Albtraumhaeufigkeit > 7
]
table(data_Erhebung$MADRE_Albtraumhaeufigkeit, useNA = "ifany")




#Monotomer zusammenhang testen & Ausreißer
install.packages("tidyverse")
library(tidyverse)

#Boxplot 
class(data_Erhebung$MADRE_Albtraumhaeufigkeit)
#numeric-> boxolot kann verwendet werden 
boxplot(data_Erhebung$MADRE_Albtraumhaeufigkeit,
        main = "Albtraumhäufigkeit",
        ylab = "Albtraumscore")

#Ausreißer
boxplot.stats(data_Erhebung$MADRE_Albtraumhaeufigkeit)$out

# Ausreißer bestimmen
ausreisser <- boxplot.stats(
  data_Erhebung$MADRE_Albtraumhaeufigkeit
)$out

# Ausreißer anzeigen
ausreisser
#2 2 2 2 2 2 2 2 1 1 1 7 2 0 7 2 2 1 2 2 2 2 2 7 1 2 2->sind alle Ausreißer 

# Anzahl der Ausreißer
length(ausreisser)
#gibt insgesamt 27 Ausreißer

Q1 <- quantile(data_Erhebung$MADRE_Albtraumhaeufigkeit, 0.25, na.rm = TRUE)
Q3 <- quantile(data_Erhebung$MADRE_Albtraumhaeufigkeit, 0.75, na.rm = TRUE)

IQR_Wert <- IQR(data_Erhebung$MADRE_Albtraumhaeufigkeit, na.rm = TRUE)

untere_Grenze <- Q1 - 1.5 * IQR_Wert
obere_Grenze <- Q3 + 1.5 * IQR_Wert

untere_Grenze #2,5->25%
obere_Grenze #6,5->75%

table(data_Erhebung$MADRE_Albtraumhaeufigkeit)
#„Die Überprüfung mittels Boxplot ergab 27 potenzielle Ausreißer (5,4 % der Stichprobe) nach der 
#1,5-IQR-Regel. Diese entfielen auf die Werte 0, 1, 2 und 7. Da sämtliche Werte innerhalb des 
#zulässigen Wertebereichs der Variable lagen und somit keine Hinweise auf fehlerhafte Beobachtungen 
#bestanden, wurden die betreffenden Fälle für die weitere Analyse beibehalten.“



boxplot(data_Erhebung$PHQ9,
        main = "Depressivität",
        ylab = "PHQ-9-Gesamtscore")


#Histogramm 
hist(data_Erhebung$MADRE_Albtraumhaeufigkeit,
     main = "Verteilung der Albtraumhäufigkeit",
     xlab = "Albtraumscore")

hist(data_Erhebung$PHQ9,
     main = "Verteilung der Depressivität",
     xlab = "PHQ-9-Gesamtscore")


#Quartile 
# Quartile PHQ-9

quantile(data_Erhebung$PHQ9, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
#Untere Quartil 9, Obere Quartil 17, Median 13

# Quartile Albtraumscore
quantile(data_Erhebung$MADRE_Albtraumhaeufigkeit, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
#Untere Quartil 4, Obere Quartil 5, Median 4-> wichtig für Berechnung der IQR (Ausreißer)


#Vorrausetzungen testen: monotoner Zusammenhang zwischen Depressionen und Alptraumhäufigkeit
#->Scatterplot

plot(data_Erhebung$MADRE_Albtraumhaeufigkeit, data_Erhebung$PHQ9,
     xlab = "MADRE_Albtraumhaeufigkeit",
     ylab = "PHQ9_Score",
     main = "Zusammenhang zwischen Albtraumhäufigkeit und PHQ-9",
     pch = 19)

#Jitter
plot(jitter(data_Erhebung$MADRE_Albtraumhaeufigkeit),
     jitter(data_Erhebung$PHQ9_Score),
     xlab = "Albtraumhäufigkeit",
     ylab = "PHQ-9",
     main = "Zusammenhang zwischen Albtraumhäufigkeit und PHQ-9",
     pch = 19)

#Trendlinie
plot(jitter(data_Erhebung$MADRE_Albtraumhaeufigkeit),
     jitter(data_Erhebung$PHQ9_Score),
     xlab = "Albtraumhäufigkeit",
     ylab = "PHQ-9",
     main = "Albtraumhäufigkeit und PHQ-9",
     pch = 19)
lines(lowess(data_Erhebung$MADRE_Albtraumhaeufigkeit, data_Erhebung$PHQ9_Score),
      lwd = 2)
#->Die visuelle Prüfung des Streudiagramms mit geglätteter Trendlinie ergab einen überwiegend 
#monoton positiven Zusammenhang zwischen Albtraumhäufigkeit und depressiver Symptomatik. 
#Hinweise auf eine ausgeprägte nicht-monotone Beziehung zeigten sich nicht.



# Einseitige Spearman-Rangkorrelation
data_cor <- cor.test(
  data_Erhebung$MADRE_Albtraumhaeufigkeit,
  data_Erhebung$PHQ9_Score,
  method = "spearman",
  alternative = "greater",
  exact = FALSE)

data_cor
#data:  data_Erhebung$MADRE_Albtraumhaeufigkeit and data_Erhebung$PHQ9_Score
#S = 18404982, p-value = 0.004545         
#-> (0.004545<0,05->Zusammenhang ist signifikant 
#aber klein, da große Stichprobe) nur schwacher positiver Zusammenhang 
#alternative hypothesis: true rho is greater than 0
#sample estimates: rho =0.1165574 





# Boot Packet 
install.packages("boot")
library(boot)


# Nur vollständige Fälle der beiden Variablen verwenden
df_cor <- na.omit(
  data.frame(
    MADRE = data_Erhebung$MADRE_Albtraumhaeufigkeit,
    PHQ9 = data_Erhebung$PHQ9_Score
  )
)

# Anzahl der tatsächlich verwendeten Personen
nrow(df_cor)

# Funktion für die Spearman-Korrelation
spearman_fun <- function(data, indices) {
  d <- data[indices, ]
  cor(d$MADRE, d$PHQ9, method = "spearman")
}

# Bootstrap mit 5000 Wiederholungen
set.seed(123)

boot_result <- boot(
  data = df_cor,
  statistic = spearman_fun,
  R = 5000
)

# 95%-Konfidenzintervall
boot.ci(boot_result, type = "perc")
#Der beobachtete Zusammenhang beträgt ρ = .117. Unter Berücksichtigung der Stichprobenunsicherheit 
#sind jedoch auch kleinere Zusammenhänge um .03 oder etwas größere Zusammenhänge bis etwa .21 mit den 
#Daten vereinbar.

#Streng statistisch sollte man nicht sagen: „Mit 95 % Wahrscheinlichkeit liegt der wahre Wert zwischen
#.030 und .206.“ Beim frequentistischen 95%-KI bezieht sich 95 % auf das Verfahren: Würde man die 
#Untersuchung sehr häufig mit neuen Stichproben wiederholen und jedes Mal auf dieselbe Weise ein 
#95%-KI bestimmen, würden ungefähr 95 % dieser Intervalle den wahren Populationsparameter einschließen.

citation()
citation("boot")
citation("pwr")
citation("rio")
citation("tidyverse")

