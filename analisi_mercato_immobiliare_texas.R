#PARTE 1

#1) Importa il dataset "Real Estate Texas.csv"

real_estate_data <- read.csv("Real Estate Texas.csv")
attach(real_estate_data)


#2) Indica il tipo di variabili contenute nel dataset

data_type <- sapply(real_estate_data, class)
data_type


#3) Calcola indici di posizione, variabilità e forma

#distribuzioni di frequenze assolute
abs_freq <- sapply(real_estate_data, table)
abs_freq

#indici di posizione

#min, max, media, mediana, quartili
summary <- summary(real_estate_data[4:8])
summary

#decili e percentili
deciles <- sapply(real_estate_data[4:8], quantile, probs=seq(0,1,0.1))
deciles 

percentiles <- sapply(real_estate_data[4:8], quantile, probs=seq(0,1,0.01))
percentiles

#indici di variabilità

#range
range <- sapply(real_estate_data[4:8], range)
range

range_func <- function(variable){
  return(max(variable)-min(variable))
}

range_var <- sapply(real_estate_data[4:8], range_func)
range_var

#range interquartile
IQR <- sapply(real_estate_data[4:8], IQR)
IQR

#varianza 
sigma2 <- sapply(real_estate_data[4:8], var)
sigma2

#deviazione standard
sigma <- sapply(real_estate_data[4:8], sd)
sigma

#coefficiente di variazione
CV <-function(variable){
  return( sd(variable)/mean(variable) * 100 )
}

CV_var <- sapply(real_estate_data[4:8], CV)
CV_var

#indici di forma: asimmetria e curtosi
install.packages("moments")
library(moments)

gamma1 <- skewness(real_estate_data[4:8])
gamma1

gamma2 <- kurtosis(real_estate_data[4:8]) - 3
gamma2


#4)variabile con variabilità più elevata e variabile più asimmetrica

#volume: 53.71 % della media e indice di asim. di Fisher 0.88


#5)Dividere una delle variabili quantitative in classi, calcolare la distr.
#di frequenze, il grafico a barre corrispondente e l’indice di Gini

summary(median_price)

median_price_cl <- cut(median_price,
                       breaks = c(70000,100000,130000,160000,190000))

N  <- length(median_price)
ni <- table(median_price_cl)
fi <- ni/N
Ni <- cumsum(ni)
Fi <- Ni/N
distr_freq_median_price_cl <- cbind(ni,fi,Ni,Fi)
distr_freq_median_price_cl 

bp_ni <- barplot(main = "Distribuzione delle frequenze assolute del prezzo mediano",
        ni,
        xlab = "Classi di prezzo mediano di vendita in $",
        ylab = "Frequenze assolute",
        names.arg = c("70k-100k","100k-130k","130k-160k","160k-190k"),
        ylim = c(0,140),
        col = "cyan3",
        border = "black")
text(x = bp_ni, y = ni, labels = ni, col = "black", pos = 1, cex = 1.1)

bp_fi <- barplot(main = "Distribuzione delle frequenze relative del prezzo mediano",
        fi,
        xlab = "Classi di prezzo mediano di vendita in $",
        ylab = "Frequenze relative",
        names.arg = c("70k-100k","100k-130k","130k-160k","160k-190k"),
        ylim = c(0,1),
        col = "blue2",
        border = "black")
text(x = bp_fi, y = fi, labels = round(fi, 2), col = "white", pos = 1, cex = 1.1)

bp_Ni <- barplot(main = "Distribuzione delle frequenze cumulate del prezzo mediano",
        Ni,
        xlab = "Classi di prezzo mediano di vendita in $",
        ylab = "Frequenze cumulate",
        names.arg = c("70k-100k","100k-130k","130k-160k","160k-190k"),
        ylim = c(0,250),
        col = "orange2",
        border = "black")
text(x = bp_Ni, y = Ni, labels = Ni, col = "black", pos = 1, cex = 1.1)

bp_Fi <- barplot(main = "Distribuzione delle frequenze relative cumulate del prezzo mediano",
        Fi,
        xlab = "Classi di prezzo mediano di vendita in $",
        ylab = "Frequenze relative cumulate",
        names.arg = c("70k-100k","100k-130k","130k-160k","160k-190k"),
        ylim = c(0,1),
        col="red3",
        border = "black")
text(x = bp_Fi, y = Fi, labels = round(Fi, 2), col = "white", pos = 1, cex = 1.1)


#indice di eterogeneità di Gini
J <- length(table(median_price_cl))
G <- 1-sum(fi^2)
gini_index <- G/((J-1)/J)
gini_index


#6) indice di eterogneità di Gini per la variabile city

table(city)
#indice di gini (city) = 1


#7)Probabilità 

#città: Beaumont
poss_cases <- length(real_estate_data[,1])
fav_cases_Beau <- sum(city == "Beaumont")
p_Beaumont <- fav_cases_Beau/poss_cases
p_Beaumont

#mese: luglio
fav_cases_july <- sum(month == "7")
p_july <- fav_cases_july/poss_cases
p_july

#mese e anno: dicembre 2012
fav_cases_dec_12 <- sum(month == "12" & year == "2012")
p_dec_12 <- fav_cases_dec_12/poss_cases
p_dec_12


#8)Creare una colonna con il prezzo medio

real_estate_data$average_price <- (volume*10^6)/sales


#9)Crea una colonna per l'efficacia degli annunci di vendita

real_estate_data$conversion_rate <- (sales/listings)*100

attach(real_estate_data)
summary(conversion_rate)


#10)Crea dei summary() con dplyr condizionatamente a città, anni e mesi

install.packages("dplyr")
library(dplyr)

#group_by_city
summary_volume_city <-
  real_estate_data %>%
  group_by(city) %>%
  summarise(Min = min(volume),
            First_Qu = quantile(volume)[2],
            Median = median(volume), 
            Mean = mean(volume),
            Third_Qu = quantile(volume)[4],
            Max = max(volume))

summary_volume_city 

#group_by_year
summary_listings_year <-
  real_estate_data %>%
  group_by(year) %>%
  summarise(Min = min(listings),
            First_Qu = quantile(listings)[2],
            Median = median(listings), 
            Mean = mean(listings),
            Third_Qu = quantile(listings)[4],
            Max = max(listings))

summary_listings_year 

#group_by_month
summary_average_price_month <-
  real_estate_data %>%
  group_by(month) %>%
  summarise(Min = min(average_price),
            First_Qu = quantile(average_price)[2],
            Median = median(average_price), 
            Mean = mean(average_price),
            Third_Qu = quantile(average_price)[4],
            Max = max(average_price))

summary_average_price_month

#group_by_city_and_years
summary_sales_city_year <-
  real_estate_data %>%
  group_by(city,year) %>%
  summarise(Min = min(sales),
            First_Qu = quantile(sales)[2],
            Median = median(sales), 
            Mean = mean(sales),
            Third_Qu = quantile(sales)[4],
            Max = max(sales))

summary_sales_city_year


#PARTE 2
#grafici con ggplot2

install.packages("ggplot2")
library(ggplot2)

#1)Boxplot per confrontare i prezzi mediani delle case nelle varie città

ggplot(real_estate_data) +
  geom_boxplot(aes(
    x = city, 
    y = median_price), 
    fill = "lightblue2") +
  labs(title = "Boxplot dei prezzi mediani di vendita per città",
       x = "Città",
       y = "Prezzo mediano di vendita in $") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


#2)Boxplot per confrontare il valore totale delle vendite rispetto a città  e anni

#versione 1
ggplot(real_estate_data) +
  geom_boxplot(aes(
    x = city, 
    y = volume, 
    fill = factor(year))) +
  labs(title = "Boxplot del valore totale delle vendite per città e anno",
       x = "Città",
       y = "Valore totale delle vendite in milioni di $",
       fill = "Anno") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

#versione 2
ggplot(real_estate_data) +
  geom_boxplot(aes(
    x = factor(year), 
    y = volume, 
    fill = city)) +
  labs(title = "Boxplot del valore totale delle vendite per anno e città",
       x = "Anno",
       y = "Valore totale delle vendite in milioni di $",
       fill = "Città") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")


#3)Grafico a barre sovrapposte e barre normalizzato per anno che confronta
#il totale delle vendite nei vari mesi e sempre considerando le città.

#grafico a barre sovrapposte - anno 2010
ggplot(real_estate_data[year==2010, ])+
  geom_col(aes(
    x = factor(month),
    y = sales,
    fill = city),
    position = "stack",
    col = "black") +
  labs(title = "Grafico a barre sovrapposte delle vendite totali per mese e città - 2010",
       x = "Mese",
       y = "Numero totale di vendite - 2010",
       fill = "Città") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  scale_y_continuous(breaks = seq(0,1000,100))

#grafico a barre normalizzato - anno 2010
ggplot(real_estate_data[year==2010, ]) +
  geom_col(aes(
    x = factor(month),
    y = sales,
    fill = city),
    position = "fill",
    col = "black") +
  labs(title = "Grafico a barre normalizzato delle vendite totali per mese e città - 2010",
       x = "Mese",
       y = "Numero totale di vendite relative - 2010",
       fill = "Città") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  scale_y_continuous(breaks = seq(0,1,0.1))

#grafico a barre sovrapposte per ogni anno con facet_wrap
ggplot(real_estate_data)+
  geom_col(aes(
    x = factor(month),
    y = sales,
    fill = city),
    position = "stack") +
  labs(title="Grafico a barre sovrapposte delle vendite totali per mese, città e anno",
       x="Mese",
       y="Numero totale di vendite",
       fill = "Città") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  facet_wrap(~year)

#grafico a barre normalizzato per ogni anno con facet_wrap
ggplot(real_estate_data)+
  geom_col(aes(
    x = factor(month),
    y = sales,
    fill = city),
    position = "fill") +
  labs(title="Grafico a barre sovrapposte delle vendite totali per mese, città e anno",
       x="Mese",
       y="Numero totale di vendite",
       fill = "Città") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  facet_wrap(~year)


#4)line chart del numero totale di annunci attivi per confronti tra città e periodi storici

#line chart con facet_wrap
ggplot(real_estate_data) +
  geom_line(aes(
    x = month,
    y = listings,
    color = city),
    size = 1) +
  geom_point(aes(
    x = month, 
    y = listings,
    color = city),
    size = 1.5) +
  labs(title = "Line chart del numero totale di annunci attivi per città, mese e anno",
       x = "Mese",  
       y = "Numero totale di annunci attivi") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  scale_x_continuous(breaks = seq(0,12,1)) +
  facet_wrap(~year)

#line chart con tutto il periodo storico sull'asse x
ggplot(real_estate_data) +
  geom_line(aes(
    x = (year - min(year)) * 12 + month,
    y = listings,
    color = city),
    size = 1) +
  geom_point(aes(
    x = (year - min(year)) * 12 + month,
    y = listings,
    color = city),
    size = 1.5) +
  geom_vline(xintercept = c(1,12,24,36,48,60), 
             linetype = "dashed", 
             color = "gray30") + 
  labs(x = "Mese",
       y = "Numero totale di annunci attivi",
       title = "Line chart del numero totale di annunci attivi per città e mese") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  scale_x_continuous(breaks = seq(1, 60, 1),
                     labels = rep(c(1:12), 5)) +
  annotate("text", 
           x = c(6,18,30,42,54), 
           y = -3, 
           label = c("2010", "2011", "2012", "2013", "2014"), 
           size = 4)
  







