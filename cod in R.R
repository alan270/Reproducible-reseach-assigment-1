datos <- read.csv("c:/Users/cmeza/Documents/Documentos varios/Coursera/proyecto modulo 5/Assigment 1/activity.csv", sep="|")
datos <- read.csv("activity.csv")
summary(datos)
=======================================================================
steps_by_day <- aggregate(steps ~ date, datos, sum)
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
rmean <- mean(steps_by_day$steps)
rmedian <- median(steps_by_day$steps)
summary(steps_by_day)
summary(rmean)
summary(rmedian)



========================================================================  
pasos_diarios <- aggregate(steps ~date, datos, sum)
hist(pasos_diarios$steps, main=paste("Total de pasos por dia"), col="blue", xlab="Numero de pasos")
rmean <- mean(pasos_diarios$steps)
rmedian <- median(pasos_diarios$steps)
summary(pasos_diarios)
summary(rmean)
summary(rmedian)


=======================================================================
steps_by_interval <- aggregate(steps ~ interval, datos, mean)
summary(steps_by_interval)
plot(steps_by_interval$interval,steps_by_interval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")
max_interval <- steps_by_interval[which.max(steps_by_interval$steps),1]
summary(max_interval)
=======================================================================
  pasos_intervalo <- aggregate(steps ~ interval, datos, mean)
summary(pasos_intervalo)
plot(pasos_intervalo$interval,pasos_intervalo$steps, type="l", xlab="intervalo", ylab="Numero de pasos", main="Promedio de nuero de pasos por dia por intervalo")
maximo_intervalo <- pasos_intervalo[which.max(pasos_intervalo$steps),1]
summary(maximo_intervalo)
=======================================================================
  incomplete <- sum(!complete.cases(datos))
imputed_data <- transform(datos, steps = ifelse(is.na(datos$steps), steps_by_interval$steps[match(datos$interval, steps_by_interval$interval)], datos$steps))  
summary(imputed_data)
========================================================================  
  incompletos <- sum(!complete.cases(datos))
datos_imputados <- transform(datos, steps = ifelse(is.na(datos$steps), pasos_intervalo$steps[match(datos$interval, pasos_intervalo$interval)], datos$steps))
summary(datos_imputados)

=======================================================================
  datos_imputados[as.character(datos_imputados$date) == "2012-10-01", 1] <- 0
imputed_data[as.character(imputed_data$date) == "2012-10-01", 1] <- 0
=======================================================================
  
  steps_by_day_i <- aggregate(steps ~ date, imputed_data, sum)
hist(steps_by_day_i$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
summary(steps_by_day_i)
#Create Histogram to show difference. 
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="red", xlab="Number of Steps", add=T)
legend("topright", c("Imputado", "No-imputado"), col=c("blue", "red"), lwd=10)  
=======================================================================
  pasos_diarios_i <- aggregate(steps~date, datos_imputados, sum)
hist(pasos_diarios_i$steps, main=paste("Total de pasos por dia"), col="blue", xlab="numero de pasos")
summary(pasos_diarios_i)
#Crear un histograma para mostrar la diferencia
hist(pasos_diarios$steps, main=paste("Total de pasos cada dia"), col="red", xlab="Numero de pasos", add=T)
legend("topright", c("Imputado", "No-imputado"), col=c("blue", "red"), lwd=10)
=======================================================================
  # Calculate new mean and median for imputed data.
  
  rmean.i <- mean(steps_by_day_i$steps)
rmedian.i <- median(steps_by_day_i$steps)  
summary(rmean.i)
summary(rmedian.i)
=======================================================================  
  media_i <- mean(pasos_diarios_i$steps)
mediana_i <- median(pasos_diarios_i$steps)
summary(media_i)
summary(mediana_i)
=======================================================================
  #Calculate difference between imputed and non-imputed data.
  
  mean_diff <- rmean.i - rmean
med_diff <- rmedian.i - rmedian
summary(mean_diff)
summary(med_diff)
====================================================================  
  diferencia_media <- media_i - rmean
diferencia_mediana <- mediana_i - rmedian
summary(diferencia_media)
summary(diferencia_mediana)

======================================================================
  diferencia_total <- sum(pasos_diarios_i$steps) - sum(pasos_diarios$steps)







======================================================================
  weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday")
imputed_data$dow = as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)),weekdays), "Weekday", "Weekend"))
steps_by_interval_i <- aggregate(steps ~ interval + dow, imputed_data, mean)
library(lattice)
xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$dow, main="Average Steps per Day by Interval", xlab="Interval", ylab="Steps",layout=c(1,2), type="l")  
xyplot(pasos_intervalo_i$steps ~ pasos_intervalo_i$interval | pasos_intervalo_i$dow, main="Promedio de pasos por dia por intervalo,", xlab="intervalo", ylab="Pasos", layout= c(1,2), type="l")
===========================================================================  
  weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday")
datos_imputados$dow = as.factor(ifelse(is.element(weekdays(as.Date(datos_imputados$date)), weekdays),"Dias entre semana", "Fin de semana"))
pasos_intervalo_i <- aggregate(steps ~ interval + dow, datos_imputados, mean)
library(lattice)
xyplot(pasos_intervalo_i$steps ~ pasos_intervalo_i$interval | pasos_intervalo_i$dow, main="Promedio de pasos por dia por intervalo,", xlab="intervalo", ylab="Pasos", layout= c(1,2), type="l")

