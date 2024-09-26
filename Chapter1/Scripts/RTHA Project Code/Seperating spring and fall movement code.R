filtered <- filter(raw, timestamp >= "2021-03-01 00:01:00.000")
spring <- filter(filtered, timestamp <= "2021-05-31 00:01:00.000")
fall <- filter(filtered, timestamp >= "2021-07-31 00:01:00.000")
s <- 1
f <- 2
spring$season <- s
fall$season <- f
spring$season<-as.factor(spring$season)
fall$season<-as.factor(fall$season)
total <- rbind(spring, fall)