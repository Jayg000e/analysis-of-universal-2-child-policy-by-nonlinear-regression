library(readxl)
population_data <- read_excel("~/statistic data/dongguan/DongGuan registered population data.xlsx")
View(population_data)

population_data_ts <- list()
for(item in colnames(population_data)){
  if(item!='years'){
    population_data_ts[[item]] <-  window(ts(population_data[[item]],start=1978),start=2002)
  }
}

plot(window(ts(population_data[-1],start=1978),start=2002))
plot(population_data_ts$birth_rate)

#truncate years by 2002 where the birth rate seems to become stable
birth_rate <- population_data_ts$birth_rate
years <- c(2002:2020)

#tsingle is t1
single <- ifelse(years>2011,1,0)
tsingle <- cumsum(single)-1
#tuniversal is t2
universal <- ifelse(years>2016,1,0)
tuniversal <- cumsum(universal)-1

fit1 <- nls(birth_rate~base_rate
+prompt1*single*exp(-tsingle*decay1)
+prompt2*universal*exp(-tuniversal*decay2),
start=list(prompt1=2,prompt2=4,decay1=1,decay2=0.5,base_rate=10))
summary(fit1)

fit2 <- nls(birth_rate~base_rate
+prompt1*single
+prompt2*universal*exp(-tuniversal*decay2),
start=list(prompt1=2,prompt2=4,decay2=0.5,base_rate=10))
summary(fit2)
