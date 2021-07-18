library(readxl)
Daily_Update_Data_Agregat_Covid_19_Jakarta <- read_excel("C:/Users/Fabianus Fangri/Downloads/Daily Update Data Agregat Covid-19 Jakarta.xlsx",sheet = "Data Indonesia dan Jakarta")
View(Daily_Update_Data_Agregat_Covid_19_Jakarta)
mean(Daily_Update_Data_Agregat_Covid_19_Jakarta$`Positif Harian (Jakarta)`)
median(Daily_Update_Data_Agregat_Covid_19_Jakarta$`Positif Harian (Jakarta)`)
modusfunc <- function(x){
  unikx<-unique(x)
  unikx[which.max(tabulate(match(x,unikx)))]
}
modus <- modusfunc(Daily_Update_Data_Agregat_Covid_19_Jakarta$`Positif Harian (Jakarta)`)
print(modus)
min(Daily_Update_Data_Agregat_Covid_19_Jakarta$`Positif Harian (Jakarta)`)
max(Daily_Update_Data_Agregat_Covid_19_Jakarta$`Positif Harian (Jakarta)`)
boxplot(Daily_Update_Data_Agregat_Covid_19_Jakarta$`Positif Harian (Jakarta)`,main="Positif Harian (Jakarta)")
outlier = boxplot.stats(Daily_Update_Data_Agregat_Covid_19_Jakarta$`Positif Harian (Jakarta)`)$out
outlier
length(outlier)
