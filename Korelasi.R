library(ggpubr)
library(dplyr)
library(tidyr)
DataJakarta1 <- Daily_Update_Data_Agregat_Covid_19_Jakarta
ggqqplot(DataJakarta1$`Positif Harian`)
hist(DataJakarta1$`Positif Harian`)
outlier = boxplot.stats(DataJakarta1$`Positif Harian`)$out
DataJakarta2 <- DataJakarta1 %>% mutate(`Positif Harian` = replace(`Positif Harian`, match(outlier, `Positif Harian`),NA))
DataJakartaClean <- drop_na(DataJakarta2)
hist(DataJakartaClean$`Positif Harian`)
ggqqplot(DataJakartaClean$`Positif Harian`)
ggqqplot(DataJakartaClean$`Sembuh Harian`)
outlier1 = boxplot.stats(DataJakartaClean$`Sembuh Harian`)$out
length(outlier1)
DataJakarta3 <- DataJakartaClean %>% mutate(`Sembuh Harian` = replace(`Sembuh Harian`, match(outlier1, `Sembuh Harian`), NA))
DataJakartaClean1 <-drop_na(DataJakarta3)
ggqqplot(DataJakartaClean1$`Sembuh Harian`)
ggqqplot(DataJakartaClean1$`Positif Harian`)
outlier3 = boxplot.stats(DataJakartaClean1$`Positif Harian`)$out
length(outlier3)
DataJakarta4 <- DataJakartaClean1 %>% mutate(`Positif Harian`=replace(`Positif Harian`, match(outlier3, `Positif Harian`),NA))
DataJakartaClean2 <- drop_na(DataJakarta4)
ggqqplot(DataJakartaClean2$`Positif Harian`)
outlier4 = boxplot.stats(DataJakartaClean2$`Sembuh Harian`)$out
length(outlier4)
DataJakarta5 <- DataJakartaClean2 %>% mutate(`Sembuh Harian` = replace(`Sembuh Harian`, match(outlier4, `Sembuh Harian`), NA))
DataJakartaClean3 <- drop_na(DataJakarta5)
ggqqplot(DataJakartaClean3$`Sembuh Harian`)
DataDone <- DataJakartaClean3
ggscatter(DataDone, x="Positif Harian", y="Sembuh Harian", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson")
korelasi <- cor.test(DataDone$`Positif Harian`, DataDone$`Sembuh Harian`, method = "spearman")
korelasi
