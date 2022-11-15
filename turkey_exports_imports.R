#loading libraries and fetching the data
library(readxl)
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
library(httr)

GET("https://data.tuik.gov.tr/Bulten/DownloadIstatistikselTablo?p=FFtxWO6rlKXiYSrLPZ9r07XvpdPN6M6/DxdggC86w18EvXvJYn4VpzME/BOda7za", write_disk(tf1 <- tempfile(fileext = ".xls")))
GET("https://data.tuik.gov.tr/Bulten/DownloadIstatistikselTablo?p=bFUB7h8ZquNCPi4DjUtCm1lyXYoOyG9t5emG2Z5BW0uOVBwFcIZHImqPYjndnVeR", write_disk(tf2 <- tempfile(fileext = ".xls")))

exports <- read_excel(tf1)
imports <- read_excel(tf2)

#manipulating data
##exports
exports <- exports[,2:4]
exports <- subset(exports, ...2=="A" |
                    ...2=="B" |
                    ...2=="C" |
                    ...2=="D" |
                    ...2=="E" |
                    ...2=="H" |
                    ...2=="J" |
                    ...2=="K" |
                    ...2=="M" |
                    ...2=="R")

exports_main <- cbind(exports[1:10,3])
colnames(exports_main) <- "2022"
exports_main["2021"] <- exports[11:20,3]
exports_main["2020"] <- exports[21:30,3]
exports_main["2019"] <- exports[31:40,3]
exports_main["2018"] <- exports[41:50,3]
exports_main["2017"] <- exports[51:60,3]
exports_main["2016"] <- exports[61:70,3]
exports_main["2015"] <- exports[71:80,3]
exports_main["2014"] <- exports[81:90,3]
exports_main["2013"] <- exports[91:100,3] 
exports_main["ExportCategories"] <- exports[1:10,2]
exports_main <- exports_main[ ,c(11,10,9,8,7,6,5,4,3,2,1)]
exports_main <- melt(exports_main, id.vars = "ExportCategories", variable.name = "Years")
exports_main$value <- as.numeric(exports_main$value)
exports_main$value[is.na(exports_main$value)] <- 0
exports_main$value <- exports_main$value/1000000
exports_main$ExportCategories <- as.factor(exports_main$ExportCategories)

exports_main$value <- round(exports_main$value, digits = 2)
exports_main$Years <- as.Date(exports_main$Years, "%Y")
exports_main$Years <- as.numeric(format(exports_main$Years, "%Y"))

##imports
imports <- imports[,2:4]
imports <- subset(imports, ...2=="A" |
                    ...2=="B" |
                    ...2=="C" |
                    ...2=="D" |
                    ...2=="E" |
                    ...2=="H" |
                    ...2=="J" |
                    ...2=="K" |
                    ...2=="M" |
                    ...2=="R")

imports_main <- cbind(imports[1:10,3])
colnames(imports_main) <- "2022"
imports_main["2021"] <- imports[11:20,3]
imports_main["2020"] <- imports[21:30,3]
imports_main["2019"] <- imports[31:40,3]
imports_main["2018"] <- imports[41:50,3]
imports_main["2017"] <- imports[51:60,3]
imports_main["2016"] <- imports[61:70,3]
imports_main["2015"] <- imports[71:80,3]
imports_main["2014"] <- imports[81:90,3]
imports_main["2013"] <- imports[91:100,3] 
imports_main["ImportCategories"] <- imports[1:10,2]
imports_main <- imports_main[ ,c(11,10,9,8,7,6,5,4,3,2,1)]
imports_main <- melt(imports_main, id.vars = "ImportCategories", variable.name = "Years")
imports_main$value <- as.numeric(imports_main$value)
imports_main$value[is.na(imports_main$value)] <- 0
imports_main$value <- imports_main$value/1000000
imports_main$ImportCategories <- as.factor(imports_main$ImportCategories)

imports_main$value <- round(imports_main$value)
imports_main$Years <- as.Date(imports_main$Years, "%Y")
imports_main$Years <- as.numeric(format(imports_main$Years, "%Y"))

#plotting the graph
##exports
ten_root <- function(x) x ^ (1/6)
power_of_ten <- function(x) x ^ 6

trans_y <- trans_new(name = "ten root",
                     transform = ten_root,
                     inverse = power_of_ten)

p1 <- ggplot(data = exports_main, 
             aes(x = Years, y = value, colour = ExportCategories)) + 
  labs(colour = "Ihracat Kategorileri - Export Categories") + 
  ylab("Exports (in $)") + 
  geom_point() +
  geom_smooth(method = "lm", se = F) + 
  theme(axis.title.x=element_text(colour="Red",size=15),
        axis.title.y=element_text(colour="Red",size=15),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = c(0.5,0.5),
        legend.key.size = unit(0.2, "cm"),
        legend.direction = "vertical") + 
  scale_x_continuous(breaks=c(2013,2014,2015,2016,2017,2018,2019,2020,2021,2022), limits = c(2013, 2022)) + 
  coord_trans(y = trans_y) + 
  scale_y_continuous(breaks = c(0,0.1,0.5,1,2,5,10,20,50,200), limits = c(0,220))
p1

##imports
p2 <- ggplot(data = imports_main, aes(x = Years, y = value, colour = ImportCategories)) + 
  labs(colour = "Ithalat Kategorileri - Import Categories") + 
  ylab("Imports (in $)") + 
  geom_point() +
  coord_cartesian() +
  geom_smooth(method = "lm", se = F) + 
  theme(axis.title.x=element_text(colour="Red",size=15),
        axis.title.y=element_text(colour="Red",size=15),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = c(0.5,0.5),
        legend.key.size = unit(0.2, "cm"),
        legend.direction = "vertical") + 
  scale_x_continuous(breaks=c(2013,2014,2015,2016,2017,2018,2019,2020,2021,2022), limits = c(2013, 2022)) + 
  coord_trans(y = trans_y) + 
  scale_y_continuous(breaks = c(0,0.1,0.5,1,2,5,10,20,50,200), limits = c(0,220))
p2

#comparing imports and exports
netx <- exports_main
colnames(netx) <- c("Categories", "Years", "Exports")
netx$Imports <- imports_main$value

p3 <- ggplot(data = netx, aes(x = Years, y = Exports, colour = Categories)) + 
  labs(colour = "Kategoriler - Categories") + 
  ylab("Imports and Exports (in $)") + 
  geom_line(aes(x = Years, y = Exports, colour = Categories), linewidth = 1, linetype = "dashed") + 
  geom_line(aes(x = Years, y = Imports, colour = Categories), linewidth = 1) + 
  geom_point(colour = "blue", size = 2) + 
  geom_point(aes(x = Years, y = Imports, colour = Categories), colour = "red", size = 2) + 
  theme(axis.title.x=element_text(colour="DarkGreen",size=15),
        axis.title.y=element_text(colour="DarkGreen",size=15),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = c(0.5,0.5),
        legend.key.size = unit(0.2, "cm"),
        legend.direction = "vertical") + 
  scale_x_continuous(breaks=c(2013,2014,2015,2016,2017,2018,2019,2020,2021,2022), limits = c(2013, 2022)) + 
  coord_trans(y = trans_y) + 
  scale_y_continuous(breaks = c(0,0.1,0.5,1,2,5,10,20,50,200), limits = c(0,220))
p3

#!we observe that despite export is higher than import in some categories, weight of negative net exported categories is higher than others. 

#for a brief conclusion about Turkey's balance, we compare total imports and exports
##manipulating data
GET("https://data.tuik.gov.tr/Bulten/DownloadIstatistikselTablo?p=zdt14y3/HREY4LFmhL9s6W1a4i/Xl7kZnmGL6H/DPsBv6yg8Wr/J3s3r8OXhOLjQ", write_disk(tf3 <- tempfile(fileext = ".xls")))

totalnetxp <- read_excel(tf3)
totalnetxp <- totalnetxp[c(12:20), ]
totalnetxp <- totalnetxp[, -c(4,7,9,11)]
colnames(totalnetxp) <- c("Years", "Exports", "ChangeinExports", "Imports", "ChangeinImports", "BOT", "VOT", "ProportionofImportsCoveredbyExports")
totalnetxp[, c(2:8)] <- sapply(totalnetxp[, c(2:8)], as.numeric)
totalnetxp[is.na(totalnetxp)] <- 0
totalnetxp[, c(2,4,6,7)] <- totalnetxp[, c(2,4,6,7)]/1000000
totalnetxp[, c(2:8)] <- round(totalnetxp[, c(2:8)], digits = 2)
totalnetxp$Years <- as.Date(totalnetxp$Years, "%Y")
totalnetxp$Years <- as.numeric(format(totalnetxp$Years, "%Y"))

##plotting the graphs
###exports vs imports
p4 <- ggplot(data = totalnetxp, aes(x = Years, y = Exports)) + 
  labs(colour = "Imports and Exports") + 
  ylab("Imports and Exports (in billion $)") + 
  geom_line(aes(x = Years, y = Exports, colour = "red"), size = 1) + 
  geom_line(aes(x = Years, y = Imports, colour = "blue"), size = 1) + 
  geom_point(colour = "blue", size = 2) + 
  geom_point(aes(x = Years, y = Imports), colour = "red", size = 2) + 
  theme(axis.title.x=element_text(colour="DarkGreen",size=15),
        axis.title.y=element_text(colour="DarkGreen",size=15),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = c(0.5,0.5),
        legend.key.size = unit(0.2, "cm"),
        legend.direction = "vertical") + 
  scale_x_continuous(breaks=c(2013,2014,2015,2016,2017,2018,2019,2020,2021), limits = c(2013, 2021)) + 
  scale_colour_discrete(labels = c("Imports", "Exports"))
p4

#!we can clearly say that after looking at this graph, the statement we made before holds.

###change in exports vs imports
p5 <- ggplot(data = totalnetxp, aes(x = Years, y = ChangeinExports)) + 
  labs(colour = "Change in Imports and Exports") + 
  ylab("Change in Imports and Exports") + 
  geom_line(aes(x = Years, y = ChangeinExports, colour = "red"), size = 1) + 
  geom_line(aes(x = Years, y = ChangeinImports, colour = "blue"), size = 1) + 
  geom_point(colour = "blue", size = 2) + 
  geom_point(aes(x = Years, y = ChangeinImports), colour = "red", size = 2) + 
  theme(axis.title.x=element_text(colour="DarkGreen",size=15),
        axis.title.y=element_text(colour="DarkGreen",size=15),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = c(0.5,0.5),
        legend.key.size = unit(0.2, "cm"),
        legend.direction = "vertical") + 
  scale_x_continuous(breaks=c(2014,2015,2016,2017,2018,2019,2020,2021), limits = c(2014, 2021)) + 
  scale_colour_discrete(labels = c("Change in Imports", "Change in Exports"))
p5

###BOT and VOT
p6 <- ggplot(data = totalnetxp, aes(x = Years, y = BOT)) + 
  labs(colour = "BOT and VOT") + 
  ylab("Values") + 
  geom_line(aes(x = Years, y = BOT, colour = "red"), size = 1) + 
  geom_line(aes(x = Years, y = VOT, colour = "blue"), size = 1) + 
  geom_point(colour = "blue", size = 2) + 
  geom_point(aes(x = Years, y = VOT), colour = "red", size = 2) + 
  theme(axis.title.x=element_text(colour="DarkGreen",size=15),
        axis.title.y=element_text(colour="DarkGreen",size=15),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = c(0.5,0.5),
        legend.key.size = unit(0.2, "cm"),
        legend.direction = "vertical") + 
  scale_x_continuous(breaks=c(2013,2014,2015,2016,2017,2018,2019,2020,2021), limits = c(2013, 2021)) + 
  scale_colour_discrete(labels = c("VOT", "BOT"))
p6

#!we can observe from this graph that Turkey is in trade deficit with medium-low volume of trade.

###Proportion of Imports Covered by Exports
p7 <- ggplot(data = totalnetxp, aes(x = Years, y = ProportionofImportsCoveredbyExports)) + 
  labs(title = "Proportion of Imports Covered by Exports") +
  ylab("Percentage") + 
  geom_line(colour = "black", size = 1) + 
  geom_point(colour = "black", size = 2) + 
  theme(axis.title.x=element_text(colour="DarkGreen",size=15),
        axis.title.y=element_text(colour="DarkGreen",size=15),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        
        plot.title = element_text(size = 25, colour = "darkblue", hjust = 0.5),
        
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = c(0.5,0.5),
        legend.key.size = unit(0.2, "cm"),
        legend.direction = "vertical") + 
  scale_x_continuous(breaks=c(2013,2014,2015,2016,2017,2018,2019,2020,2021), limits = c(2013, 2021)) + 
  scale_y_continuous(breaks = c(60,65,70,75,80,85,90), limits = c(60, 90)) + 
  scale_colour_discrete(labels = c("Proportion of Imports Covered by Exports"))
p7

#!this graph says that Turkey's proportion of imports covered by exports has been volatile with increasing trend but lower that 100% which shows the trade deficit again.