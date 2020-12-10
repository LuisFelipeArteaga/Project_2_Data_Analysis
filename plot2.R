library(ggplot2)
library(data.table)
library(RColorBrewer)

#-------- Load Dataset-----------------------

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Emissions Total by year
NEI_sub         <- subset(NEI,fips == "24510")   
emissions       <- tapply(NEI_sub$Emissions, list(NEI_sub$year), sum)
Emissions_Total <- as.data.frame(emissions)

# Plot
jBrewColors <- brewer.pal(n = 8, name = "Dark2")

plot <- ggplot(Emissions_Total, aes(x = c(1999, 2002, 2005,2008), y = emissions))+
        theme_classic()+
        geom_bar(stat = "identity", fill = jBrewColors[c(1,2,3,4)]) + 
        labs(x = "Years", y ="Total Emissions PM2.5")+
        ggtitle("Total Emissions PM2.5 in Baltimore City")+
        geom_text(aes(label=round(emissions), digits = 7), vjust=-.3, color="black",size=5)+
        scale_x_discrete(limits = c(1999, 2002, 2005,2008))+
        theme(
                panel.background = element_rect( fill = "white"),
                plot.margin = margin(2, 2, 1, 1, "cm"),
                plot.title = element_text(hjust = 0.5),
                axis.title = element_text(face = "bold", size = 12),
                axis.text.x = element_text(face="bold", color="black", 
                                           size=10),
                axis.text.y = element_text(face="bold", color="black", 
                                           size=10)
        )
plot
png("plot2.png")
print(plot)
dev.off()
