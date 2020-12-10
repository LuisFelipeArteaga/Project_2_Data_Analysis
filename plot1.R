library(ggplot2)
library(data.table)
library(RColorBrewer)

#-------- Load Dataset-----------------------

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Emissions Total by year
emissions       <- tapply(NEI$Emissions, list(NEI$year), sum)
Emissions_Total <- as.data.frame(emissions)

# Plot
jBrewColors <- brewer.pal(n = 8, name = "Dark2")

plot <- ggplot(Emissions_Total, aes(x = c(1999, 2002, 2005,2008), y = emissions/1000))+
        geom_bar(stat = "identity", fill = jBrewColors[c(1,2,6,7)]) + 
        theme_classic()+
        labs(x = "Years", y ="Total Emissions PM2.5 (Kilotons)")+
        ggtitle("Total Emissions PM2.5")+
        geom_text(aes(label=round(emissions/1000), digits = 7), vjust=-.3, color="black",size=5)+
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
png("plot1.png")
print(plot)
dev.off()
        