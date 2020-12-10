library(ggplot2)
library(data.table)
library(RColorBrewer)

#-------- Load Dataset-----------------------

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Emissions Total by year
SCC_sub         <- subset(SCC,grepl("Fuel Comb.*Coal",EI.Sector))
NEI_sub         <- subset(NEI, SCC %in% SCC_sub$SCC)
emissions       <- tapply(NEI_sub$Emissions, list(NEI_sub$year), sum)
Emissions_Total <- as.data.frame(emissions)

# Plot
jBrewColors <- brewer.pal(n = 8, name = "Spectral")

plot <- ggplot(Emissions_Total, aes(x = c(1999, 2002, 2005,2008), y = emissions/1000))+
        geom_bar(stat = "identity", fill = jBrewColors[c(1,2,3,4)]) + 
        theme_classic()+
        labs(x = "Years", y ="Total Emissions PM2.5 (Kilotons)")+
        ggtitle("Emissions from Coal Combustion-related Sources")+
        geom_text(aes(label=round(emissions, digits = 2), digits = 7), vjust=-.3, color="black",size=5)+
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
png("plot4.png")
print(plot)
dev.off()
