library(ggplot2)
library(data.table)
library(RColorBrewer)

#-------- Load Dataset-----------------------

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Emissions Total by year
NEI_sub              <- subset(NEI,fips == "24510")   
emissions            <- tapply(NEI_sub$Emissions, c(list(NEI_sub$year),list(NEI_sub$type)), sum)
Emissions_Total      <- as.data.frame(emissions)
Emissions_Total$year <- c(1999, 2002, 2005,2008)
Emissions_Total      <- data.frame(year = Emissions_Total$year,stack(Emissions_Total[1:4]))

# Plot
jBrewColors <- brewer.pal(n = 8, name = "Dark2")

plot <- ggplot(Emissions_Total, aes(x = as.character(year), y = values))+
        geom_bar(stat = "identity", fill = rep(jBrewColors[c(3,2,5,6)],4)) + 
        facet_grid(.~ind)+
        theme_classic()+
        labs(x = "Years", y ="Total Emissions PM2.5")+
        ggtitle("Total Emissions PM2.5 in Baltimore City by Various Types")+
        geom_text(aes(label=round(values,digits = 2), digits = 7), vjust=-.3, color="black",size=3)+
        theme(
                panel.background = element_rect( fill = "white"),
                strip.text.x = element_text(size = 10, color = "black", face = "bold"),
                strip.background = element_rect(color="white", size=0, linetype="solid"),
                plot.margin = margin(2, 2, 1, 1, "cm"),
                plot.title = element_text(hjust = 0.5),
                axis.title = element_text(face = "bold", size = 10),
                axis.text.x = element_text(face="bold", color="black", 
                                           size=10),
                axis.text.y = element_text(face="bold", color="black", 
                                           size=10)
        )
plot
png("plot3.png",width = 680,height = 480)
print(plot)
dev.off()