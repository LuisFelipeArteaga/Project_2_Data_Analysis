library(ggplot2)
library(data.table)
library(RColorBrewer)

#-------- Load Dataset-----------------------

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Emissions Total by year
NEI_sub              <- subset(NEI,fips %in% c("24510","06037") & type == "ON-ROAD")   
emissions            <- tapply(NEI_sub$Emissions, c(list(NEI_sub$year),list(NEI_sub$fips)), sum)
Emissions_Total      <- as.data.frame(emissions)
Emissions_Total$year <- c(1999, 2002, 2005,2008)
Emissions_Total      <- data.frame(year = Emissions_Total$year,stack(Emissions_Total[1:2]))

# Plot
jBrewColors <- brewer.pal(n = 8, name = "Spectral")
to_string <- as_labeller(c("24510" = "Baltimore City", "06037" = "Los Angeles"))
plot <- ggplot(Emissions_Total, aes(x = as.character(year), y = values))+
        geom_bar(stat = "identity", fill = rep(jBrewColors[c(1,2,3,4)],2)) + 
        facet_grid(ind~., scales="free",labeller = to_string)+
        theme_classic()+
        labs(x = "Years", y ="Total Emissions PM2.5 (Tons)")+
        ggtitle("Motor Vehicle Emission from 1999–2008 in Baltimore City and Los Angeles")+
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
png("plot6.png")
print(plot)
dev.off()