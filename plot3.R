plot3 <- function(workDirStr){
    setwd(workDirStr)
    library(ggplot2)
    NEI <- readRDS("./data/summarySCC_PM25.rds")
    NEI_Baltimore <- subset(NEI, NEI$fips =="24510")
    data_to_plot <- aggregate(Emissions ~ year + type, data = NEI_Baltimore, FUN = sum, na.rm = TRUE)
    p <- qplot(year, Emissions, data = data_to_plot, geom = c("point","line"), color = type)
    p + ggtitle("Emissions by Type in Baltimore city")
}