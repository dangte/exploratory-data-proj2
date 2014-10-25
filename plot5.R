plot5 <- function(workDirStr){
    setwd(workDirStr)
    library(ggplot2)
    NEI <- readRDS("./data/summarySCC_PM25.rds")
    SCC <- readRDS("./data/Source_Classification_Code.rds")
    #Select records from Baltimore city only
    NEI_Baltimore <- subset(NEI, NEI$fips =="24510")
    # in EI.Sector of SCC table we have several category related with Motor Vehicle Sources
    # Mobile - On-Road Gasoline Light Duty Vehicles     
    # Mobile - On-Road Gasoline Heavy Duty Vehicles     
    # Mobile - On-Road Diesel Light Duty Vehicles       
    # Mobile - On-Road Diesel Heavy Duty Vehicles 
    # the grep command is used for subset the category related with Motor Vehicle Sources
    # which acctually is the string start by "Mobile" and end by "Vehicle" 
    sub.SCC <- SCC[grep("^Mobile .* Vehicles$",SCC$EI.Sector),]
    #merge two data frame with the same "SCC" and remove all records
    #which  don't appear in both frames
    NEI_SCC_Baltimore <- merge(NEI_Baltimore,sub.SCC, by.x = "SCC", by.y = "SCC", all = F)
    plot_data <- aggregate(Emissions ~ year, data = NEI_SCC_Baltimore, FUN = sum, na.rm = TRUE)
    pd <- qplot(year, Emissions, data = plot_data, geom = c("point","line"))
    pd + ggtitle("Motor Vehicle Sources changed in Baltimore")
}

