plot6 <- function(workDirStr){
    setwd(workDirStr)
    library(ggplot2)
    # Read data from sources
    NEI <- readRDS("./data/summarySCC_PM25.rds")
    SCC <- readRDS("./data/Source_Classification_Code.rds")
    
    #Read data for Baltimore only
    NEI_Baltimore <- subset(NEI, NEI$fips =="24510")
    #Read data for Los Angeles Count only
    NEI_LAC <- subset(NEI, NEI$fips =="06037")
    
    # Read the Motor Vehicle Source Category only from SCC data frame
    # Mobile - On-Road Gasoline Light Duty Vehicles     
    # Mobile - On-Road Gasoline Heavy Duty Vehicles     
    # Mobile - On-Road Diesel Light Duty Vehicles       
    # Mobile - On-Road Diesel Heavy Duty Vehicles 
    sub.SCC <- SCC[grep("^Mobile .* Vehicles$",SCC$EI.Sector),]
    
    #merge two data frame with the same "SCC" and remove all records
    #which  don't appear in both frames
    # Baltimore region only
    NEI_SCC_Baltimore <- merge(NEI_Baltimore,sub.SCC, by.x = "SCC", by.y = "SCC", all = F)
    
    #merge two data frame with the same "SCC" and remove all records
    #which  don't appear in both frames
    # Los Angeles Count region only
    NEI_SCC_LAC <- merge(NEI_LAC,sub.SCC, by.x = "SCC", by.y = "SCC", all = F)
    
    # Caculate total motor vehicle source emissions from Baltimore
    baltimore <- aggregate(Emissions ~ year, data = NEI_SCC_Baltimore, FUN = sum, na.rm = TRUE)
    #Add city name
    baltimore[3] <- "Baltimore"
    
    # Caculate total motor vehicle source emissions from Los Angeles Count
    lac <- aggregate(Emissions ~ year, data = NEI_SCC_LAC, FUN = sum, na.rm = TRUE)
    #Add city name
    lac[3] <- "Los Angeles Count"
    
    # Combine total from Baltimore and total from Los Angeles Count
    dtp <- rbind(baltimore,lac)
    #Rename the third column
    names(dtp)[3] <- "City"
    # Draw the plot
    dp <- qplot(year, Emissions, data = dtp, geom = c("point","line"), color = City)
    dp + ggtitle("Motor Vehicle Sources from Baltimore and Los Angeles Count")
}
  

