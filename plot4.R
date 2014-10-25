plot4 <- function(workDirStr){
    setwd(workDirStr)
    library(ggplot2)
    NEI <- readRDS("./data/summarySCC_PM25.rds")
    SCC <- readRDS("./data/Source_Classification_Code.rds")
    # in EI.Sector of SCC table we have several category related with Coal
    # Fuel Comb - Electric Generation - Coal
    # Fuel Comb - Industrial Boilers, ICEs - Coal
    # Fuel Comb - Comm/Institutional - Coal
    # the grep command is used for subset the category related with Coal Combustion
    # which acctually is the string start by "Fual Comb" and end by "Coal"
    sub.SCC <- SCC[grep("^Fuel Comb .* Coal$",SCC$EI.Sector),]
    #merge two data frame with the same "SCC" and remove all records
    #which  don't appear in both frames
    NEI_SCC <- merge(NEI,sub.SCC, by.x = "SCC", by.y = "SCC", all = F)
    # Calculate the Emisson by year
    data_to_plot <- aggregate(Emissions ~ year, data = NEI_SCC, FUN = sum, na.rm = TRUE)
    # draw the result
    pl <- qplot(year, Emissions, data = data_to_plot, geom = c("point","line"))
    pl + ggtitle("Coal Combustion Sources across U.S 1999-2008")
}

