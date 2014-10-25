plot1 <- function(workDirStr){
    setwd(workDirStr)
    NEI <- readRDS("./data/summarySCC_PM25.rds")
    #Calculate the total Emissions in year 1999, 2002, 2005, 2008
    NEI_by_year <- aggregate(Emissions ~ year, data = NEI, FUN = sum)
    # Draw the plot using line to see the trend of Emissions through the year
    plot(
            #Convert Emissions from tons -> kilotons (divide by 1000)
            NEI_by_year$year,NEI_by_year$Emissions/1000, 
            main = "Total Emissions decreased in 1999 - 2008", 
            xlab = "Year",
            ylab = "Emissions (kilotons)",
            pch=20, 
            type = 'l',
    )
}

