plot2 <- function(workDirStr){
  setwd(workDirStr)
  NEI <- readRDS("./data/summarySCC_PM25.rds")
  # Select the data from Baltimore only; fips = 24510
  NEI_Baltimore <- subset(NEI, NEI$fips =="24510")
  #Calculate the total Emissions in year 1999, 2002, 2005, 2008 for Baltimore city
  NEI_Baltimore_by_year <- aggregate(Emissions ~ year, data = NEI_Baltimore, FUN = sum)
  # Draw the plot using line to see the trend of Emissions through the year
  plot(
    NEI_Baltimore_by_year$year,NEI_Baltimore_by_year$Emissions, 
    main = "Total Emissions decreased in Baltimore city", 
    xlab = "Year",
    ylab = "Emissions (tons)",
    pch=20, 
    type = 'l',
  )
}
