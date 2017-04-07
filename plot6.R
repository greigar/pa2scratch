# 6. Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle
#    sources in Los Angeles County, California (𝚏𝚒𝚙𝚜 == "𝟶𝟼𝟶𝟹𝟽").
#    Which city has seen greater changes over time in motor vehicle emissions?


# NEI <- readRDS("summarySCC_PM25.rds")
# SCC <- readRDS("Source_Classification_Code.rds")

scc_vehicle <- SCC %>% filter( grepl("Mobile.*Vehicles.*", EI.Sector, ignore.case = TRUE) ) %>% select(SCC)
emissions   <- NEI %>% filter(fips == "24510" | fips == "06037")

cities <- c("24510" = "Baltimore", "06037" = "LA")
emissions$City <- cities[emissions$fips]
nei_vehicle <- inner_join(emissions, scc_vehicle, by = "SCC" )

yearly_emissions_vehicle <- nei_vehicle %>% group_by(City,year) %>% summarise(total_emissions = sum(Emissions))

change_from_1999 <- function(x) { round(x$total_emissions - x[x$year == 1999,]$total_emissions, 2) }

yearly_emissions_vehicle$cf1999 <- c( yearly_emissions_vehicle %>% filter(City == "Baltimore")  %>% change_from_1999,
                                     yearly_emissions_vehicle %>% filter(City == "LA")         %>% change_from_1999 )

ggplot(yearly_emissions_vehicle, aes(x = year, y = total_emissions, label = cf1999, fill = factor(year))) +
  geom_bar( stat = "identity" )  +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Year", y = "Emissions") +
  guides(fill = guide_legend(title = "Year")) +
  facet_grid(.~City) +
  geom_hline(data = baseline, aes(yintercept = year_1999), color = "#E41A1C") + # first colour from Set1
  scale_x_discrete( limits = c(1999,2002,2005,2008) )  +
  geom_text(size = 3, vjust = 0,  aes(colour = cf1999 <= 0 ) , nudge_x = -0.1 )  +
  ggtitle("PM[2.5] Yearly Emissions - Showing change against 1999 Levels")

ggsave("plot6-bar-4.png")

