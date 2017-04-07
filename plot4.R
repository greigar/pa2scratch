# 4. Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

# NEI <- readRDS("summarySCC_PM25.rds")
# SCC <- readRDS("Source_Classification_Code.rds")

scc_coal <- SCC %>% filter( grepl("coal", EI.Sector, ignore.case = TRUE) ) %>% select(SCC, EI.Sector)
nei_coal <- inner_join(NEI, scc_coal, by = "SCC")

yearly_emissions_coal <- nei_coal %>%
                         group_by(year, EI.Sector) %>%
                         summarise(total_emissions = sum(Emissions)) %>%
                         mutate(EI.Sector = sub(" - Coal", "", EI.Sector) ) # remove coal text to make plot titles fit

ggplot(yearly_emissions_coal, aes(x = year, y = total_emissions )) +
  geom_line() +
  facet_grid(.~EI.Sector) +
  labs(x = "Year", y = expression(PM[2.5]~"Emissions (tons)")) +
  ggtitle(expression(PM[2.5]~"Emissions from Coal "))

ggsave("plot4.png")


