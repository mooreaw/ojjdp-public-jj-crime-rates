library(tidyverse)
library(glue)
library(viridis)
library(scales)

# the online statistical briefing book has data from 2000 to 2016
yrs  <- 2000:2016
fils <- glue("https://www.ojjdp.gov/ojstatbb/crime/qa05103.asp?qaDate={yrs}&export=yes")

# helper to do some minor formatting/cleaning of each file
crime_getter <- function(link) {
  link %>%
    read_csv(skip = 2) %>%
    gather(index, rate, -State, -`Reporting Coverage`) %>%
    mutate(
      year = link %>%
        str_extract("=(.*)&") %>%
        str_replace_all("=|&", "") %>%
        as.numeric()
    )
}

# download each file, but give a few seconds of waiting in between each call
jjc <- map_df(fils, function(link) {
  Sys.sleep(5)
  crime_getter(link)
})

# minor cleanup of names/cols
jjc <- jjc %>%
  rename(state = State, reporting_coverage = `Reporting Coverage`) %>%
  mutate(reporting_coverage = as.numeric(str_replace(reporting_coverage, "%", "")))

# there's some footer information in each year's file-- will drop
jjc <- filter(jjc, nchar(state) < 30)

# plot the data for michigan
p0 <- jjc %>%
  filter(state == "Michigan") %>%
  ggplot(aes(x = year, y = rate, color = index)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = viridis(7)) +
  scale_y_continuous(labels = comma) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = c(.8, .8),
    legend.title = element_blank()
  ) +
  labs(
    x = "",
    y = "Rate per 100,000 persons between the ages of 10-17",
    title = "Juvenile Arrest Rates in Michigan, 2000 - 2016",
    caption = "Source: OJJDP Statistical Briefing Book (Available Online)"
  )

p0

# save the plot
ggsave("jj-arrest-rates-mi-00-16.png", p0)

# save the data
write_csv(jjc, "data/ojjdp-jj-arrest-rates-00-16.csv")
