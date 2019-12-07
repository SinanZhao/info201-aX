library("dplyr")
library("ggplot2")

df <- read.csv("master.csv")

suicide_by_year <- df %>%
  group_by(year) %>%
  summarize(num = sum(suicides_no))

gdp_by_year <- df %>%
  group_by(year) %>%
  summarise(sum = sum(gdp_per_capita....))

gdp_by_year_plot <- ggplot(data = gdp_by_year) +
  geom_col(
    mapping = aes(
      x = year,
      y = sum, fill = year
    ),
    width = 0.5
  ) +
  labs(
    title = "The Trend of GDP Per Capita from 1985 to 2018",
    x = "Year",
    y = "GDP Per Capita"
  )

suicides_by_year_plot <- ggplot(data = suicide_by_year) +
  geom_col(
    mapping = aes(
      x = year,
      y = num, fill = year
    ),
    width = 0.5
  ) +
  labs(
    title = "The Trend of Suicides from 1985 to 2018",
    x = "Year",
    y = "The Number of Suicides"
  )

suicide_wrt_gdp_per_capita <- df %>%
  group_by(gdp_per_capita....) %>%
  summarize(sum = sum(suicides_no)) %>%
  arrange(gdp_per_capita....) %>%
  top_n(500)

suicide_wrt_gdp_plot <- ggplot(data = suicide_wrt_gdp_per_capita) +
  geom_point(
    mapping = aes(
      x = gdp_per_capita....,
      y = sum
    )
  ) +
  labs(
    title = "The Relationship Between Suicides and GDP Per Capita",
    x = "GDP Per Capital",
    y = "The Number of Suicides",
    color = "gdp_per_capita...."
  )
