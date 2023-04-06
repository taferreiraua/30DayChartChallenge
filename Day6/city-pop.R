# packages
pacman::p_load(tidyverse,
               countrycode,
               ggbump,
               ggtext,
               ggview)




# dados
df = read.csv('https://raw.githubusercontent.com/owid/owid-datasets/master/datasets/City%20populations%20(1950-2035)%20-%20UN%20Urbanization%20Prospects%20(2018)/City%20populations%20(1950-2035)%20-%20UN%20Urbanization%20Prospects%20(2018).csv')

# https://stackoverflow.com/questions/49435847/extracting-country-name-from-city-name-in-r
city_country = read.csv("https://raw.githubusercontent.com/girijesh18/dataset/master/City_and_province_list.csv")
city_country <- city_country[!duplicated(city_country$City), ]





# texto
sysfonts::font_add('fb', 'fontes/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add_google("Oranienbaum", "oranienbaum")
sysfonts::font_add_google("Open Sans","opensans")
showtext::showtext_opts(dpi=150)
showtext::showtext_auto()

title = paste0("<span style='font-family:oranienbaum;font-size:95pt;color:#111111;'>", 
               'The rise of the mega-cities</span>',
               "<span style='font-family:opensans;font-size:36pt;color:#111111;'>", 
               '<br>By 2035, _**Delhi**_ will be the largest city in the world, followed by 3 other Asian cities.')

subtitle = paste0("<span style='font-family:opensans;font-size:30pt;color:#111111;'>", "Data from _Our World In Data_. City population information is available for the world's 30 largest cities by population (as of 2015).<br>",
                                                                                       'Estimates are available at 5-year intervals from 1950 to 2015. Projections are also available for the year 2035, as published by<br>',
                                                                                       'the _UN World Urbanization Prospects (2018)_, based on its median fertility scenario and urbanization trends.')

caption = paste0(
  "<span style='font-family:fb;font-size:20pt;color:#111111;'>&#xf099;</span>",
  "<span style='font-family:opensans;font-size:25pt;color:#111111;'> @taferreiraua </span>",
  "<span style='font-family:fb;font-size:20pt;color:#111111;'>&#xf09b;</span>",
  "<span style='font-family:opensans;font-size:25pt;color:#111111;'> taferreiraua</span>")






# manipulação de dados
city.pop = df |>
  filter(Year %in% c(1990, 2000, 2010, 2020, 2035)) |>
  mutate(Population = case_when(!is.na(City.population..UN.Urbanization.Prospects..2018.) ~ City.population..UN.Urbanization.Prospects..2018.,
                                TRUE ~ Projected.city.population..UN.Urbanization.Prospects..2018.)) |>
  select(Year, Entity, Population) |>
  group_by(Year) |>
  mutate(Rank = rank(-Population, ties.method = "first")) |>
  ungroup() |>
  mutate(Country = countrycode(sourcevar = Entity, origin='City', destination='Country', custom_dict = city_country),
         Continent = countrycode(sourcevar = Country, origin='country.name', destination='continent')) 

prospect = city.pop |>
  filter(Year<=2020)

projected = city.pop |>
  filter(Year>=2020)






# plot
ggplot() +
  geom_bump(data = prospect,
            mapping = aes(Year, Rank, group=Entity, color=Continent),
            linewidth=.8,
            linetype = 'solid') +
  geom_bump(data = projected,
            mapping = aes(Year, Rank, group=Entity, color=Continent),
            linewidth=.8,
            linetype = 'dashed') +
  geom_point(data = city.pop |> filter(Year==1990),
             aes(x=Year, y=Rank, color=Continent),
             size=3.3) +
  geom_point(data = city.pop |> filter(Year==2035),
             aes(x=2035, y=Rank, color=Continent),
             shape=21,
             fill='#edf2f4',
             size=2.2,
             stroke=1.1) +
  geom_richtext(data = city.pop |> filter(Year==1990),
            aes(x=1989, y=Rank, 
                label=paste0("<b><span style='font-size:28pt;font-family:opensans;'>", Entity, 
                             "</b><br><i><span style='font-size:22pt;font-family:opensans;'>", Country)),
            color='grey30',
            hjust=1,
            label.color=NA,
            fill=NA,
            lineheight=.6) +
  geom_richtext(data = city.pop |> filter(Year==2035),
            aes(x=2036, y=Rank, 
                label=paste0("<b><span style='font-size:28pt;'>", Entity, 
                             "</b><br><i><span style='font-size:22pt;'>", Country)),
            color='grey30',
            hjust=0,
            label.color=NA,
            fill=NA,
            lineheight=.6) +
  scale_color_manual(values = c('Asia'='#FFC425',
                                'Americas'='#F37735',
                                'Africa'='#00A1E4',
                                'Europe'='#00B159')) +
  scale_x_continuous(limits=c(1982, 2043),
                     breaks=seq(1990, 2035, 9)) +
  scale_y_reverse(breaks=seq(30, 1, -1),
                     sec.axis = dup_axis()) +
  labs(title=title, x=subtitle, caption=caption) +
  theme_minimal() +
  theme(plot.background = element_rect(fill='grey95', color='grey95'),
        plot.title = element_markdown(hjust=.5, lineheight=2.5),
        plot.caption = element_markdown(hjust=.5),
        plot.margin = margin(t=32, b=20, l=11, r=11),
        panel.grid.major.x = element_line(linewidth=.3, color='grey75'),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'top',
        legend.title = element_blank(),
        legend.text = element_text(size=27, family='opensans'),
        axis.title.x = element_markdown(margin=margin(t=20, b=10)),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=30, family='opensans'),
        axis.text.y = element_text(size=21, family='opensans'))





ggview(units='px', height=5000, width=4000)
ggsave(filename='city-pop.png', units='px', height=5000, width=4000)