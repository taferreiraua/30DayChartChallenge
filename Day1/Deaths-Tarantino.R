# packages
pacman::p_load(tidyverse, 
               ggpattern, 
               ggtext,
               ggview)



# dados
df = read.csv("C:/Users/Thays Ferreira/Downloads/tarantino.csv")



# texto
sysfonts::font_add('fb', 'fontes/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add_google("Open Sans","opensans")
sysfonts::font_add('benguiat', 'fontes/BenguiatITCbyBT-Bold.otf')
sysfonts::font_add('bloody', 'fontes/BLOODY.ttf')
sysfonts::font_add_google("Headland One", "headland")
showtext::showtext_auto()
showtext::showtext_opts(dpi=150)


title = paste0("<span style='font-family:bloody;font-size:66pt;color:red3;'>Deaths<br></span>",
               "<span style='font-family:benguiat;font-size:46pt;color:#000100;'>WRITTEN AND<br>DIRECTED BY<br></span>",
               "<span style='font-family:benguiat;font-size:76pt;color:#000100;'>QUENTIN<br>TARANTINO<br></span>",
               "<span style='font-family:headland;font-size:17pt;color:#000100;'>[Number of deaths per movie]</span>")

caption = paste0("<span style='font-size:17pt;font-family:fb;color:#000100;'>&#xf099;</span>",
                 "<span style='font-size:18pt;font-family:opensans;color:#000100;'> @taferreiraua </span>",
                 "<span style='font-size:17pt;font-family:fb;color:#000100;'> &#xf09b;</span>",
                 "<span style='font-size:18pt;font-family:opensans;color:#000100;'> taferreiraua </span>",
                 "<span style='font-size:18pt;font-family:opensans;color:#000100;'> | Data from <b><i>FiveThirtyEight</span>")



# manipulação de dados
tarantino = df |>
  filter(type=='death') |>
  group_by(movie) |>
  mutate(count = n()) |>
  ungroup() |>
  distinct(movie, count) |>
  arrange(desc(count)) |>
  mutate(perc = round(count*100/sum(count), 1),
         lab.pos = cumsum(perc)-.5*perc,
         filename = paste0(str_remove(str_remove(movie, ' '), ': Vol. '), ".jpg"),
         links = paste0("C:/Users/Thays Ferreira/Documents/Visualização de dados/30DaysChartChallenge/Day1/Images/", filename),
         movie = case_when(!grepl('Kill Bill', movie) ~ str_replace(movie, ' ', '<br>'),
                           TRUE ~ str_replace(movie, ':', '<br>')),
         movie_label = paste0("<span style='font-family:benguiat;font-size:25pt;color:#000100;'>", movie, "<br></span>",
                              "<span style='font-family:headland;font-size:18pt;color:#000100;'>[", count, "]</span>"))



# plot
ggplot(data = tarantino, 
       aes(x = 2, y = perc, fill = as.factor(perc))) +
  geom_bar_pattern(aes(pattern_angle = as.factor(perc), pattern_filename=as.factor(perc)), 
    pattern = 'image',
    pattern_type = 'expand',
    colour = '#FEB81C',
    stat = 'identity',
    linewidth=1
  ) +
  geom_richtext(aes(x=0, y=0, label=title),
                fill=NA, 
                label.color=NA) +
  geom_richtext(aes(x=2.9, y=lab.pos, label=movie_label),
                fill=NA,
                label.color=NA) +
  coord_curvedpolar("y", start = 200) +
  xlim(0,2.9) +
  labs(caption=caption) +
  theme_minimal() +
  theme(plot.background = element_rect(fill='#FEB81C', color='#FEB81C'),
        plot.caption = element_markdown(),
        legend.position = 'none',
        legend.text = element_text(color='#000100', size=35),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank()) +
  scale_pattern_filename_discrete(choices = links) 



ggview(units='px', height=3000, width=3000)
ggsave(filename='Deaths-Tarantino.png', units='px', height=3000, width=3000)
