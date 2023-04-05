# packages
pacman::p_load(tidyverse,
               lubridate,
               ggtext,
               ggview)



# dados
df = read.csv("~/planecrashinfo_accident_data.csv")

df = df |>
  mutate(date = strptime(date, format="%B %d, %Y"), 
         year = as.numeric(format(date, "%Y")),
         day = as.numeric(format(date, "%d")),
         month = format(date, "%B"))


# fonte e texto
sysfonts::font_add('fb', 'fontes/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add_google("Oranienbaum", "oranienbaum")
sysfonts::font_add_google("Open Sans","opensans")
showtext::showtext_opts(dpi=150)
showtext::showtext_auto()

title = paste0("<span style='font-family:oranienbaum;font-size:150pt;color:#111111;'>Nepal<br>Plane<br>Crashes<br></span>",
               "<span style='font-size:30pt;color:#111111;font-family:opensans;'>",
               "Since 2000, around **366 lives** have been lost as a result of **19 fatal<br>",
               "air crashes** in Nepal. Pokhara's crash on January 15 is Nepal's deadliest<br>",
               "since 1992, when all 167 people aboard a Pakistan International Airlines<br>",
               "plane were killed when it plowed into a hill as it tried to land in Kathmandu.</span>")

caption <- paste0(
  "<span style='font-family:fb;'>&#xf099;</span>",
  "<span style='font-family:opensans;'> @taferreiraua </span>",
  "<span style='font-family:fb;'>&#xf09b;</span>",
  "<span style='font-family:opensans;'> taferreiraua </span>",
  "<span style='font-family:opensans;'>| Dados: <i>planecrashinfo.com </span>")



# adicionando informações sobre o acidente de Pokhara 2023
df = add_row(df, 
             location='Pokhara, Nepal',
             year=2023,
             day=15,
             month='January',
             fatalities = '72')



# manipulação de dados
crashes = df |> 
  filter(!is.na(fatalities) & 
         grepl("Nepal", location) | grepl("nepal", location) | grepl("Napal", location) | grepl("Near Lete Pass", location)) |>
  mutate(fatalities.ind = str_split_i(fatalities, ' ', 1),
         fatalities = as.numeric(fatalities.ind),
         location = str_split_i(location, ',', 1)) |>
  distinct(year, day, month, fatalities, fatalities, location) |>
  filter(year>1999) |>
  mutate(label = case_when(year==2023~paste0('<i>', month, ' ', day, '. ', location, '</i><br><b>', fatalities, ' deaths</b>'),
                           TRUE ~  paste0('<i>', month, ' ', day, '. ', location, ', </i><b>', fatalities, ' deaths</b>')),
         ylab = case_when(year==2023~cumsum(fatalities),
                          TRUE~-150),
         angle = case_when(year==2023~0,
                           TRUE~90)) |>
  group_by(year) |>
  mutate(label = paste(label, collapse='<br>'),
         fatalities = sum(fatalities)) |>
  ungroup() |>
  distinct(year, fatalities, label, angle, ylab) |>
  arrange(year) |>
  mutate(fac = cumsum(fatalities))



# plot
ggplot(crashes) +
  geom_line(aes(x=year, y=fac),
            color='red3',
            linewidth=.9) +
  geom_area(aes(x=year, y=fac),
            fill='#8b0000',
            alpha=.5) +
  geom_area(aes(x=year, y=-150),
            fill='#8b0000',
            alpha=.5) +
  geom_segment(aes(x=year, xend=year, y=-150, yend=fac),
               color='#edf2f4',
               alpha=.8,
               linetype='dashed',
               linewidth=.45) +
  geom_point(aes(x=year, y=fac),
             color='red3',
             size=2.5,
             stroke=1.1,
             shape = 21, 
             fill = "white") +
  geom_richtext(aes(x=2000, y=400, label=title),
                label.color=NA,
                fill=NA,
                hjust=0,
                vjust=0) +
  geom_richtext(aes(x=year, y=ylab, label=label, angle=angle),
                hjust=1,
                vjust=1,
                lineheight=.5,
                color='#111111',
                size=7,
                label.color=NA,
                fill=NA) +
  scale_y_reverse(limits=c(400,-150)) +
  scale_x_continuous(sec.axis = dup_axis(breaks=seq(2000, 2023, 1))) +
  labs(caption=caption) +
  theme_minimal() +
  theme(plot.background = element_rect(fill="#edf2f4", color="#edf2f4"),
        plot.caption = element_markdown(size=17),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x.bottom = element_blank(),
        axis.text.x.top = element_text(angle=90, 
                                       size=25, 
                                       vjust=.5,
                                       hjust=.5,
                                       family='oranienbaum',
                                       color='#111111',
                                       margin=margin(b=-25, t=12)))



ggview(units="px", height=2800, width=3000)
ggsave("Nepal-Plane-Crashes.png", height=2800, width=3000, units="px")