# packages
pacman::p_load(tidyverse,
               geomtextpath,
               ggtext,
               ggimage,
               ggview)




# dados
df = tidytuesdayR::tt_load('2021-11-23')




# texto
sysfonts::font_add('aw6', 'fontes/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('futura', 'fontes/Futura Medium.otf')
sysfonts::font_add_google("Open Sans","opensans")
showtext::showtext_opts(dpi=150)
showtext::showtext_auto()

title=paste0("<span style='font-family:opensans;color:white;font-size:43pt;'><i><br>", 
             '"', "We're all stories in the end... make it a **good one**, eh?", '"')

subtitle="<span style='font-family:opensans;color:white;font-size:29pt;'>**Doctor Who** episode and doctor ratings."

caption = paste0(
  "<span style='font-family:aw6;'>&#xf099;</span>",
  "<span style='font-family:opensans;'> @taferreiraua </span>",
  "<span style='font-family:aw6;'>&#xf09b;</span>",
  "<span style='font-family:opensans;'> taferreiraua </span>",
  "<span style='font-family:opensans;'>| **Source:** _datardis_")




# cores
pal = c('#F37735',
        '#00B159',
        '#C77CFF',
        '#00AEDB',
        '#FFC425')
        



# manipulação de dados
doctorwho = df$writers |>
  left_join(df$episodes, by='story_number') |>
  left_join(df$directors, by='story_number') |>
  left_join(df$imdb, by=c('episode_number'='ep_num', 'season_number'='season')) |>
  filter(!is.na(story_number), !is.na(rating.y)) |>
  distinct(season_number, episode_number, episode_title, writer, director, rating.y) |>
  mutate(id = seq(1, length(writer)),
         doctor_actor = case_when(season_number==1~'Christopher Eccleston',
                                  season_number%in%c(2:4)~'David Tennant',
                                  season_number%in%c(5:7)~'Matt Smith',
                                  season_number%in%c(8:10)~'Peter Capaldi',
                                  season_number%in%c(11:12)~'Jodie Whittaker'),
         doctor_number = case_when(season_number==1~'9th Doctor',
                                   season_number%in%c(2:4)~'10th Doctor',
                                   season_number%in%c(5:7)~'11th Doctor',
                                   season_number%in%c(8:10)~'12th Doctor',
                                   season_number%in%c(11:12)~'13th Doctor'),
         doctor_label = paste0("<span style='font-family:opensans;color:white;font-size:20pt;'><b>", doctor_actor, "<br></span>",
                               "<span style='font-family:opensans;color:white;font-size:17pt;'>", doctor_number, "</span>"),
         images = paste0("https://raw.githubusercontent.com/taferreiraua/30DayChartChallenge/main/Day5/",
                         str_split_i(doctor_number, ' ', 1), ".png"),
         image_posx = case_when(doctor_number=='9th Doctor'~25,
                               doctor_number=='10th Doctor'~50,
                               doctor_number=='11th Doctor'~75,
                               doctor_number=='12th Doctor'~100,
                               doctor_number=='13th Doctor'~125)) |>
  group_by(season_number) |>
  mutate(seg_season_x = case_when(episode_number==min(episode_number)~id)) |>
  ungroup() |>
  mutate(points = case_when(rating.y==max(rating.y)~id,
                            rating.y==min(rating.y)~id),
         point_lab = case_when(rating.y==max(rating.y)~paste0("<span style='font-family:futura;font-size:20pt;color:white;'>",
                                                              '"<b>', episode_title, '"<br></span>',
                                                              "<span style='font-family:futura;font-size:15pt;color:white;'>",
                                                              'written by ', writer, '<br>', 'directed by ', director),
                               rating.y==min(rating.y)~paste0("<span style='font-family:futura;font-size:20pt;color:white;'>",
                                                              '"<b>', episode_title, '"<br></span>',
                                                              "<span style='font-family:futura;font-size:15pt;color:white;'>",
                                                              'written by ', writer, '<br>', 'directed by ', director))) |>
  group_by(doctor_number) |>
  mutate(avg = round(mean(rating.y),2))



# Inspirado no plot da Tanya Shapiro para a Tidy Tuesday:
# https://twitter.com/tanya_shapiro/status/1463237495135641606

# plot
ggplot(doctorwho) +
  geom_textsegment(aes(x=seg_season_x, xend=seg_season_x, y=1, yend=10.5,
                       label=paste0(season_number, ' season')),
                   color='white',
                   linewidth=.4,
                   alpha=.3,
                   hjust=0,
                   size=5) +
  geom_line(aes(x=id, y=rating.y, group=1, color=reorder(doctor_label, id))) +
  geom_point(aes(x=points, y=rating.y, color=reorder(doctor_label, id)),
             size=3) +
  geom_image(aes(x=image_posx, y=12.6, image=images), 
             size=.07,
             asp=1.4) +
  geom_richtext(aes(x=image_posx, y=11.9, label=doctor_label),
                hjust=.5,
                vjust=1,
                label.color=NA,
                fill=NA) +
  geom_richtext(aes(x=image_posx, y=11.2, label=avg, color=reorder(doctor_label, id)),
                family='opensans',
                fontface='bold',
                size=6,
                hjust=.5,
                vjust=1,
                label.color=NA,
                fill=NA) +
  geom_richtext(aes(x=points+3, y=rating.y, label=rating.y, color=reorder(doctor_label, id)),
                size=5.5,
                fontface='bold',
                hjust=.5,
                lineheight=.6,
                label.color=NA,
                fill=NA) +
  geom_richtext(aes(x=points-11, y=rating.y, label=point_lab),
                hjust=.5,
                lineheight=.6,
                label.color=NA,
                fill=NA) +
  geom_curve(aes(x=82, xend=76, y=10.5, yend=10.8),
             color='white',
             curvature=-0.2, 
             linewidth=0.3,
             arrow=arrow(length=unit(0.05,"in"))) +
  geom_text(aes(x=83, y=10.5, label='Avg rating per Doctor'),
            size=5.5,
            color='white',
            family='opensans',
            hjust=0) +
  scale_y_continuous(limits=c(1, 13), breaks=seq(3.5, 9.5, 1), sec.axis = dup_axis()) +
  scale_color_manual(values=pal) +
  labs(title=title, subtitle=subtitle, caption=caption) +
  theme_minimal() +
  theme(plot.background = element_rect(fill='#222222', color='#222222'),
        plot.title = element_markdown(hjust=.5),
        plot.subtitle = element_markdown(hjust=.5),
        plot.caption = element_markdown(size=17, color='white', hjust=.98),
        plot.margin = margin(l=10, r=10, b=10),
        panel.grid = element_blank(),
        legend.position = 'none',
        axis.text.x=element_blank(),
        axis.text.y.left = element_text(color='grey75', 
                                        size=14, 
                                        margin=margin(r=-20)),
        axis.text.y.right = element_text(color='grey75', 
                                        size=14, 
                                        margin=margin(l=-20)),
        axis.title = element_blank()) 
  



ggview(units='px', height=2500, width=3000)
ggsave(filename = 'doctorwho.png', units='px', height=2500, width=3000)
