# packages
pacman::p_load(tidyverse,
               waffle,
               scales,
               ggtext,
               ggview)



# dados
df = tidytuesdayR::tt_load(2023, week = 9)
afrisenti = df$afrisenti
languages = df$languages



# fontes e textos
sysfonts::font_add('fb', 'fontes/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add_google("Spectral","spectral")
sysfonts::font_add_google("Open Sans","opensans")
showtext::showtext_auto()
showtext::showtext_opts(dpi=150)

title = "<span style='font-family:spectral;font-size:45pt;color:#00A6FB;'>**POSITIVIDADE NO TWITTER AFRICANO**</span>"

caption <- paste0(
  "<span style='font-family:fb;font-size:16pt;color:#00A6FB;'>&#xf099;</span>",
  "<span style='font-family:opensans;color:#EDF2FB;font-size:18pt;'> @taferreiraua </span>",
  "<span style='font-family:fb;font-size:16pt;color:#00A6FB;'> &#xf09b;</span>",
  "<span style='font-family:opensans;color:#EDF2FB;font-size:18pt;'> taferreiraua<br></span>")

subtitle = paste0("<span style='font-family:spectral;font-size:30pt;color:#EDF2FB;'>Proporção de sentimentos </span>", 
                  "<span style='font-family:spectral;font-size:30pt;color:#00A6FB;'>**positivos**</span>",
                  "<span style='font-family:spectral;font-size:30pt;color:#EDF2FB;'> de acordo com a análise de mais<br>de 100mil tweets em linguas africanas. Dados do <i><b>African Language Sentiment (AfriSenti).<br><br></span>",
                  caption)



# manipulação de dados
data = afrisenti |>
  left_join(languages, by='language_iso_code') |>
  select(language, label) |>
  mutate(label = case_when(label!='positive'~'other', TRUE ~ label)) |>
  group_by(language, label) |>
  count(label) |>
  ungroup() |>
  group_by(language) |>
  mutate(total = sum(n),
         perc = (100 * n)/total,
         perc.round = round(perc),
         pos.rate = case_when(label=='positive'~paste(perc.round), TRUE~''),
         language = case_when(language=='Algerian Arabic/Darja'~'Árabe Argelino<br>(Darja)',
                              language=='Amharic'~'Amárico',
                              language=='Hausa'~'Hauçá',
                              language=='Kinyarwanda'~'Quiniaruanda',
                              language=='Moroccan Arabic/Darija'~'Árabe Marroquino<br>(Darija)',
                              language=='Mozambican Portuguese'~'Português<br>Moçambicano',
                              language=='Nigerian Pidgin'~'Pidgin Nigeriano',
                              language=='Swahili'~'Suaíli',
                              language=='Tigrinya'~'Tigrínia',
                              language=='Twi'~'Axante',
                              language=='Xitsonga'~'Tsonga',
                              language=='Yorùbá'~'Iorubá',
                              TRUE ~ language),
         strip.lang = factor(language, levels=language, 
                             labels=paste0("<span style='font-size:21pt;color:#EDF2FB;'><b>", toupper(language), "</b><br></span>",
                                           "<br>",
                                           "<span style='font-family:fb;font-size:16pt;color:#00A6FB;'>&#xf099; </span>",
                                           "<span style='font-size:20pt;color:#EDF2FB;'> ", comma(total), "<span style='font-size:17pt;'> tweets</span>")),
         percent.label = case_when(pos.rate!=''~paste0("<span style='font-size:85pt;color:#EDF2FB;'>", pos.rate, "</span><span style='font-size:40pt;color:#EDF2FB;'>%</span>")))



# plot
ggplot(data) +
  geom_waffle(aes(fill=label, values=perc.round), 
              flip=T, 
              color='#333333', 
              size=1.5) +
  coord_equal() +
  facet_wrap(~strip.lang, ncol=5) +
  geom_richtext(aes(x=10.4, y=1.8, label=percent.label), 
                family='spectral', 
                hjust=1, 
                label.color = NA,
                fill = NA) +
  scale_fill_manual(values=c('grey30', '#00A6FB')) +
  labs(title=title, subtitle=subtitle) +
  theme_minimal() +
  theme(
    plot.background = element_rect(color='#333333', fill='#333333'),
    plot.title = element_markdown(hjust=.5),
    plot.subtitle = element_markdown(hjust=.5),
    plot.margin = margin(l=-4, r=0, t=22, b=10),
    panel.grid = element_blank(),
    strip.text = element_markdown(family='spectral', vjust=0, hjust=0), 
    strip.clip = 'off',
    legend.position = 'none',
    axis.text = element_blank(),
    axis.title = element_blank()
  )



ggview(units='px', height=2800, width=3000)
ggsave(units='px', height=2800, width=3000, filename='AfriSenti.png')
