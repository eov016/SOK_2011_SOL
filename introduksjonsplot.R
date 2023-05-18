
# Til introduksjonen har jeg to plot som er ment til å vise sammenhengen mellom 
# levealder og BNP, samt lykke og BNP. 
# for levealder Vs BNP brukte jeg pakken gapminder, for inspirasjon til
# dette har jeg sett litt på koding gjort tidligere i sok-2009 da jeg gjorde 
# tilsvarende plot.
gapminder <- gapminder

gap2007<- gapminder %>% 
  filter(year == 2007)
gap2007 <- janitor::clean_names(gap2007)


gap2007 <- gap2007 %>% 
  dplyr::mutate(gdp_percap=as.double(gdp_percap),
                life_exp=as.double(life_exp),
                pop=as.double(pop),
                continent=as.character(continent),
                country=as.character(continent))

Tabell_gap2007 <- gap2007 %>%
  dplyr::group_by(continent) %>%
  summarise(BNP_pr_innb_Snitt = mean(gdp_percap),
            Levealder_Snitt = mean(life_exp),
            Pop_pr_kontinent_Snitt = mean(pop))


intro_plot1<-ggplot(gap2007, aes(log10(gdp_percap), (life_exp), color=continent))+
  geom_point(aes(size=pop), alpha=0.8)+
  scale_color_manual(values = c("dodgerblue4","mediumpurple4",
                                "darkorchid","darkturquoise",
                                "darkcyan")) +
  labs(title = "Gapminder",
       subtitle = "Forventet levealder basert på BNP per innbygger",
       y="Levealder",
       x="BNP per Kapita",
       color="Kontinent",
       caption="Kilder: https://www.gapminder.org/data/") +
  scale_size_area(guide = "none", max_size = 14) +
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5, family = "LM Roman 10", color = "black", size=10),
        plot.subtitle = element_text(hjust = 0.5, family = "LM Roman 10", color = "lightsteelblue4", size=8),
        plot.caption = element_text(hjust = 0.5, family = "LM Roman 10", color = "lightsteelblue4", size=8),
        axis.ticks = element_blank(),
        axis.title.y = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.title.x = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.text.x = element_text(angle = 0, size = 5, face = "italic",color = "lightsteelblue4"),
        axis.text.y = element_text(size = 6, face = "italic",color = "lightsteelblue4")) +
  theme(legend.text = element_text(size = 4, face = "italic",color = "lightsteelblue4"),
        legend.title = element_text(size=6, hjust = 0.3, family = "LM Roman 10", color = "gray25"))

# etter å ha hentet datasettet for "lykke" og lastet det ned (se kilder)
# var plottet ganske rett fram.
Lykkelig2019 <- read.csv("2019.csv", header = TRUE)

#view(Lykkelig2019)


intro_plot2<-ggplot(Lykkelig2019, aes(Score, GDP.per.capita, color=Score))+
  geom_point(aes(size=Score), alpha=0.8)+
  scale_color_gradient(low = "darkorchid", high = "darkturquoise") +
  labs(title = "WHR",
       subtitle = "Forventet lykke til et land basert på BNP per innbygger",
       x="Lykke",
       y="BNP per innbygger",
       color="Lykke",
       caption="Kilde: https://www.kaggle.com/unsdsn/world-happiness")+
  scale_size_area(guide = "none", max_size = 6)+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5, family = "LM Roman 10", color = "black", size=10),
        plot.subtitle = element_text(hjust = 0.5, family = "LM Roman 10", color = "lightsteelblue4", size=8),
        plot.caption = element_text(hjust = 0.5, family = "LM Roman 10", color = "lightsteelblue4", size=8),
        axis.ticks = element_blank(),
        axis.title.y = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.title.x = element_text(hjust = 0.5, family = "LM Roman 10", color = "gray25"),
        axis.text.x = element_text(angle = 0, size = 5, face = "italic",color = "lightsteelblue4"),
        axis.text.y = element_text(size = 6, face = "italic",color = "lightsteelblue4")) +
  theme(legend.text = element_text(size = 4, face = "italic",color = "lightsteelblue4"),
        legend.title = element_text(size=6, hjust = 0.3, family = "LM Roman 10", color = "gray25"))

# brukte ggarange fra pakken ggpubr til å plassere plottene side om side.
ggarrange(intro_plot1, intro_plot2, ncol = 2, common.legend = F, legend="bottom")
