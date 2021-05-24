# Pit Stops Formula 1


# Dados -----


dados_pit <- dados_pit %>% 
  select(raceId,
         driverId,
         circuitId,
         date,
         name.x,
         name.y,
         code,
         surname,
         time.x,
         milliseconds.x,
         duration,
         lap,
         stop) %>% 
  mutate(date = lubridate::ymd(date),
         year = lubridate::year(date))





# Checando os pit stops elevados. Boa parte foi devido a bandeira vermelha-----------

ferrari <- dados_pit %>% filter(year > 2009 & name.y == "Ferrari")

ferrari <- ferrari %>% filter(!str_detect(milliseconds.x, "\\d{7}"))
# esses tempos enormes de pit stop são as paradas sob bandeira vermelha. Não deveria ser considerado pit mas enfim

ferrari <- ferrari %>% filter(!str_detect(milliseconds.x, "\\d{6}"))

check <- ferrari %>% arrange(desc(milliseconds.x))





# Analise dados ------


# box plot f1

dados_pit %>% filter(year > 2009 & year < 2021 & name.y == c("Red Bull", "Ferrari")) %>% 
  filter(!str_detect(milliseconds.x, "\\d{7}")) %>% 
  filter(!str_detect(milliseconds.x, "\\d{6}")) %>% 
  ggplot(aes(factor(year), milliseconds.x))+
  geom_boxplot(aes(fill = name.y))+
  geom_jitter(aes(),alpha = 1/5)+
  theme_bw()+
  ylab("Milisegundos")+
  xlab("")+
  labs(title = "BOXPLOTS DO TEMPO NOS PIT STOPS NA F1", subtitle = 'Comparação do tempo de parada entre Ferrari e Red Bull')+
  theme(axis.text.x = element_text(size = 12, colour = '#004d4d'),
        axis.text.y = element_text(size = 11,colour = '#001a1a'),
        plot.title = element_text(hjust = 0.5, colour = '#004d4d', size = 17),
        plot.subtitle = element_text(hjust = 0.5, colour = '#004d4d'),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())+
  scale_fill_discrete(name = 'Equipes')


# Dados agrupados por equipe/ano com média, mediana e desvio padrao

pt_stop <- dados_pit %>% filter(year > 2009 & name.y == c("Red Bull", "Ferrari")) %>% 
  filter(!str_detect(milliseconds.x, "\\d{7}")) %>% 
  filter(!str_detect(milliseconds.x, "\\d{6}")) %>%  
  group_by(year, name.y) %>% 
  summarise(media = mean(milliseconds.x),
            mediana = median(milliseconds.x),
            desvio_pd = sd(milliseconds.x),
            min = min(milliseconds.x), # checar pq pega o 2 segundo valor minimo e não o primeiro
            max = max(milliseconds.x))



# grafico comparativo de média rbr e ferrari

pt_stop %>% filter(year < 2021) %>% 
  pivot_longer(cols = c("media"),
               names_to = "medidas",
               values_to = "valores") %>% 
  mutate(year = lubridate::ymd(year, truncated = 2L)) %>% 
  ggplot(aes(year, valores, group = interaction(medidas, name.y)))+
  geom_line(aes(),colour = 'grey', size = 1.5)+
  geom_point(aes(fill = name.y), size = 6, shape = 21, stroke = 0.5)+
  theme_bw()+
  ylab('TEMPO EM MILISEGUNDOS')+
  xlab('')+
  labs(title = 'Média de tempo nos Pit Stops', subtitle = 'Comparação entre Ferrari e Red Bull na última década')+
  scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
  theme(axis.text.x = element_text(size = 12, colour = '#004d4d'),
        axis.text.y = element_text(size = 11,colour = '#001a1a'),
        plot.title = element_text(hjust = 0.5, colour = '#004d4d', size = 20, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, colour = '#004d4d', face = 'bold', size = 13),
        #panel.grid.major = element_blank(),
        #panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())+
  scale_fill_discrete(name = 'Equipes')