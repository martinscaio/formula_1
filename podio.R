
# Dados Pódio


podio <- podio %>% 
  select(resultId,
         grid, 
         position, 
         positionText, 
         positionOrder, 
         driverRef,
         name.x, 
         name.y, 
         date, 
         year)


# Dados Ferrari --------


ferrari <- podio %>% 
  filter(name.x == "Ferrari" & year > 2009 & year < 2021)


ferrari_podium <- ferrari %>% filter(position == 1 | position == 2 | position == 3) %>% 
  group_by(year) %>% 
  count()



# Numero de corridas por temporada ------

numero_corridas <- podio %>% select(name.y, year) %>% 
  group_by(year) %>% distinct() %>% count() %>% 
  arrange(desc(year)) %>% 
  rename(CORRIDAS = n)



# TABELA PODIO FERRARI -----------

ferrari_podium %>% 
  left_join(numero_corridas, by = 'year') %>% 
  rename(ANO = year, PODIOS = n) %>% 
  arrange(ANO) %>% 
  kableExtra::kbl() %>% 
  kableExtra::kable_styling(bootstrap_options = c('striped', 'hover'))


# grafico pódios da ferrari


ferrari_podium %>% 
  left_join(numero_corridas, by = 'year') %>% 
  rename(ANO = year, PODIOS = n) %>% 
  mutate(ANO = lubridate::ymd(ANO, truncated = 2L)) %>% 
  ggplot(aes(ANO, PODIOS))+
  geom_col(aes(), fill = '#ff3333')+
  geom_text(aes(label = PODIOS, vjust = 1.5), size = 4.5,colour = 'white')+
  theme_bw()+
  xlab('')+
  ylab('')+
  labs(title = 'PÓDIOS DA FERRARI POR ANO', subtitle = 'Comparativo do ano a ano do número de pódios pela Ferrari')+
  scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
  theme(axis.text.x = element_text(size = 12, colour = '#004d4d'),
        axis.text.y = element_text(size = 11,colour = '#001a1a'),
        plot.title = element_text(hjust = 0.5, colour = '#004d4d', size = 17),
        plot.subtitle = element_text(hjust = 0.5, colour = '#004d4d'),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())



# Gráfico do Numero de vitorias----


podio %>% filter(position == 1 & name.x == 'Ferrari' & year > 2009 & year < 2021) %>% 
  group_by(year, .add = FALSE) %>% 
  count() %>% 
  rename(ANO = year,
         VITORIAS = n) %>% 
  ungroup() %>% 
  add_row(ANO = c(2014, 2016, 2020), VITORIAS = c(0,0,0)) %>% 
  arrange(ANO) %>% 
  mutate(ANO = lubridate::ymd(ANO, truncated = 2L)) %>% 
  ggplot(aes(ANO, VITORIAS))+
  geom_col(aes(), fill = '#ff4d4d')+
  geom_text(aes(label = VITORIAS, vjust = -0.35), size = 4.5,colour = '#333333', fontface = 'bold')+
  theme_bw()+
  xlab('')+
  ylab('')+
  labs(title = 'Vitórias da Ferrari na F1', subtitle = 'Comparativo ano a ano do número de vitórias pela Ferrari')+
  scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
  theme(axis.text.x = element_text(size = 12, colour = '#004d4d'),
        axis.text.y = element_text(size = 11,colour = '#001a1a'),
        plot.title = element_text(hjust = 0.5, colour = 'Black', size = 20, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, colour = '#004d4d', face = 'bold'),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())








