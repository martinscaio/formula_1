

# Numero de poles ferrari-----

pole <- quali %>% filter(name.x == 'Ferrari' & position == 1 & year > 2009 & year < 2021) %>% 
  group_by(year) %>% 
  count() %>% 
  rename(ANO = year, 
         POLES = n) %>% 
  ungroup() %>% 
  add_row(ANO = c(2011, 2013, 2014, 2016, 2020), POLES = c(0,0,0,0,0))


pole %>% mutate(ANO = lubridate::ymd(ANO, truncated = 2L)) %>%  
  ggplot(aes(ANO, POLES))+
  geom_col(aes(), fill = "#ff4d4d")+
  geom_text(aes(label = POLES, vjust = -0.35), size = 4.5,colour = '#001a1a', fontface = 'bold')+
  theme_bw()+
  xlab('')+
  ylab('')+
  labs(title = 'NÚMERO DE POLES POSITION', subtitle = 'Poles Position da Ferrari nos últimos 10 anos')+
  scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
  theme(axis.text.x = element_text(size = 12, colour = '#333333'),
        axis.text.y = element_text(size = 11,colour = '#333333'),
        plot.title = element_text(hjust = 0.5, colour = '#004d4d', size = 17),
        plot.subtitle = element_text(hjust = 0.5, colour = '#004d4d'),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())






pole %>% mutate(ANO = lubridate::ymd(ANO, truncated = 2L)) %>%  
  ggplot(aes(ANO, POLES))+
  geom_line(aes(), fill = "#ff4d4d")+
  geom_text(aes(label = POLES, vjust = -0.35), size = 4.5,colour = '#333333', fontface = 'bold')+
  theme_bw()+
  xlab('')+
  ylab('')+
  labs(title = 'NÚMERO DE POLES POSITION', subtitle = 'Poles Position da Ferrari nos últimos 10 anos')+
  scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
  theme(axis.text.x = element_text(size = 12, colour = '#004d4d'),
        axis.text.y = element_text(size = 11,colour = '#001a1a'),
        plot.title = element_text(hjust = 0.5, colour = '#004d4d', size = 17),
        plot.subtitle = element_text(hjust = 0.5, colour = '#004d4d'),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())


                  