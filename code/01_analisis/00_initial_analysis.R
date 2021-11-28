################################################################################
### Title: "Análisis Presidenciales y Pobreza por Comuna - Chile 2021"
### Author: Magdalena Bennett
################################################################################

rm(list = ls())
cat("\014")

options(scipen = 99)

library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(firasans)

servel <- read.csv("https://raw.githubusercontent.com/maibennett/presidenciales_pobreza/main/data/servel_county_clean.csv")

casen <- read.csv("https://raw.githubusercontent.com/maibennett/presidenciales_pobreza/main/data/casen_county_clean.csv")

casen <- casen %>% rename(comuna_nombre = comuna)

servel <- servel %>% mutate(comuna_nombre = str_to_title(comuna_nombre))
  
d <- left_join(servel, casen, by = "comuna_nombre")

d <- d %>% mutate(Boric = ifelse(candidato=="GABRIEL BORIC FONT", 1, 0),
                  Kast = ifelse(candidato=="JOSE ANTONIO KAST RIST", 1, 0))

# Gráficos sin ajustes - Boric

# Basic - multidimensional
d %>% filter(Boric==1) %>% 
  ggplot(data = ., aes(x = pobreza_multi_4d*100, y = p_votos*100)) +
  geom_point(pch = 21, color = "dark orange", fill = alpha("dark orange", 0.3),
             size = 3) + 
  geom_smooth(se = TRUE, color = "#E16462", fill = alpha("#E16462", 0.3), lwd = 1.1) + 
  theme_bw() +
  theme_ipsum_fsc() + #plain 
  xlab("Pobreza Multidimensional (%)") + ylab("Total Votos (%)")+
  labs(title = "Correlación Pobreza y Votos", 
       subtitle = "Garbiel Boric", 
       caption = "Source: @maibennett") + 
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(colour = "dark grey"))+
  theme(axis.title.x = element_text(size=12),#margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size=12),#margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size = 10),legend.position=c(0.9,0.9),
        legend.title = element_text(size = 10),
        legend.text = element_text(size=10),
        legend.background = element_rect(fill="white",colour ="dark grey"),
        title = element_text(size=14))

# Basic - no multidimensional;
d %>% filter(Boric==1) %>% 
  ggplot(data = ., aes(x = pobreza_d*100, y = p_votos*100)) +
  geom_point(pch = 21, color = "dark orange", fill = alpha("dark orange", 0.3),
             size = 3) + 
  geom_smooth(se = TRUE, color = "#E16462", fill = alpha("#E16462", 0.3), lwd = 1.1) + 
  theme_bw() +
  theme_ipsum_fsc() + #plain 
  xlab("Pobreza (%)") + ylab("Total Votos (%)")+
  labs(title = "Correlación Pobreza y Votos", 
       subtitle = "Garbiel Boric", 
       caption = "Source: @maibennett") + 
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(colour = "dark grey"))+
  theme(axis.title.x = element_text(size=12),#margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size=12),#margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size = 10),legend.position=c(0.9,0.9),
        legend.title = element_text(size = 10),
        legend.text = element_text(size=10),
        legend.background = element_rect(fill="white",colour ="dark grey"),
        title = element_text(size=14))

# Weighted
d %>% filter(Boric==1) %>% 
  ggplot(data = ., aes(x = pobreza_multi_4d*100, y = p_votos*100)) +
  geom_point(aes(size = total/1000), pch = 21, color = "dark orange", fill = alpha("dark orange", 0.3)) + 
  geom_smooth(aes(weight = total), se = TRUE, color = "#E16462", fill = alpha("#E16462", 0.3), lwd = 1.1) +
  scale_size_continuous(range = c(1, 10), name = "Población (miles)") +
  theme_bw() +
  theme_ipsum_fsc() + #plain 
  xlab("Pobreza Multidimensional (%)") + ylab("Total Votos (%)")+
  labs(title = "Correlación Pobreza y Votos", 
       subtitle = "Garbiel Boric", 
       caption = "Source: @maibennett") + 
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(colour = "dark grey"))+
  theme(axis.title.x = element_text(size=12),#margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size=12),#margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size = 10),legend.position=c(0.9,0.9),
        legend.title = element_text(size = 12),
        legend.text = element_text(size=10),
        legend.background = element_rect(fill="white",colour ="dark grey"),
        title = element_text(size=14))

# Weighted - no multidimensioanl
d %>% filter(Boric==1) %>% 
  ggplot(data = ., aes(x = pobreza_d*100, y = p_votos*100)) +
  geom_point(aes(size = total/1000), pch = 21, color = "dark orange", fill = alpha("dark orange", 0.3)) + 
  geom_smooth(se = TRUE, color = "#E16462", fill = alpha("#E16462", 0.3), lwd = 1.1) +
  scale_size_continuous(range = c(1, 10), name = "Población (miles)") +
  theme_bw() +
  theme_ipsum_fsc() + #plain 
  xlab("Pobreza (%)") + ylab("Total Votos (%)")+
  ggtitle("Correlación Pobreza y Votos\nG. Boric") +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(colour = "dark grey"))+
  theme(axis.title.x = element_text(size=12),#margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size=12),#margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size = 10),legend.position=c(0.9,0.9),
        legend.title = element_text(size = 12),
        legend.text = element_text(size=10),
        legend.background = element_rect(fill="white",colour ="dark grey"),
        title = element_text(size=14))
  
# RM
d %>% filter(Boric==1 & region_nombre=="METROPOLITANA DE SANTIAGO") %>% 
  ggplot(data = ., aes(x = pobreza_multi_4d*100, y = p_votos*100)) +
  geom_point(pch = 21, color = "dark orange", fill = alpha("dark orange", 0.3),
             size = 3) + 
  geom_smooth(se = TRUE, color = "#E16462", fill = alpha("#E16462", 0.3), lwd = 1.1) + 
  theme_bw() +
  theme_ipsum_fsc() + #plain 
  xlab("Pobreza Multidimensional (%)") + ylab("Total Votos (%)")+
  labs(title = "Correlación Pobreza y Votos en la RM", 
       subtitle = "Garbiel Boric", 
       caption = "Source: @maibennett") + 
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(colour = "dark grey"))+
  theme(axis.title.x = element_text(size=12),#margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size=12),#margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size = 10),legend.position=c(0.9,0.9),
        legend.title = element_text(size = 10),
        legend.text = element_text(size=10),
        legend.background = element_rect(fill="white",colour ="dark grey"),
        title = element_text(size=14))


#No RM
d %>% filter(Boric==1 & region_nombre!="METROPOLITANA DE SANTIAGO") %>% 
  ggplot(data = ., aes(x = pobreza_multi_4d*100, y = p_votos*100)) +
  geom_point(aes(size = total/1000), pch = 21, color = "dark orange", fill = alpha("dark orange", 0.3)) + 
  geom_smooth(se = TRUE, color = "#E16462", fill = alpha("#E16462", 0.3), lwd = 1.1) +
  scale_size_continuous(range = c(1, 10), name = "Población (miles)") +
  theme_bw() +
  theme_ipsum_fsc() + #plain 
  xlab("Pobreza Multidimensional (%)") + ylab("Total Votos (%)")+
  ggtitle("Correlación Pobreza y Votos\nG. Boric") +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(colour = "dark grey"))+
  theme(axis.title.x = element_text(size=12),#margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size=12),#margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size = 10),legend.position=c(0.9,0.9),
        legend.title = element_text(size = 12),
        legend.text = element_text(size=10),
        legend.background = element_rect(fill="white",colour ="dark grey"),
        title = element_text(size=14))


# Linear models
summary(lm(p_votos ~ pobreza_multi_4d, data = d %>% filter(Boric==1), weights = d$total[d$Boric==1]))

summary(lm(p_votos ~ pobreza_multi_4d  + factor(region_nombre), data = d %>% filter(Boric==1)))

summary(lm(p_votos ~ pobreza_d, data = d %>% filter(Boric==1)))

summary(lm(p_votos ~ pobreza_d  + factor(region_nombre), data = d %>% filter(Boric==1)))


# Kast

# Basic - multidimensional
d %>% filter(Kast==1) %>% 
  ggplot(data = ., aes(x = pobreza_multi_4d*100, y = p_votos*100)) +
  geom_point(pch = 21, color = "dark orange", fill = alpha("dark orange", 0.3),
             size = 3) + 
  geom_smooth(se = TRUE, color = "#E16462", fill = alpha("#E16462", 0.3), lwd = 1.1) + 
  theme_bw() +
  theme_ipsum_fsc() + #plain 
  xlab("Pobreza Multidimensional (%)") + ylab("Total Votos (%)")+
  labs(title = "Correlación Pobreza y Votos", 
       subtitle = "José Antonio Kast", 
       caption = "Source: @maibennett") + 
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(colour = "dark grey"))+
  theme(axis.title.x = element_text(size=12),#margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size=12),#margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size = 10),legend.position=c(0.9,0.9),
        legend.title = element_text(size = 10),
        legend.text = element_text(size=10),
        legend.background = element_rect(fill="white",colour ="dark grey"),
        title = element_text(size=14))


# Weighted
d %>% filter(Kast==1) %>% 
  ggplot(data = ., aes(x = pobreza_multi_4d*100, y = p_votos*100)) +
  geom_point(aes(size = total/1000), pch = 21, color = "dark orange", fill = alpha("dark orange", 0.3)) + 
  geom_smooth(aes(weight = total), se = TRUE, color = "#E16462", fill = alpha("#E16462", 0.3), lwd = 1.1) +
  scale_size_continuous(range = c(1, 10), name = "Población (miles)") +
  theme_bw() +
  theme_ipsum_fsc() + #plain 
  xlab("Pobreza Multidimensional (%)") + ylab("Total Votos (%)")+
  labs(title = "Correlación Pobreza y Votos", 
       subtitle = "José Antonio Kast", 
       caption = "Source: @maibennett") + 
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(colour = "dark grey"))+
  theme(axis.title.x = element_text(size=12),#margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size=12),#margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size = 10),legend.position=c(0.9,0.9),
        legend.title = element_text(size = 12),
        legend.text = element_text(size=10),
        legend.background = element_rect(fill="white",colour ="dark grey"),
        title = element_text(size=14))



# RM
d %>% filter(Kast==1 & region_nombre=="METROPOLITANA DE SANTIAGO") %>% 
  ggplot(data = ., aes(x = pobreza_multi_4d*100, y = p_votos*100)) +
  geom_point(pch = 21, color = "dark orange", fill = alpha("dark orange", 0.3),
             size = 3) + 
  geom_smooth(se = TRUE, color = "#E16462", fill = alpha("#E16462", 0.3), lwd = 1.1) + 
  theme_bw() +
  theme_ipsum_fsc() + #plain 
  xlab("Pobreza Multidimensional (%)") + ylab("Total Votos (%)")+
  labs(title = "Correlación Pobreza y Votos en la RM", 
       subtitle = "José Antonio Kast", 
       caption = "Source: @maibennett") + 
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(colour = "dark grey"))+
  theme(axis.title.x = element_text(size=12),#margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size=12),#margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size = 10),legend.position=c(0.9,0.9),
        legend.title = element_text(size = 10),
        legend.text = element_text(size=10),
        legend.background = element_rect(fill="white",colour ="dark grey"),
        title = element_text(size=14))