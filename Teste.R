library(corrplot)
library(readxl)
library(car)
library(tidyverse)
library(reactable)
library(ggplot2)
library(plotly)
library(forcats)
library(dplyr)
library(janitor)
library(esquisse)
library(patchwork)
library(RColorBrewer)
library(gridExtra)
library(grid)

## TUDO É TESTE DE GRÁFICOS

setwd("C:/Users/iaram/OneDrive/Documentos/GitHub/R-Project")
df <- read_csv("mxmh_survey_results.csv", col_names=TRUE)

df <- df %>% 
  mutate(
    Ansiedade = case_when(
      Anxiety > 8 ~ "Muito Alto",
      6 < Anxiety & Anxiety <= 8 ~ "Alto",
      4 < Anxiety & Anxiety <= 6 ~ "Medio",
      Anxiety <=4 ~ "Baixo",
  )
)
df$Ansiedade = factor(df$Ansiedade, levels = c('Muito Alto', 'Alto','Medio', 'Baixo'))

df <- df %>% 
  mutate(
    Depressao = case_when(
      Depression > 8 ~ "Muito Alto",
      6 < Depression & Depression <= 8 ~ "Alto",
      4 < Depression & Depression <= 6 ~ "Medio",
      Depression <=4 ~ "Baixo",
  )
)

df$Depressao = factor(df$Depressao, levels = c('Muito Alto', 'Alto','Medio', 'Baixo'))

df <-df %>% 
  mutate(
    Insonia = case_when(
      Insomnia > 8 ~ "Muito Alto",
      6 < Insomnia & Insomnia <= 8 ~ "Alto",
      4 < Insomnia & Insomnia <= 6 ~ "Medio",
      Insomnia <=4 ~ "Baixo",
  )
)

df$Insonia = factor(df$Insonia, levels = c('Muito Alto', 'Alto','Medio', 'Baixo'))

df <-df %>% 
  mutate(
    TOC = case_when(
      OCD > 8 ~ "Muito Alto",
      6 < OCD & OCD <= 8 ~ "Alto",
      4 < OCD & OCD <= 7 ~ "Medio",
      OCD <=4 ~ "Baixo",
  )
)

df$TOC = factor(df$TOC, levels = c('Muito Alto', 'Alto','Medio', 'Baixo'))

df <- df %>% 
  mutate(
    Quantidade_de_Horas = case_when(
      `Hours per day` >= 6 ~ "6-24",
      4 <= `Hours per day` & `Hours per day` < 6 ~ "4-6",
      2 <= `Hours per day` & `Hours per day` < 4 ~ "2-4",
      `Hours per day` < 2 ~ "0-2",
  )
)

df$Quantidade_de_Horas = factor(df$Quantidade_de_Horas, levels = c('6-24', '4-6','2-4', '0-2'))

df2 <- df[,c("Anxiety","Depression","OCD", "Insomnia")]


#cor_mat <- cor(df2)
#corrplot(cor_mat, method = "color")

#### HORAS/DIA X IDADE
df %>%
  filter(!(`Fav genre` %in% "R&B")) %>%
  ggplot() +
  aes(x = Age) +
  geom_histogram(bins = 25L, fill = "#0C4C8A") +
  theme_minimal() +
  facet_wrap(vars(Quantidade_de_Horas))

###### CORR PLOT
cor_mat <- cor(df2)
corrplot(cor_mat, method = "color", tl.col = 'black')
###################
g1 <- ggplot(df %>% filter(!(`Fav genre` %in% "R&B")), aes(x = Anxiety, y = `Music effects`)) +
  geom_boxplot(fill = "#15284A") +
  coord_flip() +
  theme_gray()

g2 <- ggplot(df %>% filter(!(`Fav genre` %in% "R&B")), aes(x = Depression, y = `Music effects`)) +
  geom_boxplot(fill = "#15284A") +
  coord_flip() +
  theme_gray()

g3 <- ggplot(df %>% filter(!(`Fav genre` %in% "R&B")), aes(x = Insomnia, y = `Music effects`)) +
  geom_boxplot(fill = "#15284A") +
  coord_flip() +
  theme_gray()

g4 <- ggplot(df %>% filter(!(`Fav genre` %in% "R&B")), aes(x = OCD, y = `Music effects`)) +
  geom_boxplot(fill = "#15284A") +
  coord_flip() +
  theme_gray()

grid.arrange(arrangeGrob(g1, g2, ncol=2), arrangeGrob(g3, g4, ncol=2), nrow = 2)

###########################
chart1 <- df %>% 
  filter(!is.na(`Primary streaming service`)) %>%
  group_by(`Primary streaming service`) %>%
  summarise(Freq = n()) %>%
  ggplot(aes(x = "", y = Freq, fill = `Primary streaming service`)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(fill = "") +
  ylab("") +
  xlab("") +
  theme_void() +
  theme(
    panel.grid.major = element_blank(),
    plot.background = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  ) 

chart2 <- df %>% 
  filter(!is.na(`Primary streaming service`)) %>%
  group_by(`Fav genre`) %>%
  summarise(Freq = n()) %>%
  ggplot(aes(x = "", y = Freq, fill = `Fav genre`)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(fill = "") +
  ylab("") +
  xlab("") +
  theme_void() +
  theme(
    panel.grid.major = element_blank(),
    plot.background = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  ) 

chart1 + chart2 + plot_layout(ncol = 2)

##################
cor_mat <- cor(df2)
corrplot(cor_mat, method = "color", tl.col = 'black', tl.srt = 45)

# Porcentagem
df_fav_genre <- df %>%
  filter(!is.na(`Music effects`)) %>%
  group_by(`Fav genre`, `Music effects`) %>%
  summarise(Percentage = n()) %>%
  mutate(Percentage = Percentage / sum(Percentage) * 100)
  

# Plot
ggplot(df_fav_genre, aes(x = `Fav genre`, y = Percentage, fill = `Music effects`)) +
  geom_col(position = "identity", width = 0.8) +
  labs(title = NULL, x = "Favourite Genre", y = "Percentage") +
  scale_fill_discrete(name = "Music effects") +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(factor(df_fav_genre$`Fav genre`)))) +
  theme_minimal() 

############

esquisser()

