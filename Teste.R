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


df <- read_csv("mxmh_survey_results.csv", col_names=TRUE)

df <- df %>% 
  mutate(
    Ansiedade = case_when(
      Anxiety > 8 ~ "Muito Alto",
      6 < Anxiety & Anxiety <= 8 ~ "Alto",
      3 < Anxiety & Anxiety <= 6 ~ "Medio",
      Anxiety <=3 ~ "Baixo",
  )
)
df$Ansiedade = factor(df$Ansiedade, levels = c('Muito Alto', 'Alto','Medio', 'Baixo'))

df <- df %>% 
  mutate(
    Depressao = case_when(
      Depression > 8 ~ "Muito Alto",
      6 < Depression & Depression <= 8 ~ "Alto",
      3 < Depression & Depression <= 6 ~ "Medio",
      Depression <=3 ~ "Baixo",
  )
)

df$Depressao = factor(df$Depressao, levels = c('Muito Alto', 'Alto','Medio', 'Baixo'))

df <-df %>% 
  mutate(
    Insonia = case_when(
      Insomnia > 8 ~ "Muito Alto",
      6 < Insomnia & Insomnia <= 8 ~ "Alto",
      3 < Insomnia & Insomnia <= 6 ~ "Medio",
      Insomnia <=3 ~ "Baixo",
  )
)

df$Insonia = factor(df$Insonia, levels = c('Muito Alto', 'Alto','Medio', 'Baixo'))

df <-df %>% 
  mutate(
    TOC = case_when(
      OCD > 8 ~ "Muito Alto",
      6 < OCD & OCD <= 8 ~ "Alto",
      3 < OCD & OCD <= 7 ~ "Medio",
      OCD <=3 ~ "Baixo",
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

df2 <- df[,c("Ansiedade","Depressao","TOC","Insonia")]

df2 <- df2 %>%
  mutate(Ansiedade = case_when(
    Ansiedade == "Muito Alto" ~ 4,
    Ansiedade == "Alto" ~ 3,
    Ansiedade == "Medio" ~ 2,
    Ansiedade == "Baixo" ~ 1),
    Depressao = case_when(
      Depressao == "Muito Alto" ~ 4,
      Depressao == "Alto" ~ 3,
      Depressao == "Medio" ~ 2,
      Depressao == "Baixo" ~ 1),
    Insonia = case_when(
      Insonia == "Muito Alto" ~ 4,
      Insonia == "Alto" ~ 3,
      Insonia == "Medio" ~ 2,
      Insonia == "Baixo" ~ 1),
    TOC = case_when(
      TOC == "Muito Alto" ~ 4,
      TOC == "Alto" ~ 3,
      TOC == "Medio" ~ 2,
      TOC == "Baixo" ~ 1))

#cor_mat <- cor(df2)
#corrplot(cor_mat, method = "color")

df %>%
  filter(!(`Fav genre` %in% "R&B")) %>%
  ggplot() +
  aes(x = Age) +
  geom_histogram(bins = 25L, fill = "#0C4C8A") +
  theme_minimal() +
  facet_wrap(vars(Quantidade_de_Horas))

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

