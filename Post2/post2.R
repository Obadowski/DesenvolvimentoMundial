##### Post 2 - Desenvolvimento Mundial - Taxa de Natalidade
## Feito por Obadowski

# Carregando bibliotecas
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(dslabs)
library(wesanderson)

# Obtendo base de dados da Gapminder
data(gapminder)

# Gerando coluna PIB per capita diário
gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)

# Produzindo data.frame com dados de mortalidade infantil e PIB presentes
# Países sem dados não serão analisados.
gap_no_na <- gapminder %>%
  filter(!is.na(infant_mortality) & !is.na(gdp))

# Os dados mais recentes da maioria dos países é de 2011. Logo, analisaremos este ano.

# Gerando padrão de cores para o gráfico
set.seed(4)
gen_colors = sample(rainbow(5))

# Por etapas
# 1) Filtrar o ano: 2011
# 
# 2) Definir o mapeamento estético, eixo x, eixo y, padrão de cores,
#   x: coluna fertilidade
#   y: Expectativa de vida
#   padrão de cores: por continente
#   
# 3) Gráfico de pontos (scatter plot)
# 
# 4) geom_label_repel: tudo aquilo apenas para marcar Brasil, numa caixa usando uma linha para
# identificar o ponto
# 
# 5) Tema: Semelhante ao do Wall Street Journal + nome dos eixos
# 
# 6) Título e eixos (ggtitle, xlab e ylab)
# 
# 7) Organização das cores usando o padrão definido em gen_colors,
#   Organização e tradução dos nomes dos continentes
# 
p1 <- gap_no_na %>%
  filter(year == 2011) %>%
  ggplot(aes(x = fertility,
             y = life_expectancy,
             color = continent,
             size = population)) +
  geom_point(show.legend = FALSE) +
  geom_label_repel(aes(label = ifelse(country %in% c("Brazil"), "Brasil", '')),
                   box.padding = 1, point.padding = 0.5,
                   show.legend = FALSE) +
  theme_wsj() + theme(axis.title=element_text(size=12)) +
  ggtitle("Nascimentos x Expectativa de vida", subtitle = "2011") +
  xlab("Taxa de Natalidade") +
  ylab("Expectativa de vida") +
  scale_color_manual(values = gen_colors,
                     name = "",
                     breaks = c("Africa", "Americas", "Asia", "Europe", "Oceania"),
                     labels = c("África", "Américas", "Ásia", "Europa", "Oceania"))

# Mostra o gráfico
print(p1)
