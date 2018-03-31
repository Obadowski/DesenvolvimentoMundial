##### Post 1 - Desenvolvimento Mundial
## Feito por Obadowski

# Carregando bibliotecas
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(dslabs)
library(wesanderson)

#Definindo algumas variáveis importantes
# Pobreza extrema, UNESCO, 1 dólar diário per capita
# Pobreza segundo Banco Mundial, 5.5 doláres diários per capita.
ext_poverty <- 1
poverty_WB <- 5.5

# Ajustando semente para geração randômica de cores
set.seed(3)
gen_colors = sample(rainbow(5))
print(gen_colors)

# Obtendo base de dados da Gapminder
data(gapminder)

# Gerando coluna PIB per capita diário
gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)

# Existem muitos mais países em 2010 do que em 1970. Isto devido a fusão ou separação
# de nações ocorridas no início da década de 1990 pela queda URSS
# Além disso, muitos países não tinha informações públicas sobre suas
#   características em 1970.
# Consequentemente, é necessário remover países que não aparecem nas duas listas
#   simultaneamente. para isso, pode-se usar a função intersect
past_year = 1970
present_year = 2010

country_list_1 <- gapminder %>%
  filter(year == past_year & !is.na(dollars_per_day)) %>% .$country
country_list_2 <- gapminder %>%
  filter(year == present_year & !is.na(dollars_per_day)) %>% .$country
country_list <- intersect(country_list_1, country_list_2)

# Definindo de forma geral o gráfico
# 1) Filtrando por anos e pela lista de países
# 2) Reordenando a variável continent pela maior mediana em doláres diários
# 3) inicializando o gráfico: ggplot()
# 4) Colocando o tema do Wall Street Journal (Por quê? Porque eu gosto.)
# 5) Removendo o nome do eixo x
# 
p <- gapminder %>%
  filter(year %in% c(past_year, present_year) &
           country %in% country_list) %>%
  mutate(continent = reorder(continent, dollars_per_day, FUN = median)) %>%
  ggplot() +
  theme_wsj(color = "white") +
  xlab("")

# Agora o gráfico mesmo
# 1) Inicializando o gráfico boxplot
# 2) Ajustando cores conforme o conjunto gerador aleatoriamente
# 3) Título e subtítulo do gráfico (ggtitle)
# 4) Ajuste os nomes no eixo x (traduzindo para português)
# 5) Reposicionando os nomes no eixo x
# 6) Mudando a escala do eixo y para logarítmica, renomeando para US$
# 7) Marcando a linha da pobreza extrema
# 
p1 <- p + geom_boxplot(aes(continent, dollars_per_day,  fill = factor(year))) +
  scale_fill_manual(values= gen_colors,
                    name = '') +
  ggtitle("Evolução da Renda", subtitle = "Distribuição por continentes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(size = 14)) +
  scale_x_discrete(breaks = c("Africa", "Americas", "Asia", "Europe", "Oceania"),
                   labels = c("África", "Américas","Ásia", "Europa", "Oceania")) +
  scale_y_continuous(trans = "log2", 
                     breaks = c(1, 8, 64),
                     labels = c("US$ 1", "US$ 8", "US$ 64")) +
  geom_hline(yintercept = ext_poverty, color = "darkred", size = 1, linetype = "solid") +
  geom_hline(yintercept = poverty_WB, color = "darkgreen", size = 1, linetype = "dashed")

# Apresentando o gráfico pronto
print(p1)

# E o planeta todo?

p2 <- p +
  geom_boxplot(aes("Mundo", dollars_per_day, fill = factor(year)), width = 0.3) +
  scale_fill_manual(values= gen_colors,
                    name = '') +
  ggtitle("Evolução da Renda", subtitle = "1970 e 2010") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(size = 14)) +
  scale_y_continuous(trans = "log2", 
                     breaks = c(1, 8, 64),
                     labels = c("US$ 1", "US$ 8", "US$ 64")) +
  geom_hline(yintercept = ext_poverty, color = "darkred", size = 1, linetype = "solid") +
  geom_hline(yintercept = poverty_WB, color = "darkgreen", size = 1, linetype = "dashed")
  
print(p2)

# Verificando alguns parâmetros
ex_poors2010 <- gapminder %>%
  filter(year == present_year & dollars_per_day <= ext_poverty)
ex_poors1970 <- gapminder %>%
  filter(year == past_year & dollars_per_day <= ext_poverty)

poor_WB2010 <- gapminder %>%
  filter(year == present_year & dollars_per_day <= poverty_WB)
poor_WB1970 <- gapminder %>%
  filter(year == past_year & dollars_per_day <= poverty_WB)

world_2010 <- gapminder %>%
  filter(year == present_year)
world_1970 <- gapminder %>%
  filter(year == past_year)

sum(ex_poors2010$population, na.rm = TRUE)/sum(world_2010$population, na.rm = TRUE)
sum(poor_WB2010$population, na.rm = TRUE)/sum(world_2010$population, na.rm = TRUE)

sum(ex_poors1970$population, na.rm = TRUE)/sum(world_1970$population, na.rm = TRUE)
sum(poor_WB1970$population, na.rm = TRUE)/sum(world_1970$population, na.rm = TRUE)


