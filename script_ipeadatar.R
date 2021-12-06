#install.packages("ipeadatar")
library(ipeadatar)
library(magrittr)
library(ggplot2)
library(dplyr)



# Séries disponíveis ------------------------------------------------------
series2 <- search_series(terms = NULL, 
                         fields = c('name'), 
                         language = c("en", "br"))

# fields podem ser "code", "name", "theme", "source", "freq", 
# "lastupdate" e "status".


# IPCA --------------------------------------------------------------------

# Suponha interece no IPCA mensal
pesquisa <- search_series(terms = "IPCA", # Termo de pesquisa
                         fields = c('name'), # Campo pelo qual queremos pesquisar
                         language = c("en", "br")) # Idioma

# Após identificar que o código da série desejada é PRECOS12_IPCA12
# podemos usar a função ipeadata para obter os dados
ipca <- ipeadata("PRECOS12_IPCA12") 


# Gráfico
ggplot(ipca, aes(x = date, y = value)) +
  geom_line(size = 1.2, color = "darkorange") +
  labs(x = "", y = "IPCA - Geral - Índice (dez. 1993 = 100)") +
  scale_x_date(
    date_breaks = "2 years", date_labels = "%m/%Y",
    limits = c(min(ipca$date) - 360, max(ipca$date) + 90),
    expand = c(0, 0)
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 50, hjust = 1),
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_line(colour = "grey40", size = 1.1),
    axis.ticks.length = unit(0.3, "cm"),
    text = element_text(size = 18, face = "bold")
  )



# Salário Mínimo Real -----------------------------------------------------


salmin <- ipeadata("GAC12_SALMINRE12")

ggplot(salmin, aes(x = date, y = value)) +
  geom_line(size = 1.06, color = "darkorange") +
  labs(x = "", y = "Salário Mínimo Real") +
  scale_x_date(
    date_breaks = "3 years",
    date_labels = "%m/%Y",
    limits = c(min(salmin$date) - 360, max(salmin$date) + 300),
    expand = c(0, 0)
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_line(colour = "grey40", size = 1.1),
    axis.ticks.length = unit(0.3, "cm"),
    text = element_text(size = 18, face = "bold")
  )



# Homicídios --------------------------------------------------------------
library(geobr)

states <- read_state(year = 2019)

homicidios <- ipeadata("HOMIC")

homicidios %>% 
  filter(uname == "States",
         date == max(date)) %>% 
  left_join(states, by = c("tcode" = "code_state")) -> homicidios

ggplot() +
  geom_sf(data = homicidios, aes(fill=value, geometry = geom), 
          color = NA, size = .15)+
  scale_fill_distiller(palette = "YlOrRd", 
                       direction = 1, 
                       name="Número de homicídios por estado em 2019") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.width = unit(3.5, "cm"),
        legend.text  = element_text(size=18),
        legend.title = element_text(size=18)) + 
  guides(fill = guide_colourbar(ticks.colour = "black",
                                frame.colour = "black",
                                title.position="top",
                                title.hjust = 0.5,
                                frame.linewidth = 2))




# Outros Exemplos ---------------------------------------------------------

# INPC
series_inpc <- search_series(terms = "INPC", fields = c('name'), language = c("en", "br"))


inpc <- ipeadata("PRECOS12_INPC12")


ggplot(inpc, aes(x = date, y = value)) +
  geom_line(size = 1.2, color = "mediumseagreen") +
  labs(x = "", y = "INPC - geral - índice (dez. 1993 = 100)") +
  scale_x_date(
    date_breaks = "12 months", date_labels = "%m/%Y",
    limits = c(min(ipca$date) - 360, max(ipca$date) + 300),
    expand = c(0, 0)
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 50, hjust = 1),
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_line(colour = "grey40", size = 1.1),
    axis.ticks.length = unit(0.3, "cm"),
    text = element_text(size = 12, face = "bold")
  )

# Desemprego
desemprego <- ipeadata("PAN12_TD12")

ggplot(desemprego, aes(x = date, y = value)) +
  geom_line(size = 1.2, color = "mediumseagreen") +
  labs(x = "", y = "Taxa de desemprego") +
  scale_x_date(date_breaks = "12 months", date_labels = "%m/%Y") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 50, hjust = 1),
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_line(colour = "grey40", size = 1.1),
    axis.ticks.length = unit(0.3, "cm"),
    text = element_text(size = 12, face = "bold")
  )
