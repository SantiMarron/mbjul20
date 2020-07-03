# Reporte SAI julio 2020 (ult. act 24 de junio 2020)
# Sankey diagrams de flujos de importacion de Mexico y EUA

gc()
library(tidyverse)
library(readxl)
library(xlsx)
library(networkD3)

##### Lectura de base de datos #####

TMEC <- read_excel("Inputs/IM_NorteAmerica_ene-feb2020.xlsx")

Aux_paises <- read_excel("Inputs/Aux-paises.xlsx")

##### Procesamiento de base para Sankey #####

TMEC_proc <- TMEC %>% 
  group_by(Year, Reporter, Partner) %>% 
  summarise(Val = sum(`Trade Value (US$)`)) %>% 
  filter(Partner != "World") %>% 
  arrange(Reporter, -Val) %>% 
  group_by(Reporter) %>% 
  mutate(Val_acum = cumsum(Val)) %>% 
  mutate(Umbral = Val_acum/sum(Val)) %>% 
  mutate(Partner_f = ifelse(Umbral >= 0.90, "Otros (10% restante)", Partner)) %>% 
  group_by(Reporter, Partner_f) %>% 
  summarise(Val = sum(Val)) 

Lista_paises <- TMEC_proc %>% ungroup() %>% distinct(Partner_f)

write.xlsx2(as.data.frame(Lista_paises), "Outputs_Excel/Lista_paises.xlsx", row.names = F, 
            sheetName = "Lista_paises", append = F)

TMEC_fin <- TMEC_proc %>% 
  left_join(Aux_paises) %>% 
  mutate(ind = case_when(Reporter == "United States of America" ~ 1,
                         Reporter == "Mexico" ~ 2,
                         Reporter == "Canada" ~ 3)) %>% 
  arrange(ind, -Val) %>% ungroup() %>% 
  mutate(Reporter_es = case_when(Reporter == "United States of America" ~ "EUA",
                                Reporter == "Mexico" ~ "México",
                                Reporter == "Canada" ~ "Canadá")) %>% 
  select(Partner_es, Reporter_es, Val)

##### Sankey #####

links <- TMEC_fin %>% 
  mutate(group = case_when(Reporter_es == 'México' ~ 'type_a',
                           Reporter_es == 'Canadá' ~ 'type_b',
                           Reporter_es == 'EUA' ~ 'type_c')) %>% 
  mutate(group = as.factor(group))

# Enlistar nodos

nodes <- data.frame(
  name=c(as.character(links$Partner_es), as.character(links$Reporter_es)) %>% 
    unique()
)

# Add a 'group' column to each node. Here I decide to put all of them in the same group to make them grey

nodes <- nodes %>% 
  mutate(
    group = 
      case_when(name %in% c('China', 'India', 'Suiza', 'Singapur', 'Bélgica','Suecia') ~ 'gp1',
                name %in% c('México', 'Alemania', 'Reino Unido', 'Malasia', 'Israel', 'Arabia Saudita') ~ 'gp2',
                name %in% c('Canadá', 'India', 'Suiza') ~ 'gp3',
                name %in% c('Japón', 'Corea del Sur', 'Perú') ~ 'gp4',
                name %in% c('EUA') ~ 'gp5',
                name %in% c('Vietnam','España','Italia','Francia','Indonesia') ~ 'gp6',
                TRUE ~ 'gp7')) %>% 
  mutate(group = as.factor(group))

# Give a color for each group:

my_color <- 'd3.scaleOrdinal() .domain(["type_c", "type_b", "type_a", 
            "gp1", "gp2", "gp3", "gp4", "gp5", "gp6", "gp7"]) 
            .range(["grey", "brown", "darkgreen", "maroon", "green", "red", "orange", 
            "navy", "purple", "silver"])'


# Formato especifico de origen y destino

links$IDsource <- match(links$Partner_es, nodes$name)-1 
links$IDtarget <- match(links$Reporter_es, nodes$name)-1

#Hacer la conexion

p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "Val", NodeID = "name", 
                   colourScale = my_color, LinkGroup = "group", NodeGroup="group",
                   sinksRight=FALSE, fontSize = 14, fontFamily = "Century Gothic", nodeWidth = 15)
p

