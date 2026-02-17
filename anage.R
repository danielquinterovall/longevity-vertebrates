#######################################################################
###### Gráficas de longevidad máxima a partir de AnAge ################
#######################################################################

#---------------------------------------------------------
### Carggando librerías, directorio de trabajo y datos

##-- librerías
library(tidyverse)
library(showtext)

##--directorio de trabajo
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # working directory

##-- Cargando datos
datos0 <- read.delim("anage_data.txt", header = TRUE, sep = "\t")

#---------------------------------------------------------
### Preparando datos

##-- Creando columna con genero y epíteto específico 

datos1 <- datos0 %>% mutate(spp = paste(Genus, Species, sep = " ")) %>%
  relocate(spp, .after = Species) %>% rename(maxlong = Maximum.longevity..yrs.)

##-- Estableciendo columnas a mantener

col = c("Class", "Kingdom", "Phylum", "Class", "Genus", "spp", "Common.name",
        "maxlong", "Data.quality", "Sample.size", "Specimen.origin")

#### PARA TODAS LAS ESPECIES

##-- Datos para las 10 especies con mayor longevidad máxima
##-- La columna Common.name.new contiene el nombre común separado en dos renglones

datos2 <- datos1 %>% 
  #filter(Data.quality %in% c("high", "acceptable"), Kingdom == "Animalia") %>%
  select(all_of(col)) %>% arrange(desc(maxlong)) %>% head(10) %>% 
  mutate(Common.name = as.character(Common.name), # Asegurarse de que Common.name es de tipo carácter
         Common.name.new = sapply(strsplit(Common.name, " "), function(x) {
           if (length(x) == 1) {
             x
           } else if (length(x) == 2) {
             paste(x[1], x[2], sep = "\n")
           } else if (length(x) == 3) {
             paste(paste(x[1:2], collapse = " "), x[3], sep = "\n")
           } else {
             paste(paste(x[1:2], collapse = " "), paste(x[3:4], collapse = " "), sep = "\n")
           }
         }))

datos2 <- datos2prueba %>% mutate(Common.name.new = unlist(Common.name.new))

#### PARA ANIMALES VERTEBRADOS

levels(datos1$Class)

vert <- c("Mammalia", "Aves", "Reptilia", "Amphibia", "Actinopterygii", 
          "Chondrichthyes", "Chondrostei", "Teleostei", "Cephalaspidomorphi",
          "Cladistei", "Coelacanthi", "Dipnoi", "Holostei")

datos3 <- datos1 %>% mutate(is_vert = ifelse(Class %in% vert, "Vertebrata", "No vertebrata"), .after = Class) %>%
  filter(is_vert == "Vertebrata") %>%
  select(all_of(col)) %>% arrange(desc(maxlong)) %>% head(10) %>% 
  mutate(Common.name = as.character(Common.name), # Asegurarse de que Common.name es de tipo carácter
         Common.name.new = sapply(strsplit(Common.name, " "), function(x) {
           if (length(x) == 1) {
             x
           } else if (length(x) == 2) {
             paste(x[1], x[2], sep = "\n")
           } else if (length(x) == 3) {
             paste(paste(x[1:2], collapse = " "), x[3], sep = "\n")
           } else {
             paste(paste(x[1:2], collapse = " "), paste(x[3:4], collapse = " "), sep = "\n")
           }
         }))

#### Para muchas más especies (solo animales)

# Solo datos con calidad alta o aceptable
# Creamos columna de spp con el género abreviado (sppnew)
# Un acolumna que combine el nombre común con el género

datos4 <- datos1 %>% filter(Kingdom == "Animalia", Data.quality %in% c("acceptable", "high")) %>%
  select(all_of(col)) %>% arrange(desc(maxlong)) %>% head(20) %>%
  mutate(sppnew = paste(paste0(substr(spp, start = 1, stop = 1), "."), 
                        word(spp, 2),
                        sep = " "),
         .after = spp) %>%
  mutate(label = paste(Common.name, paste0("(", sppnew, ")"), sep = " "),
         .after = sppnew)





#---------------------------------------------------------
### Graficas con ggplot2

##-- Estableciendo fuente y temas

## Fuentes

font_add("Source Sans Pro", regular = "SourceSansPro-Regular.ttf",
         bold = "SourceSansPro-SemiBold.ttf")# Activar showtext
showtext_auto()
mifont = "Source Sans Pro" 
font <- mifont

## Tema

theme_anage <- function(){
  theme_minimal() %+replace%    
    theme(panel.background =  element_rect(fill = "transparent", color = NA),
          panel.grid.minor = element_line(linetype = "dotted"),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "grey", size = 1, linetype = "dotted"),
          axis.text.x = element_blank(),
          axis.text.y = element_text(face = "bold", vjust= 0.5, hjust = -1.2, size = 15, colour = "grey20"),
          text = element_text(size = 15),
          plot.title = element_text(family = font, face = "bold", size = 28, colour = "grey20"),
          plot.subtitle = element_text(family = font, vjust = -1, size = 23, colour = "grey25"),
          plot.caption = element_text(family = font),
          axis.title = element_text(family = font),               
          axis.text = element_text(family = font),
          legend.text = element_text(family = font),
          legend.title = element_text(family = font, face = "bold", size = 18, colour = "grey25"),
          legend.position = "bottom")
}


#### PARA TODAS LAS ESPECIES

##-- Estableciendo colores por quality

color_qual = c("questionable" = "#FA7FAE",
               "acceptable" = "#A976E1")

##-- Grafica para todas las especies

allspp <- datos2 %>% ggplot(aes(x = reorder(Common.name.new, -maxlong), y = maxlong, fill = Data.quality)) + 
  geom_bar(stat="identity") +
  coord_cartesian(ylim = c(0,17000)) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(breaks = c(5000, 10000, 15000),
                     labels = c("5,000 years", "10,000 years", "15,000 years"),
                     expand = c(0,0)) +
 scale_fill_manual(values = color_qual, labels = c("Acceptable", "Questionable")) +
  labs(fill = "Data quality",
       y = element_blank(),
       x = element_blank(),
       title = "Species with the Highest Maximum Longevity",
       subtitle = "Based on Recorded Individual Lifespans",
       caption = "Source: AnAge: The Animal Ageing and Longevity Database (2024)") +
  geom_text(aes(label = spp),
            size = 4, vjust = -0.8, hjust = 0.5, colour = "grey30", fontface = "italic")+
  geom_text(aes(label = Common.name.new), 
            size = 5.3, vjust = -0.9, hjust = 0.5, colour = "grey30", fontface = "bold",  lineheight = 0.7) +
  theme_anage() +
  theme(axis.text.y = element_text(hjust = -0.5))

#### PARA ANIMALES VERTEBRADOS

##-- Estableciendo colores por origen

color_origin = c("wild" = "#66A182",
               "captivity" = "#2E4057")

##-- Grafica para todas las especies

vertebrates <- datos3 %>% ggplot(aes(x = reorder(Common.name.new, -maxlong), y = maxlong, fill = Specimen.origin)) + 
  geom_bar(stat="identity") +
  coord_cartesian(ylim = c(0,430)) +
  scale_x_discrete(expand = c(0,0.6)) +
  scale_y_continuous(breaks = seq(100, 400, 100),
                     labels = c("100 years", "200 years", "300 years", "400 years"),
                     expand = c(0,0)) +
  scale_fill_manual(values = color_origin, labels = c("Captivity", "Wild")) +
  labs(fill = "Specimen origin",
       y = element_blank(),
       x = element_blank(),
       title = "Vertebrate Animals with the Highest Maximum Longevity",
       subtitle = "Based on Recorded Individual Lifespans",
       caption = "Source: AnAge: The Animal Ageing and Longevity Database (2024)") +
  geom_text(aes(label = spp),
            size = 4, vjust = -0.8, hjust = 0.5, colour = "grey30", fontface = "italic")+
  geom_text(aes(label = Common.name.new), 
            size = 5.3, vjust = -0.9, hjust = 0.5, colour = "grey30", fontface = "bold",  lineheight = 0.7) +
  theme_anage()


#####--- Guardando

# Todas las spp
svg("maximumlongevity.svg", width = 20, height = 10.4, bg = "transparent")
print(allspp)
dev.off()

# vertebrados
svg("maximumlongevity_vertebrates.svg", width = 20, height = 10.4, bg = "transparent")
print(vertebrates)
dev.off()
