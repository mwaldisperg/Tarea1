library(tidyverse)
library(rmarkdown)
library(tinytex)
library(pacman)
library(knitr)
library(lubridate)

parks <- read_csv("parks.csv")
View(parks)

species <- read_csv("species.csv")
View(species)

#cambiar el nombre de las variables que tienen espacios x ej: Petal Lenght
colnames(parks)<-make.names(colnames(parks))
colnames(species)<-make.names(colnames(species))

#ordenar las tablas de datos por Park.Names
parks1<-parks %>% group_by(Park.Name)
species1<-species %>% group_by(Park.Name)

#Juntar las dos tablas de datos
parks.species <- parks %>% full_join(species) 
View(parks.species)

#Filtrar por las especies presentes en la variable de ocurrencia y seleccionar las variables que me interesan 
parks.species<-parks.species %>% filter(Occurrence == "Present") %>% select(Park.Name, Park.Code, Acres,Latitude, Longitude, Common.Names,Scientific.Name,Abundance, Category,Nativeness,  Conservation.Status) 
View(parks.species)

unique(parks.species$Category)

Animal<-parks.species %>% filter(!is.na(Abundance)) %>% filter(Category == "Mammal"| Category == "Bird"| Category == "Reptile"| Category == "Amphibian"| Category == "Fish"| Category == "Invertebrate"|
                                   Category == "Insect"| Category == "Spider/Scorpion"| Category == "Crab/Lobster/Shrimp"| Category == "Slug/Snail") %>% filter(Nativeness == "Native" | Nativeness == "Not Native") %>% mutate(Grupo= c("Animal"))
View(Animal)

unique(Animal$Nativeness)

Plant<-parks.species %>% filter(!is.na(Abundance)) %>% filter(Category == "Vascular Plant"| Category == "Fungi"| Category == "Algae"| Category == "Nonvascular Plant") %>% filter(Nativeness == "Native" | Nativeness == "Not Native") %>% mutate(Grupo= c("Planta"))
View(Plant)

Full<- Animal %>% full_join(Plant) 
View(Full)
Fullnative<- Animal %>% full_join(Plant) %>% filter(Nativeness == "Native")
View(Fullnative)

Calida<-Full %>% arrange(Latitude) %>% filter(Latitude== 19.38 | Latitude== 20.72)%>% mutate(Zona= c("Cálida"))
Templada<-Full %>% arrange(Latitude) %>% filter(Latitude != 19.38 & Latitude != 20.72 & Latitude != 67.55 & Latitude != 67.78)%>% mutate(Zona= c("Templada"))
Fria<-Full %>% arrange(Latitude) %>% filter(Latitude== 67.55 | Latitude== 67.78)%>% mutate(Zona= c("Fría"))

Latitudful<- Calida %>% full_join(Fria)
Latitudfull<- Latitudful %>% full_join(Templada) %>% mutate(Zona =fct_relevel(factor(Zona), c("Cálida", "Templada")))

View(Latitudfull)

##
caso<-Latitudfull %>% select(Scientific.Name,Abundance,Zona) %>% group_by(Scientific.Name) %>% arrange(Scientific.Name)
View(caso)

#Riqueza de especies según su abundancia y zona a la que pertenecen
Abundancia<-Latitudfull %>%  group_by(Abundance,Zona,Park.Code,Grupo)  %>% summarise(n=n()) %>% rename(Riqueza=n)
View(Abundancia)
ggplot(Abundancia, aes(x=Abundance, y=Riqueza))+geom_col(aes(fill=Grupo,group=Zona, color=Zona), position = "dodge")+theme_classic() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#Riqueza de los parques segun tamaño por zona donde se encuentran
Latitudfull1<-Latitudfull %>%  group_by(Acres,Zona,Latitude)  %>% summarise(n=n()) %>% rename(Riqueza=n)%>% arrange(Latitude)
View(Latitudfull1)
ggplot(Latitudfull1, aes(x=Zona, y=Riqueza))+geom_point(aes(size=Acres))+theme_classic()+geom_smooth(method="lm")

#Riqueza de por categoría segun el tamaño del parque
Full1<-Full %>% group_by(Acres,Park.Code,Category) %>% summarise(n=n()) %>% rename(Riqueza=n) 
Full11<-Full %>% group_by(Acres,Park.Code) %>% summarise(n=n()) %>% rename(Riqueza=n) %>% arrange(Riqueza)
View(Full11)
ggplot(Full1, aes(x=Acres, y=Riqueza))+geom_point(aes(color=Category))+theme_classic()
ggplot(Full11, aes(x=reorder(Park.Code,Acres), y=Riqueza))+geom_point(aes(size=Acres))+theme_classic()+ theme(axis.text.x = element_text(angle = 90))

#Riqueza de animales y plantas según si son nativas o no
Full2<-Full %>% group_by(Acres,Park.Code,Grupo,Nativeness) %>% summarise(n=n()) %>% rename(Riqueza=n)
View(Full2)
ggplot(Full2, aes(x=Grupo, y=Riqueza))+geom_boxplot()+theme_classic()+geom_jitter(aes(color=Nativeness))

#Tabla intervalo lat/zona
Latitudfulll<-Latitudfull %>% arrange(Latitude)
dput(unique(Latitudfulll$Latitude))
Latitud<-c("[19.38-20.72]","[24.63-66.33]","[67.55-67.78]")
Zona<-c("Cálida", "Templada", "Fría")
Tabla1<-data.frame(Zona,Latitud)
View(Tabla1)

#Tabla sp/categoria
Tabla2<-Latitudfull %>% select(Category,Scientific.Name)
View(Tabla2)

#Tabla planta/categoria animal/categoria
Categ<-c("Plantas vasculares, Plantas no-vasculares, Fungi, Algas", "Mamíferos, Aves, Reptiles, Anfibios, Peces, Cangrejos/Langostas/Camarones, Babosas/Caracoles, Arañas/Escorpiones, Insectos, Invertebrados")
Group<- c("Planta", "Animal")
Tabla3<-data.frame(Group, Categ)
View(Tabla3)

#Tabla desv+prom/zona
Tabla4a<-data.frame(Latitudfull %>% group_by(Acres,Grupo,Park.Code) %>% summarise(n=n()) %>% rename(Riqueza=n))
Tabla4<- Tabla4a %>% select(Grupo,Riqueza) %>% group_by(Grupo) %>% summarise (DE=sd(Riqueza), Promedio=mean(Riqueza))
View(Tabla4)

#Tabla desv+prom/zona
DE<-c(494.27, 484.71, 119.50)
Promedio<-c(2372.50, 1238.08, 637.50)
Zona<- c("Cálida", "Templada", "Fría")
Tabla5<-data.frame(Zona, Promedio, DE)
View(Tabla5)

kable(Resumen, caption="Tabla 1. En esta tabla se observa la media y desviación estándar del monóxido de carbono en distintas estaciones de Madrid", digits=2)
