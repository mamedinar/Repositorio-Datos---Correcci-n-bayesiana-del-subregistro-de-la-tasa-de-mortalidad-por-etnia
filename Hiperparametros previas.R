rm(list=ls())   
setwd("/Users/mmedinar/Documents/")
graphics.off()
if (.Platform$OS.type == 'windows') windows(width=8, height=8, record=TRUE)

set.seed(6447100)

library(dplyr)
library(splines)
library(rstan)
library(tidyr)
library(readr)

#-------------- Defunciones reportadas ----------------#
muertes2017 <- read_csv("Personal/Trabajo de Grado - Demografia/Datos/Defunciones/nofetal2017/nofetal2017.csv")

muertes2017 <- select(muertes2017, SEXO,GRU_ED1, IDPERTET)
muertes2017 <- filter(muertes2017, IDPERTET != 6 & IDPERTET != 9)
muertes2017_def <- muertes2017 %>%
  select(SEXO,GRU_ED1, IDPERTET) %>%
  group_by(SEXO,GRU_ED1, IDPERTET) %>%
  summarise(defunciones = n())  
muertes2017_def$SEXO <- ifelse(muertes2017_def$SEXO == 1, "Hombre", ifelse(muertes2017_def$SEXO == 2, "Mujer", muertes2017_def$SEXO))
muertes2017_def$IDPERTET<-ifelse(muertes2017_def$IDPERTET == 1, "Indígena", 
                                 ifelse(muertes2017_def$IDPERTET == 2, "Gitano(a) o Rrom",
                                        ifelse(muertes2017_def$IDPERTET == 3, "Raizal del Archipielago de San Andrés, Providencia y Santa Catalina", 
                                               ifelse(muertes2017_def$IDPERTET == 4, "Palenquero(a) de San Basilio",
                                                      ifelse(muertes2017_def$IDPERTET == 5, "Negro(a), Mulato(a), Afrodescendiente, Afrocolombiano(a)", muertes2017_def$IDPERTET)))))

muertes2017_def$GRU_ED1<-as.numeric(muertes2017_def$GRU_ED1)
Grupo_de_Edad <- read_csv("Personal/Trabajo de Grado - Demografia/Datos/Defunciones/Grupo de Edad - Defunciones.csv")
Grupo_de_Edad$GRU_ED1<-as.factor(Grupo_de_Edad$GRU_ED1)
muertes2017_def$GRU_ED1<-as.factor(muertes2017_def$GRU_ED1)
muertes2017_def <-inner_join(muertes2017_def,Grupo_de_Edad,by = "GRU_ED1")
muertes2017_def <-muertes2017_def[,c(1,5,3,4)]

muertes2017_def <- muertes2017_def %>%
  mutate(Grupo_de_Edad = ifelse(Grupo_de_Edad == "00 A 04 Years", "Infantil", "Total_poblacion")) %>%
  group_by(IDPERTET, SEXO, Grupo_de_Edad) %>%
  summarise(defunciones_rep = sum(defunciones))
muertes2017_def<-muertes2017_def[muertes2017_def$SEXO!=3,]
mujer_palenquera_infantil<-data.frame(IDPERTET = "Palenquero(a) de San Basilio", 
                                                    SEXO = "Mujer",
                                                    Grupo_de_Edad = "Infantil",
                                                    defunciones_rep = 0)
muertes2017_def<-rbind(muertes2017_def,mujer_palenquera_infantil)

#------------------- Factor de correccion: certificado de defunción -------------------------------#
#--------------------  Ingesta de Datos: variables demograficas -----------------------------------#
setwd("/Users/mmedinar/Documents/Personal/Trabajo de Grado - Demografia/Datos/Censo 2018/")

Antioquia <- read_csv("05Antioquia/05_Antioquia_CSV/CNPV2018_5PER_A2_05.CSV")
Antioquia <- select(Antioquia,  PA1_GRP_ETNIC, COD_ENCUESTAS)
Antioquia <- filter(Antioquia, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)
 
Bogota <- read_csv("11Bogota/11_Bogota_CSV/CNPV2018_5PER_A2_11.CSV")
Bogota <- select(Bogota,  PA1_GRP_ETNIC, COD_ENCUESTAS)
Bogota <- filter(Bogota, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)

Atlantico <- read_csv("08Atlantico/08_Atlantico_CSV/CNPV2018_5PER_A2_08.CSV")
Atlantico <- select(Atlantico,  PA1_GRP_ETNIC, COD_ENCUESTAS)
Atlantico <- filter(Atlantico, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)

Vichada <- read_csv("99Vichada/99_Vichada_CSV/CNPV2018_5PER_A2_99.CSV")
Vichada <- select(Vichada,  PA1_GRP_ETNIC, COD_ENCUESTAS)
Vichada <- filter(Vichada, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)

Vaupes <- read_csv("97Vaupes/97_Vaupes_CSV/CNPV2018_5PER_A2_97.CSV")
Vaupes <- select(Vaupes,  PA1_GRP_ETNIC, COD_ENCUESTAS)
Vaupes <- filter(Vaupes, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)

Guaviare <- read_csv("95Guaviare/95_Guaviare_CSV/CNPV2018_5PER_A2_95.CSV")
Guaviare <- select(Guaviare,  PA1_GRP_ETNIC, COD_ENCUESTAS)
Guaviare <- filter(Guaviare, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)

Guainia <- read_csv("94Guainia/94_Guainia_CSV/CNPV2018_5PER_A2_94.CSV")
Guainia <- select(Guainia,  PA1_GRP_ETNIC, COD_ENCUESTAS)
Guainia <- filter(Guainia, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)

Amazonas <- read_csv("91Amazonas/91_Amazonas_CSV/CNPV2018_5PER_A2_91.CSV")
Amazonas <- select(Amazonas,  PA1_GRP_ETNIC, COD_ENCUESTAS)
Amazonas <- filter(Amazonas, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)

SyP <- read_csv("88SanAndresProvidenciaYSantaCatalina/88_SanAndresProvidenciaYSantacatalina_CSV/CNPV2018_5PER_A2_88.CSV")
SyP <- select(SyP,  PA1_GRP_ETNIC, COD_ENCUESTAS)
SyP <- filter(SyP, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)

Putumayo <- read_csv("86Putumayo/86_Putumayo_CSV/CNPV2018_5PER_A2_86.CSV")
Putumayo <- select(Putumayo,  PA1_GRP_ETNIC, COD_ENCUESTAS)
Putumayo <- filter(Putumayo, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)

Casanare <- read_csv("85Casanare/85_Casanare_CSV/CNPV2018_5PER_A2_85.CSV")
Casanare <- select(Casanare,  PA1_GRP_ETNIC, COD_ENCUESTAS)
Casanare <- filter(Casanare, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)

Arauca <- read_csv("81Arauca/81_Arauca_CSV/CNPV2018_5PER_A2_81.CSV")
Arauca <- select(Arauca,  PA1_GRP_ETNIC, COD_ENCUESTAS)
Arauca <- filter(Arauca, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)

ValleDelCauca <- read_csv("76ValleDelCauca/76_ValleDelCauca_CSV/CNPV2018_5PER_A2_76.CSV")
ValleDelCauca <- select(ValleDelCauca,  PA1_GRP_ETNIC, COD_ENCUESTAS)
ValleDelCauca <- filter(ValleDelCauca, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)

Tolima <- read_csv("73Tolima/73_Tolima_CSV/CNPV2018_5PER_A2_73.CSV")
Tolima <- select(Tolima,  PA1_GRP_ETNIC, COD_ENCUESTAS)
Tolima <- filter(Tolima, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)

Sucre <- read_csv("70Sucre/70_Sucre_CSV/CNPV2018_5PER_A2_70.CSV")
Sucre <- select(Sucre,  PA1_GRP_ETNIC, COD_ENCUESTAS)
Sucre <- filter(Sucre, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)

Santander <- read_csv("68Santander/68_Santander_CSV/CNPV2018_5PER_A2_68.CSV")
Santander <- select(Santander,  PA1_GRP_ETNIC, COD_ENCUESTAS)
Santander <- filter(Santander, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)

Risaralda <- read_csv("66Risaralda/66_Risaralda_CSV/CNPV2018_5PER_A2_66.CSV")
Risaralda <- select(Risaralda,  PA1_GRP_ETNIC, COD_ENCUESTAS)
Risaralda <- filter(Risaralda, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)

Quindio <- read_csv("63Quindio/63_Quindio_CSV/CNPV2018_5PER_A2_63.CSV")
Quindio <- select(Quindio,  PA1_GRP_ETNIC, COD_ENCUESTAS)
Quindio <- filter(Quindio, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)

Narino <- read_csv("52Narino/52_Narino_CSV/CNPV2018_5PER_A2_52.CSV")
Narino <- select(Narino,  PA1_GRP_ETNIC, COD_ENCUESTAS)
Narino <- filter(Narino, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)

NorteDeSantander <- read_csv("54NorteDeSantander/54_NorteDeSantander_CSV/CNPV2018_5PER_A2_54.CSV")
NorteDeSantander <- select(NorteDeSantander,  PA1_GRP_ETNIC, COD_ENCUESTAS)
NorteDeSantander <- filter(NorteDeSantander, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)

Meta <- read_csv("50Meta/50_Meta_CSV/CNPV2018_5PER_A2_50.CSV")
Meta <- select(Meta,  PA1_GRP_ETNIC, COD_ENCUESTAS)
Meta <- filter(Meta, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)

Magdalena <- read_csv("47Magdalena/47_Magdalena_CSV/CNPV2018_5PER_A2_47.CSV")
Magdalena <- select(Magdalena,  PA1_GRP_ETNIC, COD_ENCUESTAS)
Magdalena <- filter(Magdalena, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)

LaGuajira <- read_csv("44LaGuajira/44_LaGuajira_CSV/CNPV2018_5PER_A2_44.CSV")
LaGuajira <- select(LaGuajira,  PA1_GRP_ETNIC, COD_ENCUESTAS)
LaGuajira <- filter(LaGuajira, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)

Huila <- read_csv("41Huila/41_Huila_CSV/CNPV2018_5PER_A2_41.CSV")
Huila <- select(Huila,  PA1_GRP_ETNIC, COD_ENCUESTAS)
Huila <- filter(Huila, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)

Choco <- read_csv("27Choco/27_Choco_CSV/CNPV2018_5PER_A2_27.CSV")
Choco <- select(Choco,  PA1_GRP_ETNIC, COD_ENCUESTAS)
Choco <- filter(Choco, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)

Cundinamarca <- read_csv("25Cundinamarca/25_Cundinamarca_CSV/CNPV2018_5PER_A2_25.CSV")
Cundinamarca <- select(Cundinamarca,  PA1_GRP_ETNIC, COD_ENCUESTAS)
Cundinamarca <- filter(Cundinamarca, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)

Cordoba <- read_csv("23Cordoba/23_Cordoba_CSV/CNPV2018_5PER_A2_23.CSV")
Cordoba <- select(Cordoba,  PA1_GRP_ETNIC, COD_ENCUESTAS)
Cordoba <- filter(Cordoba, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)

Cesar <- read_csv("20Cesar/20_Cesar_CSV/CNPV2018_5PER_A2_20.CSV")
Cesar <- select(Cesar,  PA1_GRP_ETNIC, COD_ENCUESTAS)
Cesar <- filter(Cesar, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)

Cauca <- read_csv("19Cauca/19_Cauca_CSV/CNPV2018_5PER_A2_19.CSV")
Cauca <- select(Cauca,  PA1_GRP_ETNIC, COD_ENCUESTAS)
Cauca <- filter(Cauca, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)

Caqueta <- read_csv("18Caqueta/18_Caqueta_CSV/CNPV2018_5PER_A2_18.CSV")
Caqueta <- select(Caqueta,  PA1_GRP_ETNIC, COD_ENCUESTAS)
Caqueta <- filter(Caqueta, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)

Caldas <- read_csv("17Caldas/17_Caldas_CSV/CNPV2018_5PER_A2_17.CSV")
Caldas <- select(Caldas,  PA1_GRP_ETNIC, COD_ENCUESTAS)
Caldas <- filter(Caldas, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)

Boyaca <- read_csv("15Boyaca/15_Boyaca_CSV/CNPV2018_5PER_A2_15.CSV")
Boyaca <- select(Boyaca,  PA1_GRP_ETNIC, COD_ENCUESTAS)
Boyaca <- filter(Boyaca, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)

Bolivar <- read_csv("13Bolivar/13_Bolivar_CSV/CNPV2018_5PER_A2_13.CSV")
Bolivar <- select(Bolivar,  PA1_GRP_ETNIC, COD_ENCUESTAS)
Bolivar <- filter(Bolivar, PA1_GRP_ETNIC != 6 & PA1_GRP_ETNIC != 9)

#------------------------------ CENSO 2018 ------------------------------------#

censo2018<-rbind(Amazonas,Antioquia,Arauca,Atlantico,Bogota,
                 Bolivar,Boyaca,Caldas,Caqueta,Casanare,       
                 Cauca,Cesar,Choco,Cordoba,Cundinamarca,
                 Guainia,Guaviare,Huila,LaGuajira,Magdalena,       
                 Meta,Narino,NorteDeSantander,Putumayo,Quindio,
                 Risaralda,Santander,Sucre,SyP ,Tolima,         
                 ValleDelCauca,Vaupes,Vichada )

#censo2018 <- censo2018 %>%
#  select(P_SEXO, P_EDADR, PA1_GRP_ETNIC,pop) %>%
#  group_by(P_SEXO, P_EDADR, PA1_GRP_ETNIC) %>%
#  summarise(pop_2018 = sum(pop))

censo2018$PA1_GRP_ETNIC<- ifelse(censo2018$PA1_GRP_ETNIC == 1, "Indígena", 
                                 ifelse(censo2018$PA1_GRP_ETNIC == 2, "Gitano(a) o Rrom",
                                        ifelse(censo2018$PA1_GRP_ETNIC == 3, "Raizal del Archipielago de San Andrés, Providencia y Santa Catalina", 
                                               ifelse(censo2018$PA1_GRP_ETNIC == 4, "Palenquero(a) de San Basilio",
                                                      ifelse(censo2018$PA1_GRP_ETNIC == 5, "Negro(a), Mulato(a), Afrodescendiente, Afrocolombiano(a)", censo2018$PA1_GRP_ETNIC)))))
censo2018 <- distinct(censo2018, PA1_GRP_ETNIC, COD_ENCUESTAS)

#--------------------------------------------------------------------------------------------------#
#--------------------  Ingesta de Datos: certificado defunción ------------------------------------#
#--------------------------------------------------------------------------------------------------#

Antioquia <- read_csv("05Antioquia/05_Antioquia_CSV/CNPV2018_3FALL_A2_05.CSV")
Antioquia <- select(Antioquia,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

Bogota <- read_csv("11Bogota/11_Bogota_CSV/CNPV2018_3FALL_A2_11.CSV")
Bogota <- select(Bogota,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

Atlantico <- read_csv("08Atlantico/08_Atlantico_CSV/CNPV2018_3FALL_A2_08.CSV")
Atlantico <- select(Atlantico,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

Vichada <- read_csv("99Vichada/99_Vichada_CSV/CNPV2018_3FALL_A2_99.CSV")
Vichada <- select(Vichada,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

Vaupes <- read_csv("97Vaupes/97_Vaupes_CSV/CNPV2018_3FALL_A2_97.CSV")
Vaupes <- select(Vaupes,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

Guaviare <- read_csv("95Guaviare/95_Guaviare_CSV/CNPV2018_3FALL_A2_95.CSV")
Guaviare <- select(Guaviare,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

Guainia <- read_csv("94Guainia/94_Guainia_CSV/CNPV2018_3FALL_A2_94.CSV")
Guainia <- select(Guainia,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

Amazonas <- read_csv("91Amazonas/91_Amazonas_CSV/CNPV2018_3FALL_A2_91.CSV")
Amazonas <- select(Amazonas,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

SyP <- read_csv("88SanAndresProvidenciaYSantaCatalina/88_SanAndresProvidenciaYSantacatalina_CSV/CNPV2018_3FALL_A2_88.CSV")
SyP <- select(SyP,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

Putumayo <- read_csv("86Putumayo/86_Putumayo_CSV/CNPV2018_3FALL_A2_86.CSV")
Putumayo <- select(Putumayo,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

Casanare <- read_csv("85Casanare/85_Casanare_CSV/CNPV2018_3FALL_A2_85.CSV")
Casanare <- select(Casanare,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

Arauca <- read_csv("81Arauca/81_Arauca_CSV/CNPV2018_3FALL_A2_81.CSV")
Arauca <- select(Arauca,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

ValleDelCauca <- read_csv("76ValleDelCauca/76_ValleDelCauca_CSV/CNPV2018_3FALL_A2_76.CSV")
ValleDelCauca <- select(ValleDelCauca,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

Tolima <- read_csv("73Tolima/73_Tolima_CSV/CNPV2018_3FALL_A2_73.CSV")
Tolima <- select(Tolima,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

Sucre <- read_csv("70Sucre/70_Sucre_CSV/CNPV2018_3FALL_A2_70.CSV")
Sucre <- select(Sucre,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

Santander <- read_csv("68Santander/68_Santander_CSV/CNPV2018_3FALL_A2_68.CSV")
Santander <- select(Santander,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

Risaralda <- read_csv("66Risaralda/66_Risaralda_CSV/CNPV2018_3FALL_A2_66.CSV")
Risaralda <- select(Risaralda,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

Quindio <- read_csv("63Quindio/63_Quindio_CSV/CNPV2018_3FALL_A2_63.CSV")
Quindio <- select(Quindio,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

Narino <- read_csv("52Narino/52_Narino_CSV/CNPV2018_3FALL_A2_52.CSV")
Narino <- select(Narino,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

NorteDeSantander <- read_csv("54NorteDeSantander/54_NorteDeSantander_CSV/CNPV2018_3FALL_A2_54.CSV")
NorteDeSantander <- select(NorteDeSantander,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

Meta <- read_csv("50Meta/50_Meta_CSV/CNPV2018_3FALL_A2_50.CSV")
Meta <- select(Meta,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

Magdalena <- read_csv("47Magdalena/47_Magdalena_CSV/CNPV2018_3FALL_A2_47.CSV")
Magdalena <- select(Magdalena,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

LaGuajira <- read_csv("44LaGuajira/44_LaGuajira_CSV/CNPV2018_3FALL_A2_44.CSV")
LaGuajira <- select(LaGuajira,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

Huila <- read_csv("41Huila/41_Huila_CSV/CNPV2018_3FALL_A2_41.CSV")
Huila <- select(Huila,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

Choco <- read_csv("27Choco/27_Choco_CSV/CNPV2018_3FALL_A2_27.CSV")
Choco <- select(Choco,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

Cundinamarca <- read_csv("25Cundinamarca/25_Cundinamarca_CSV/CNPV2018_3FALL_A2_25.CSV")
Cundinamarca <- select(Cundinamarca,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

Cordoba <- read_csv("23Cordoba/23_Cordoba_CSV/CNPV2018_3FALL_A2_23.CSV")
Cordoba <- select(Cordoba,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

Cesar <- read_csv("20Cesar/20_Cesar_CSV/CNPV2018_3FALL_A2_20.CSV")
Cesar <- select(Cesar,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

Cauca <- read_csv("19Cauca/19_Cauca_CSV/CNPV2018_3FALL_A2_19.CSV")
Cauca <- select(Cauca,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

Caqueta <- read_csv("18Caqueta/18_Caqueta_CSV/CNPV2018_3FALL_A2_18.CSV")
Caqueta <- select(Caqueta,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

Caldas <- read_csv("17Caldas/17_Caldas_CSV/CNPV2018_3FALL_A2_17.CSV")
Caldas <- select(Caldas,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

Boyaca <- read_csv("15Boyaca/15_Boyaca_CSV/CNPV2018_3FALL_A2_15.CSV")
Boyaca <- select(Boyaca,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

Bolivar <- read_csv("13Bolivar/13_Bolivar_CSV/CNPV2018_3FALL_A2_13.CSV")
Bolivar <- select(Bolivar,FA2_SEXO_FALL, FA3_EDAD_FALL, FA4_CERT_DEFUN, COD_ENCUESTAS)

#------------------------------ certificado_defunciones 2017 ------------------------------------#

certificado_defunciones_2017<-rbind(Amazonas,Antioquia,Arauca,Atlantico,Bogota,
                 Bolivar,Boyaca,Caldas,Caqueta,Casanare,       
                 Cauca,Cesar,Choco,Cordoba,Cundinamarca,
                 Guainia,Guaviare,Huila,LaGuajira,Magdalena,       
                 Meta,Narino,NorteDeSantander,Putumayo,Quindio,
                 Risaralda,Santander,Sucre,SyP ,Tolima,         
                 ValleDelCauca,Vaupes,Vichada )

#censo2018 <- censo2018 %>%
#  select(P_SEXO, P_EDADR, PA1_GRP_ETNIC,pop) %>%
#  group_by(P_SEXO, P_EDADR, PA1_GRP_ETNIC) %>%
#  summarise(pop_2018 = sum(pop))

certificado_defunciones_2017$FA4_CERT_DEFUN<- ifelse(certificado_defunciones_2017$FA4_CERT_DEFUN == 1, "Si", 
                                 ifelse(certificado_defunciones_2017$FA4_CERT_DEFUN == 2, "No",
                                        ifelse(certificado_defunciones_2017$FA4_CERT_DEFUN == 3, "No sabe", 
                                               ifelse(certificado_defunciones_2017$FA4_CERT_DEFUN == 9, "No informa", certificado_defunciones_2017$FA4_CERT_DEFUN))))

certificado_defunciones_2017$FA2_SEXO_FALL <- ifelse(certificado_defunciones_2017$FA2_SEXO_FALL == 1, "Hombre", 
                                              ifelse(certificado_defunciones_2017$FA2_SEXO_FALL == 2, "Mujer",
                                                     certificado_defunciones_2017$FA2_SEXO_FALL))

names(Grupo_de_Edad)<-c("FA3_EDAD_FALL","Grupo_de_Edad")
Grupo_de_Edad$FA3_EDAD_FALL<-as.numeric(Grupo_de_Edad$FA3_EDAD_FALL)
certificado_defunciones_2017 <-inner_join(certificado_defunciones_2017,Grupo_de_Edad,by = "FA3_EDAD_FALL")
certificado_defunciones_2017 <-certificado_defunciones_2017[,c(1,5,3,4)]

#----------------------------------------- JOIN factor_correccion ---------------------------------------------#

datos_defunciones<-inner_join(censo2018,certificado_defunciones_2017,by = join_by(COD_ENCUESTAS))

datos_defunciones <- datos_defunciones %>%
                      count(PA1_GRP_ETNIC, FA2_SEXO_FALL, Grupo_de_Edad, FA4_CERT_DEFUN) %>%
                      pivot_wider(names_from = FA4_CERT_DEFUN, values_from = n, values_fill = 0)
datos_defunciones <- datos_defunciones %>%
                    mutate(Grupo_de_Edad = ifelse(Grupo_de_Edad == "00 A 04 Years", "Infantil", "Total_poblacion")) %>%
                    group_by(PA1_GRP_ETNIC, FA2_SEXO_FALL, Grupo_de_Edad) %>%
                    summarise(No = sum(No),
                              `No informa` = sum(`No informa`),
                              Si = sum(Si))

datos_defunciones <- transform(datos_defunciones, factor_correcion = (No + `No informa` + Si) / Si)

factor_correccion <- datos_defunciones %>%
                      group_by(PA1_GRP_ETNIC, FA2_SEXO_FALL, Grupo_de_Edad) %>%
                      mutate(factor_correcion = ifelse(factor_correcion == Inf,
                                                       mean(factor_correcion[factor_correcion != Inf], na.rm = TRUE), factor_correcion))
factor_correccion$factor_correcion[13]<-3.25
factor_correccion$factor_correcion[15]<-3
factor_correccion$factor_correcion[19]<-5

#----------------------------------------- Busca ativa ---------------------------------------------#
factor_correccion<-factor_correccion[,c(1,2,3,7)]
names(factor_correccion)<-c("IDPERTET","SEXO","Grupo_de_Edad","factor_correcion")

busca.ativa.colombia<-left_join(muertes2017_def,factor_correccion,by = join_by(IDPERTET, SEXO, Grupo_de_Edad))
defunciones_est=busca.ativa.colombia$defunciones_rep*busca.ativa.colombia$factor_correcion

busca.ativa.colombia<-cbind(busca.ativa.colombia,defunciones_est)
names(busca.ativa.colombia)<-c("IDPERTET","SEXO","Grupo_de_Edad","defunciones_rep","factor_correcion","defunciones_est")


busca.ativa.colombia<-busca.ativa.colombia[,-5]
# Pivotar las columnas
busca.ativa.colombia <- pivot_wider(data = busca.ativa.colombia, 
                          names_from = Grupo_de_Edad, 
                          values_from = c(defunciones_rep, defunciones_est),
                          names_glue = "{Grupo_de_Edad}_{.value}")
#write.csv(busca.ativa.colombia,"busca.ativa.colombia.csv")

#----------------------------------------- Coverage  ---------------------------------------------#

est=0.9

xbar = mean(est)
s2   = 0.1^2
0.1/0.9

K = xbar*(1-xbar)/s2 - 1
a = xbar * K
b = (1-xbar)*K
a
b

#------------------------------------------------------------------------------------------------#

busca.ativa.colombia<-cbind(busca.ativa.colombia,a,b)
names(busca.ativa.colombia)<-c("IDPERTET","SEXO","Infantil_defunciones_rep","Total_poblacion_defunciones_rep",
                               "Infantil_defunciones_est","Total_poblacion_defunciones_est","a","b")

#write.csv(busca.ativa.colombia,"busca.ativa.colombia.csv")





