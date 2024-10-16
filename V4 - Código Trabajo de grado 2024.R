rm(list=ls())   
setwd("/Users/mmedinar/Documents")

set.seed(6447100)

#-----------------------
# Librerias
#----------------------

library(dplyr)
library(splines)
library(rstan)
library(readr)
library(ggplot2)
library(tidyr)
library(loo)
library(xtable)

#-----------------------
# Parametros de Trabajo
#----------------------
save.fit  = TRUE
nchains   = 4
niter     = 1200
warmup    = 200

#------------------------------------
# Datos y otra información basica
#------------------------------------

datos <- read_delim("Personal/Trabajo de Grado - Demografia/Datos/V2_Datos_Defunciones_Censo2018_Definitivo.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
datos <- datos[,-1]

busca_ativa_colombia <- read_csv("~/Documents/Personal/Trabajo de Grado - Demografia/Datos/busca.ativa.colombia.csv")
busca_ativa_colombia <- busca_ativa_colombia[,-1]

## Se calcula el lambda estandar de colombia para el 2018 por sexo: 
##    resultado es una matriz 42 X 3 (21 grupos de edades x 2 sexos Hombre y Mujer)

tmp_COL = datos %>%
  group_by(SEXO,Grupo_de_Edad) %>%
  summarize(lambda = log( sum(defunciones)/sum(pop_2018)))


## Convertir la data de los Lambda en columnas para hombres y mujeres

BRstd_COL = matrix(tmp_COL$lambda, ncol=2, dimnames=list(0:20, c('Hombre','Mujer')))

#------------------------------------
# Versión suavizada: Se proyecta la data de los lambda sobre una spline cúbico con nodos espaciados
#------------------------------------

basis = bs(0:20, knots=seq(0,20,3))
Proj  = basis %*% solve(crossprod(basis)) %*% t(basis)

BRstd_COL = Proj %*% BRstd_COL # Calculo del Lambda * para hombres y mujeres Colombia 2018

#------------------------------------
# Se construye una matriz B-spline lineal (21 x 7) (21 grupos de edad segun el censo y 7 nodos)
#------------------------------------

age = 1:21
B   = bs(age, knots=c(1,3,5,8,12,18), degree=1) #c(00 A 04 Years,10 A 14 Years,20 A 24 Years,35 A 39 Years,55 A 59 Years,85 A 89 Years)
nalpha = ncol(B)
dim(B)

#------------------------------------
# Proporción de muertes reportadas en 2018, por sexo. en grupos de edades 0, 1-29, 30+
#------------------------------------

frac.deaths           = cbind(Hombre=c(0.002275313, 0.2352105, 0.7625142), 
                              Mujer=c(0.003011571, 0.1825963, 0.8143921))
rownames(frac.deaths) = c('0', '1-29', '30+')

#-------------------------------------------------------------------------------------#
# CONSTRUCCIÓN DEL MODELO TOPALS
#-------------------------------------------------------------------------------------#

#######################################################
# STAN MODEL CON REGISTROS INCOMPLETOS
#######################################################

model.name='incomplete registration'

stanModelText = "
data {
  int<lower=0> R;       // numero de etnias
  int<lower=0> S;       // numero de etnias 
  int<lower=0> A;       // 21 grupos de edades
  int<lower=0> K;       // Numero de funciones base (usualmente son 7) 
  
  matrix[A,K] B;             // función de splines base ( 21 x 7) 
  vector[A] std_schedule;    // std logmu programado para modelo TOPALS

  vector[3] death_frac;    // Proporción de muertes reportadas: 0, 1-29, 30+
  
  vector<lower=0,upper=1>[R] pihat1;    //  pi[r,1] ~ beta( K1*pihat1[r], K1*(1-pihat1[r]))
  vector<lower=0,upper=1>[R] pihat_all;  //  pi[r,ALL] ~ beta( Kall*pihat_all[r], Kall*(1-pihat_all[r]))
  
  vector[S] a3 ;                      //  pibar[3] ~ beta(a3,b3)  Estado Agregado
  vector[S] b3 ;          
  
  matrix<lower=0>[A,R] N      ;  // Población expuesta específica por edad y etnia
  int<lower=0>         D[A,R] ;  // Defunciones observadas por edad y etnia

}

transformed data {
  matrix[A,R] lambda_star;   // std logmu programado para TOPALS en cada columna
  
  lambda_star = rep_matrix(std_schedule, R);
}

parameters {
  matrix[K,R] alpha;   // Matriz alpha de compensaciones para TOPALS
  
  simplex[4] eta[R]; // Etas usados para construir (pi1, pi2, pi3)
  
  real<lower=0> K1;    // (hiper)parámetro de precisión para Etnias: pi1 
  real<lower=0> Kall;  // (hiper)parámetro de precisión para Etnias: piall
}

transformed parameters {
  matrix<lower=0,upper=1>[R,3] pi; 
  vector<lower=0,upper=1>[R]   piall;    // Probabilidades de cobertura en todas las edades combinadas
  
  for (etnia in 1:R) {
  
    pi[etnia,1] = eta[etnia,1];
    pi[etnia,2] = eta[etnia,1] + eta[etnia,2] + eta[etnia,3];  
    pi[etnia,3] = eta[etnia,1] + eta[etnia,2];    
  
  } # for etnia
  
  piall = pi * death_frac;   // El resultado es un vector R de coberturas promedio (De todas las edades) por Etnia.
  
 //agg_pi3 = dot_product( pi[,3], agg_wts); 
  
}

model {
  matrix[A,R]    lambda;               // Tasas de log mx 
  matrix[K-1,R]  alpha_diff;           // Diferencias en compensaciones sobre grupos adyacentes
  
  lambda      = lambda_star + B * alpha ;   // Estimaciones del logmu por etnia para los grupos de edades

  alpha_diff  = alpha[2:K,] - alpha[1:(K-1),];   // Primeras diferencias en alphas
  
  ## LIKELIHOOD: Distribución Poisson para las muertes    
  ## log(N) + lambda + log(pi) = ln(expect death reports)       
  
  for (etnia in 1:R) {
    D[1,etnia]    ~ poisson_log( log(N[1,etnia])    + lambda[1,etnia]     + log(pi[etnia,1])) ;  
    D[2:6,etnia] ~ poisson_log( log(N[2:6,etnia]) + lambda[2:6,etnia]  + log(pi[etnia,2])) ;  
    D[7:A,etnia] ~ poisson_log( log(N[7:A,etnia]) + lambda[7:A, etnia] + log(pi[etnia,3])) ;  
  }
  
  ## Previas
  to_vector(alpha)       ~ normal(0,4);
  to_vector(alpha_diff)  ~ normal(0, 1/sqrt(2));

  K1      ~ exponential(0.05) ;   // Exponencial truncada para la precisión de K1
  Kall    ~ exponential(0.05) ;   // Exponencial truncada para la precisión de Kall
  
  pi[,1] ~ beta(     (5+K1)*pihat1, (5+K1)*(1-pihat1) );
  
  piall  ~ beta( (5+Kall)*pihat_all, (5+Kall)*(1-pihat_all)  ) ;    // Vectorización: Priori del total de coberturas para el total de las Etnias 1..R
  
  if (S  > 1) pi[,3]  ~ beta(a3, b3 );    // Previa para los niveles de las etnias con valores pi3 

} //modelo
"
#######################################################
# STAN MODEL CON 100% REGISTROS (all pi=1)
#######################################################

stanModelText_complete_reg = "
data {
  int<lower=0> R;       // numero de etnias
  int<lower=0> S;       // numero de etnias 
  int<lower=0> A;       // 21 grupos de edades
  int<lower=0> K;       // Numero de funciones base (usualmente son 7) 
  
  matrix[A,K] B;             // función de splines base ( 21 x 7) 
  vector[A] std_schedule;    // std logmu programado para modelo TOPALS
  
  matrix<lower=0>[A,R] N      ;  // Población expuesta específica por edad y etnia
  int<lower=0>         D[A,R] ;  // Defunciones observadas por edad y etnia
}

transformed data {
  matrix[A,R] lambda_star;   // std logmu programado para TOPALS en cada columna

  lambda_star = rep_matrix(std_schedule, R);
}

parameters {
  matrix[K,R] alpha;   // Matriz alpha de compensaciones para TOPALS
}

model {
  matrix[A,R]   lambda;               // Tasas log mx 
  matrix[K-1,R] alpha_diff;           // Diferencias en compensaciones sobre grupos adyacentes
  
  lambda      = lambda_star + B * alpha ;        // Estimaciones del logmu por etnia para los grupos de edades

  alpha_diff  = alpha[2:K,] - alpha[1:(K-1),];   // Primeras diferencias en alphas
  
  ## LIKELIHOOD: Distribución Poisson para las muertes   
  ## log(N) + lambda + log(pi) = ln(expect death reports)       
  
  for (etnia in 1:R) {
    D[,etnia]    ~ poisson_log( log(N[,etnia]) + lambda[,etnia]  ) ;  
  }
  
  ## PREVIAS
  to_vector(alpha)       ~ normal(0,4);
  to_vector(alpha_diff)  ~ normal(0, 1/sqrt(2));

} //modelO
"

#------------------------------------
# Compilación del Modelo
#------------------------------------

part_reg_model  = stan_model( model_code = stanModelText)
full_reg_model  = stan_model( model_code = stanModelText_complete_reg)

#-------------------------------------------------------------------------------------#
# BUCLE PRINCIPAL SOBRE LOS CASOS POR SEXO Y ETNIA
#-------------------------------------------------------------------------------------#


## Se generan los casos para cada combinación de sexo a nivel de etnia
case = expand.grid(sex=c('Mujer','Hombre'), 
                   big_area = datos$IDPERTET ) 

case = distinct(case)

#------------------------------------

time.stamp = format(Sys.time(), '%d%b%y-%H%M')

for (casenum in 1:nrow(case)) {
  
  print(case[casenum,])
  
  this.sex = as.character( case$sex[casenum])
  
  geog   = list(big   = case$big_area[casenum])
  
  geog.label = paste0('-within-',geog$big)
  
  sex.label  = c(Hombre='-Male',Mujer='-Female')[this.sex]
  
  fitfile    = paste0('Stan-',time.stamp, geog.label, sex.label,'.RData')
  
  busca_ativa_colombia$selected.code = busca_ativa_colombia$IDPERTET
  
  field_audit.df =  busca_ativa_colombia %>%
    group_by(selected.code) %>%
    summarize( pi1.hat   = max( .005, min(.995, sum(Infantil_defunciones_rep)/ sum(Infantil_defunciones_est))),
               piall.hat = max( .005, min(.995, sum(Total_poblacion_defunciones_rep)/ sum(Total_poblacion_defunciones_est)))) %>%
    ungroup
  
  #------------------------------------
  # Se construyen los datos y previas para etnias
  #------------------------------------
  
  ## DATOS
  ## Paso 1. leer los datos de muerte y exposición, mantener el sexo seleccionado y todas las etnias seleccionadas
  
  tmp = datos %>% filter(SEXO == this.sex)
  
  ## Paso 2. agregar los datos restantes en las etnias
  
  tmp$selected.code = tmp$IDPERTET
  
  tmp = tmp %>%
    group_by(selected.code, Grupo_de_Edad) %>%
    summarize(D = sum(defunciones), N=sum(pop_2018)) 
  
  D = matrix(tmp$D, nrow=21)   # 21 grupos de edad x 5 etnias matriz de conteo de defunciones
  N = matrix(tmp$N, nrow=21)   # 21 grupos de edad x 5 etnias matriz de expuestos
  
  colnames(D) = colnames(N) = unique(tmp$selected.code)
  
  ## Nombres de etnias para etiquetado posterior

  region.names = unique(busca_ativa_colombia$IDPERTET)
  
  ## PREVIAS INFORMATIVAS
  
  ## Párametros beta de la cobertura
  ab = filter(busca_ativa_colombia, SEXO==this.sex) %>%
    select(a:b) %>%
    as.matrix()
  
  rownames(ab) = unique(busca_ativa_colombia$IDPERTET)
  
  prior.a.pi3bar = ab[,1]
  prior.b.pi3bar = ab[,2]
  
  ## Eestimaciones ponderadas de puntos para la cobertura a edades 0 
  ## y en todas las edades, del modelo busca activa ó auditoria
  ## ASEGURARSE DE QUE ESTÉN EN EL MISMO ORDEN QUE LAS FILAS de D y N
  
  o = match( colnames(D), field_audit.df$selected.code)

  prior.mean.pi1   = field_audit.df$pi1.hat[o]      # R x 1
  prior.mean.piall = field_audit.df$piall.hat[o]    # R x 1
  
  
  #######################################################
  # DATOS DE STAN
  #######################################################
  
  R=ncol(N)
  S=ncol(N)
  A=nrow(N)
  K=ncol(B)
  
  stanDataList <- list(
    R=R,
    S=S,
    A=A,
    K=K,
    
    B            = B,
    std_schedule = BRstd_COL[,this.sex],
    death_frac = frac.deaths[,this.sex],
    
    pihat1    = prior.mean.pi1,    
    pihat_all = prior.mean.piall,
    a3    = array(prior.a.pi3bar, dim=S),
    b3    = array(prior.b.pi3bar, dim=S),
    
    N = pmax(N, .01),
    D = D
  )
  
  #######################################################
  # Iniciación de la función STAN
  #######################################################
  
  stanInits = function() {
    p1 = rbeta(R, 100*prior.mean.pi1, 100*(1-prior.mean.pi1))
    p3 = runif(R, p1, 1)
    p2 = runif(R, p3, 1)
    # Mínimo: p1 = eta1
    # Máximo: p2 = eta1 + eta2 + eta3
    # Medio: p3 = eta1 + eta2
    eta1 = p1
    eta2 = p3-p1
    eta3 = p2-p3
    eta4 = 1-p2
    
    a  = matrix(runif(K*R, -0.10, +0.10), K, R)
    
    list(
      alpha  = a,
      eta  = cbind(eta1,eta2,eta3,eta4),
      
      K1    = rexp(1,.05),
      Kall  = rexp(1,.05)
    )
  }
  
  
  #######################################################
  # MUESTRAS DE LA POSTERIOR PARA EL MODELO INCOMPLETO
  # PROBABILIDAD DE REGISTRO <1
  #######################################################
  
  fit <- sampling(part_reg_model,
                  data    = stanDataList,
                  init    = stanInits,
                  seed    = 6447100,
                  pars    = c('alpha','pi','piall'),
                  control = list(max_treedepth=10),
                  iter    = niter, 
                  warmup  = warmup,
                  thin    = 1, 
                  chains  = nchains)
  
  #######################################################
  # APROXIMACIÓN POSTERIOR PARA EL MODELO COMPLETO CON PI=1
  # (varias pruebas demostraron que estos resultados son esenciales)
  # Idéntico al muestreo MCMC desde la posterior para pi=1
  #######################################################
  ML = optimizing(full_reg_model, 
                  data  = stanDataList, 
                  init  = stanInits, 
                  seed  = 6447100,
                  draws = 2000)
  
  if (save.fit) save( fit, stanDataList, ML, region.names, file=fitfile)
  
} # POR CASO

#############################################################################################################
#############################################################################################################
#############################################################################################################
#load("~/Documents/Personal/Trabajo de Grado - Demografia/Datos/Corrida 1 - Datos Etnias.RData")

#-------------------------------------------------------------------------------------#
# RESULTADOS HOMBRES
#-------------------------------------------------------------------------------------#
load("/Users/mmedinar/Documents/Personal/Trabajo de Grado - Demografia/Datos/Corrida 2 - Modelo/Stan-04Oct24-0012-within-Male.RData") # HOMBRES

fit_H<-fit
print(fit)
print(ML)

# Calcular la devianza
dev_H <- -2 * fit_H@.MISC[["summary"]][["msd"]][56,1]

# Calcular DIC
dic_H <-  -2 * mean(fit_H@.MISC[["summary"]][["msd"]][56,1]) + 2 * length(datos)

# Imprimir resultados
print(dic_H)

#------------------------------------
# SE CONSTRUYE LAS DEFUNCIONES ESTIMADAS PARA EL MODELO INCOMPLETO
# PROBABILIDAD DE REGISTRO <1
#------------------------------------
etnias <- unique(busca_ativa_colombia$IDPERTET)
parametros_H<-data.frame(fit_H@.MISC[["summary"]][["msd"]])

# Crear un subconjunto solo con las filas que contienen "alpha"
alpha_data_H <- subset(parametros_H, grepl("^alpha", rownames(parametros_H)))

# Extraer la columna 'mean' y reorganizar en un formato de matriz con 7 filas y 5 columnas
alpha_means_H <- matrix(alpha_data_H$mean, nrow = 7, ncol = 5, byrow = F)

# Asignar nombres a las filas y columnas

rownames(alpha_means_H) <- paste0("alpha[", 1:7, "]")
colnames(alpha_means_H) <- etnias

# Crear un subconjunto solo con las filas que contienen "pi"
pi_data_H <- subset(parametros_H, grepl("^pi", rownames(parametros_H)))

# Extraer la columna 'mean' y reorganizar en un formato de matriz con 7 filas y 5 columnas
pi_means_H <- matrix(pi_data_H$mean, nrow = 4, ncol = 5, byrow = T)

rownames(pi_means_H) <- c(paste0("pi[", 1:3, "]"), "piall")
colnames(pi_means_H) <- etnias

# B_x_alpha
B_por_alpha_H<-B%*%(alpha_means_H)
dim(B_por_alpha_H)

#----------------------------------
# ESTIMACION LAMBDA PARA HOMBRES

LAMBDA_aster_H<-BRstd_COL[,1]
length(LAMBDA_aster_H)

# TASAS especificas de mortalidad para HOMBRES x Etnia
mu_H<-exp(LAMBDA_aster_H+B_por_alpha_H)

# Defunciones estimadas para HOMBRES x Etnia
tmp = datos %>% filter(SEXO == "Hombre")

tmp = tmp %>%
  group_by(IDPERTET, Grupo_de_Edad) %>%
  summarize(D = sum(defunciones), N=sum(pop_2018))

N = matrix(tmp$N, nrow=21)
colnames(N) <- etnias
N_Hombres = N

D = matrix(tmp$D, nrow=21)
colnames(D) <- etnias
D_Hombres = D

N_x_Mu_H<-(mu_H)*N # Defunciones estimadas

# Defunciones con coberturas estimadas REGISTRADAS para HOMBRES x Etnia
N_x_Mu_x_pi_H<-N_x_Mu_H*pi_means_H[4,]

#------------------------------------
# SE CONSTRUYE LAS DEFUNCIONES ESTIMADAS PARA EL MODELO COMPLETO
# PROBABILIDAD DE REGISTRO PI=1
#------------------------------------
etnias <- unique(busca_ativa_colombia$IDPERTET)
parametros_COMPLETO_H<-data.frame(ML[["par"]])

# Extraer la columna 'mean' y reorganizar en un formato de matriz con 7 filas y 5 columnas
alpha_means_COMPLETO_H <- matrix(parametros_COMPLETO_H[,1], nrow = 7, ncol = 5, byrow = F)

# Asignar nombres a las filas y columnas

rownames(alpha_means_COMPLETO_H) <- paste0("alpha[", 1:7, "]")
colnames(alpha_means_COMPLETO_H) <- etnias

# B_x_alpha
B_por_alpha_COMPLETO_H<-B%*%(alpha_means_COMPLETO_H)
dim(B_por_alpha_COMPLETO_H)

#----------------------------------
# ESTIMACION PARA HOMBRES MODELO COMPLETO

LAMBDA_aster_H<-BRstd_COL[,1]
length(LAMBDA_aster_H)

# TASAS especificas de mortalidad para HOMBRES x Etnia
mu_H_COMPLETO<-exp(LAMBDA_aster_H+B_por_alpha_COMPLETO_H)

# Defunciones estimadas para HOMBRES x Etnia
N_x_Mu_H_COMPLETO<-(mu_H_COMPLETO)*N 

#---------------------- Graficos Hombres --------------------------------#
Edad<-c(seq(1:21))
log_mu_H<-cbind(data.frame(log(mu_H),Edad))
colnames(log_mu_H) <- c("Gitano","Indigena","Negro_Afrodescendiente","Palenquero","Raizal","Edad")

# Convertir los datos a formato largo (long format)
log_mu_long_H <- log_mu_H %>%
  pivot_longer(cols = -Edad, names_to = "etnia", values_to = "log_mortalidad")

ggplot(log_mu_long_H, aes(x = Edad, y = log_mortalidad, color = etnia)) +
  geom_line(size = 1) +  # Líneas punteadas
  labs(title = "Curva de logaritmo de la tasa de mortalidad por etnia en hombres",
       x = "Grupos de edad",
       y = "Log-Tasa de mortalidad hombres") +
  theme_minimal() +
  theme(legend.title = element_blank(),  # Ocultar el título de la leyenda
        legend.position = "bottom") +    # Mover la leyenda a la parte inferior
  scale_color_manual(values = c("steelblue", "#256D3D", "#6D7980", "firebrick", "goldenrod"))  # Colores sobrios y diferenciados

#-----------------------------------------------------#
# Grafico Defunciones estimadas y registradas x Etnia
#-----------------------------------------------------#

#-------------------------------------#
# Gitano(a) o Rrom
#-------------------------------------#

Gitanos_H_D<-N_x_Mu_H[,1] # Defunciones estimadas
Gitanos_H_R<-N_x_Mu_x_pi_H[,1] # Defunciones Registradas
Gitanos_H<-data.frame(cbind(Gitanos_H_D,Gitanos_H_R,Edad))
colnames(Gitanos_H)<-c("Defunciones_Estimadas_D","Defunciones_Registradas_R","Edad")

# Convertir a formato largo
datos_Gitanos_H <- Gitanos_H %>%
  pivot_longer(cols = -Edad, names_to = "Tipo_Defuncion", values_to = "Cantidad")

# Graficar las curvas
ggplot(datos_Gitanos_H, aes(x = Edad, y = Cantidad, color = Tipo_Defuncion)) +
  geom_line(size = 1) +  # Usar líneas de puntos
  labs(title = "Defunciones Gitanos hombres: Estimadas_D vs Registradas_R",
       x = "Grupos de edad",
       y = "Cantidad de defunciones") +
  theme_minimal() +
  theme(legend.title = element_blank(),  # Ocultar el título de la leyenda
        legend.position = "bottom") +    # Mover la leyenda a la parte inferior
  scale_color_manual(values = c("steelblue", "#ACD4EC"))  # Colores sobrios y diferenciados

#write.csv(datos_Gitanos_H,"Personal/Trabajo de Grado - Demografia/Datos/datos_Gitanos_H.csv")
#-------------------------------------#
# Indígenas
#-------------------------------------#

Indigenas_H_D<-N_x_Mu_H[,2] # Defunciones estimadas
Indigenas_H_R<-N_x_Mu_x_pi_H[,2] # Defunciones Registradas
Indigenas_H<-data.frame(cbind(Indigenas_H_D,Indigenas_H_R,Edad))
colnames(Indigenas_H)<-c("Defunciones_Estimadas_D","Defunciones_Registradas_R","Edad")

# Convertir a formato largo
datos_Indigenas_H <- Indigenas_H %>%
  pivot_longer(cols = -Edad, names_to = "Tipo_Defuncion", values_to = "Cantidad")

# Graficar las curvas
ggplot(datos_Indigenas_H, aes(x = Edad, y = Cantidad, color = Tipo_Defuncion)) +
  geom_line(size = 1) +  # Usar líneas de puntos
  labs(title = "Defunciones Indígenas hombres: Estimadas vs Registradas",
       x = "Grupos de edad",
       y = "Cantidad de defunciones") +
  theme_minimal() +
  theme(legend.title = element_blank(),  # Ocultar el título de la leyenda
        legend.position = "bottom") +    # Mover la leyenda a la parte inferior
  scale_color_manual(values = c("#256D3D", "#80C673"))  # Colores sobrios y diferenciados

#write.table(datos_Indigenas_H, file = "Personal/Trabajo de Grado - Demografia/Datos/datos_Indigenas_H.csv", sep = ";", dec = ",", row.names = FALSE)

#-------------------------------------#
# Negro_Afrodescendiente
#-------------------------------------#

Afrodescendientes_H_D<-N_x_Mu_H[,3] # Defunciones estimadas
Afrodescendientes_H_R<-N_x_Mu_x_pi_H[,3] # Defunciones Registradas
Afrodescendientes_H<-data.frame(cbind(Afrodescendientes_H_D,Afrodescendientes_H_R,Edad))
colnames(Afrodescendientes_H)<-c("Defunciones_Estimadas_D","Defunciones_Registradas_R","Edad")

# Convertir a formato largo
datos_Afrodescendientes_H <- Afrodescendientes_H %>%
  pivot_longer(cols = -Edad, names_to = "Tipo_Defuncion", values_to = "Cantidad")

# Graficar las curvas
ggplot(datos_Afrodescendientes_H, aes(x = Edad, y = Cantidad, color = Tipo_Defuncion)) +
  geom_line(size = 1) +  # Usar líneas de puntos
  labs(title = "Defunciones Negro_Afrodescendientes hombres: Estimadas vs Registradas",
       x = "Grupos de edad",
       y = "Cantidad de defunciones") +
  theme_minimal() +
  theme(legend.title = element_blank(),  # Ocultar el título de la leyenda
        legend.position = "bottom") +    # Mover la leyenda a la parte inferior
  scale_color_manual(values = c("#6D7980", "#C5C7C6"))  # Colores sobrios y diferenciados

#write.table(datos_Afrodescendientes_H, file = "Personal/Trabajo de Grado - Demografia/Datos/datos_Afrodescendientes_H.csv", sep = ";", dec = ",", row.names = FALSE)

#-------------------------------------#
# Palenqueros
#-------------------------------------#

Palenqueros_H_D<-N_x_Mu_H[,4] # Defunciones estimadas
Palenqueros_H_R<-N_x_Mu_x_pi_H[,4] # Defunciones Registradas
Palenqueros_H<-data.frame(cbind(Palenqueros_H_D,Palenqueros_H_R,Edad))
colnames(Palenqueros_H)<-c("Defunciones_Estimadas_D","Defunciones_Registradas_R","Edad")

# Convertir a formato largo
datos_Palenqueros_H <- Palenqueros_H %>%
  pivot_longer(cols = -Edad, names_to = "Tipo_Defuncion", values_to = "Cantidad")

# Graficar las curvas
ggplot(datos_Palenqueros_H, aes(x = Edad, y = Cantidad, color = Tipo_Defuncion)) +
  geom_line(size = 1) +  # Usar líneas de puntos
  labs(title = "Defunciones Palenqueros hombres: Estimadas vs Registradas",
       x = "Grupos de edad",
       y = "Cantidad de defunciones") +
  theme_minimal() +
  theme(legend.title = element_blank(),  # Ocultar el título de la leyenda
        legend.position = "bottom") +    # Mover la leyenda a la parte inferior
  scale_color_manual(values = c("firebrick", "#FEB7AA"))  # Colores sobrios y diferenciados
#write.table(datos_Palenqueros_H, file = "Personal/Trabajo de Grado - Demografia/Datos/datos_Palenqueros_H.csv", sep = ";", dec = ",", row.names = FALSE)

#-------------------------------------#
# Raizal
#-------------------------------------#

Raizales_H_D<-N_x_Mu_H[,5] # Defunciones estimadas
Raizales_H_R<-N_x_Mu_x_pi_H[,5] # Defunciones Registradas
Raizales_H<-data.frame(cbind(Raizales_H_D,Raizales_H_R,Edad))
colnames(Raizales_H)<-c("Defunciones_Estimadas_D","Defunciones_Registradas_R","Edad")

# Convertir a formato largo
datos_Raizales_H <- Raizales_H %>%
  pivot_longer(cols = -Edad, names_to = "Tipo_Defuncion", values_to = "Cantidad")

# Graficar las curvas
ggplot(datos_Raizales_H, aes(x = Edad, y = Cantidad, color = Tipo_Defuncion)) +
  geom_line(size = 1) +  # Usar líneas de puntos
  labs(title = "Defunciones Raizales hombres: Estimadas vs Registradas",
       x = "Grupos de edad",
       y = "Cantidad de defunciones") +
  theme_minimal() +
  theme(legend.title = element_blank(),  # Ocultar el título de la leyenda
        legend.position = "bottom") +    # Mover la leyenda a la parte inferior
  scale_color_manual(values = c("goldenrod", "#7E5400"))  # Colores sobrios y diferenciados

#write.table(datos_Raizales_H, file = "Personal/Trabajo de Grado - Demografia/Datos/datos_Raizales_H.csv", sep = ";", dec = ",", row.names = FALSE)

#-------------------------------------------------------------------------------------#
# RESULTADOS MUJERES
#-------------------------------------------------------------------------------------#
load("/Users/mmedinar/Documents/Personal/Trabajo de Grado - Demografia/Datos/Corrida 2 - Modelo/Stan-04Oct24-0012-within-Female.RData") # MUJERES

fit_M<-fit
print(fit_M)
print(ML)

# Calcular la devianza
dev_M <- -2 * fit_M@.MISC[["summary"]][["msd"]][56,1]

# Calcular DIC
dic_M <-  -2 * mean(fit_M@.MISC[["summary"]][["msd"]][56,1]) + 2 * length(datos)

# Imprimir resultados
print(dic_M)
#------------------------------------
# SE CONSTRUYE LAS DEFUNCIONES ESTIMADAS PARA EL MODELO INCOMPLETO
# PROBABILIDAD DE REGISTRO <1
#------------------------------------
etnias <- unique(busca_ativa_colombia$IDPERTET)
parametros_M<-data.frame(fit_M@.MISC[["summary"]][["msd"]])

# Crear un subconjunto solo con las filas que contienen "alpha"
alpha_data_M <- subset(parametros_M, grepl("^alpha", rownames(parametros_M)))

# Extraer la columna 'mean' y reorganizar en un formato de matriz con 7 filas y 5 columnas
alpha_means_M <- matrix(alpha_data_M$mean, nrow = 7, ncol = 5, byrow = F)

# Asignar nombres a las filas y columnas

rownames(alpha_means_M) <- paste0("alpha[", 1:7, "]")
colnames(alpha_means_M) <- etnias

# Crear un subconjunto solo con las filas que contienen "pi"
pi_data_M <- subset(parametros_M, grepl("^pi", rownames(parametros_M)))

# Extraer la columna 'mean' y reorganizar en un formato de matriz con 7 filas y 5 columnas
pi_means_M <- matrix(pi_data_M$mean, nrow = 4, ncol = 5, byrow = T)

rownames(pi_means_M) <- c(paste0("pi[", 1:3, "]"), "piall")
colnames(pi_means_M) <- etnias

# B_x_alpha
B_por_alpha_M<-B%*%(alpha_means_M)
dim(B_por_alpha_M)

#----------------------------------
# ESTIMACION LAMBDA PARA MUJERES

LAMBDA_aster_M<-BRstd_COL[,2]
length(LAMBDA_aster_M)

# TASAS especificas de mortalidad para MUJERES x Etnia
mu_M<-exp(LAMBDA_aster_M+B_por_alpha_M)

# Defunciones estimadas para MUJERES x Etnia
tmp = datos %>% filter(SEXO == "Mujer")

tmp = tmp %>%
  group_by(IDPERTET, Grupo_de_Edad) %>%
  summarize(D = sum(defunciones), N=sum(pop_2018))

N = matrix(tmp$N, nrow=21)
colnames(N) <- etnias
N_Mujeres = N

D = matrix(tmp$D, nrow=21)
colnames(D) <- etnias
D_Mujeres = D

N_x_Mu_M<-(mu_M)*N # Defunciones estimadas

# Defunciones con coberturas estimadas REGISTRADAS para MUJERES x Etnia
N_x_Mu_x_pi_M<-N_x_Mu_M*pi_means_M[4,]

#------------------------------------
# SE CONSTRUYE LAS DEFUNCIONES ESTIMADAS PARA EL MODELO COMPLETO
# PROBABILIDAD DE REGISTRO PI=1
#------------------------------------
etnias <- unique(busca_ativa_colombia$IDPERTET)
parametros_COMPLETO_M<-data.frame(ML[["par"]])

# Extraer la columna 'mean' y reorganizar en un formato de matriz con 7 filas y 5 columnas
alpha_means_COMPLETO_M <- matrix(parametros_COMPLETO_M[,1], nrow = 7, ncol = 5, byrow = F)

# Asignar nombres a las filas y columnas

rownames(alpha_means_COMPLETO_M) <- paste0("alpha[", 1:7, "]")
colnames(alpha_means_COMPLETO_M) <- etnias

# B_x_alpha
B_por_alpha_COMPLETO_M<-B%*%(alpha_means_COMPLETO_M)
dim(B_por_alpha_COMPLETO_M)

#----------------------------------
# ESTIMACION PARA MUJERES MODELO COMPLETO

LAMBDA_aster_M<-BRstd_COL[,1]
length(LAMBDA_aster_M)

# TASAS especificas de mortalidad para MUJERES x Etnia
mu_M_COMPLETO<-exp(LAMBDA_aster_M+B_por_alpha_COMPLETO_M)

# Defunciones estimadas para MUJERES x Etnia
N_x_Mu_M_COMPLETO<-(mu_M_COMPLETO)*N 

#pi_means_M[-4,1]%*%Mujer

#---------------------- Graficos Mujeres --------------------------------#
Edad<-c(seq(1:21))
log_mu_M<-cbind(data.frame(log(mu_M),Edad))
colnames(log_mu_M) <- c("Gitano","Indigena","Negro_Afrodescendiente","Palenquero","Raizal","Edad")

# Convertir los datos a formato largo (long format)
log_mu_long_M <- log_mu_M %>%
  pivot_longer(cols = -Edad, names_to = "etnia", values_to = "log_mortalidad")

ggplot(log_mu_long_M, aes(x = Edad, y = log_mortalidad, color = etnia)) +
  geom_line(size = 1) +  # Líneas punteadas
  labs(title = "Curva de logaritmo de la tasa de mortalidad por etnia en mujeres",
       x = "Grupos de edad",
       y = "Log-Tasa de mortalidad mujeres") +
  theme_minimal() +
  theme(legend.title = element_blank(),  # Ocultar el título de la leyenda
        legend.position = "bottom") +    # Mover la leyenda a la parte inferior
  scale_color_manual(values = c("steelblue", "#256D3D", "#6D7980", "firebrick", "goldenrod"))  # Colores sobrios y diferenciados

#-----------------------------------------------------#
# Grafico Defunciones estimadas y registradas x Etnia
#-----------------------------------------------------#

#-------------------------------------#
# Gitano(a) o Rrom
#-------------------------------------#

Gitanos_M_D<-N_x_Mu_M[,1] # Defunciones estimadas
Gitanos_M_R<-N_x_Mu_x_pi_M[,1] # Defunciones Registradas
Gitanos_M<-data.frame(cbind(Gitanos_M_D,Gitanos_M_R,Edad))
colnames(Gitanos_M)<-c("Defunciones_Estimadas_D","Defunciones_Registradas_R","Edad")

# Convertir a formato largo
datos_Gitanos_M <- Gitanos_M %>%
  pivot_longer(cols = -Edad, names_to = "Tipo_Defuncion", values_to = "Cantidad")

# Graficar las curvas
ggplot(datos_Gitanos_M, aes(x = Edad, y = Cantidad, color = Tipo_Defuncion)) +
  geom_line(size = 1) +  # Usar líneas de puntos
  labs(title = "Defunciones Gitanos mujeres: Estimadas_D vs Registradas_R",
       x = "Grupos de edad",
       y = "Cantidad de defunciones") +
  theme_minimal() +
  theme(legend.title = element_blank(),  # Ocultar el título de la leyenda
        legend.position = "bottom") +    # Mover la leyenda a la parte inferior
  scale_color_manual(values = c("steelblue", "#ACD4EC"))  # Colores sobrios y diferenciados

#write.table(datos_Gitanos_M, file = "Personal/Trabajo de Grado - Demografia/Datos/datos_Gitanos_M.csv", sep = ";", dec = ",", row.names = FALSE)

#-------------------------------------#
# Indígenas
#-------------------------------------#

Indigenas_M_D<-N_x_Mu_M[,2] # Defunciones estimadas
Indigenas_M_R<-N_x_Mu_x_pi_M[,2] # Defunciones Registradas
Indigenas_M<-data.frame(cbind(Indigenas_M_D,Indigenas_M_R,Edad))
colnames(Indigenas_M)<-c("Defunciones_Estimadas_D","Defunciones_Registradas_R","Edad")

# Convertir a formato largo
datos_Indigenas_M <- Indigenas_M %>%
  pivot_longer(cols = -Edad, names_to = "Tipo_Defuncion", values_to = "Cantidad")

# Graficar las curvas
ggplot(datos_Indigenas_M, aes(x = Edad, y = Cantidad, color = Tipo_Defuncion)) +
  geom_line(size = 1) +  # Usar líneas de puntos
  labs(title = "Defunciones Indígenas mujeres: Estimadas vs Registradas",
       x = "Grupos de edad",
       y = "Cantidad de defunciones") +
  theme_minimal() +
  theme(legend.title = element_blank(),  # Ocultar el título de la leyenda
        legend.position = "bottom") +    # Mover la leyenda a la parte inferior
  scale_color_manual(values = c("#256D3D", "#80C673"))  # Colores sobrios y diferenciados

#write.table(datos_Indigenas_M, file = "Personal/Trabajo de Grado - Demografia/Datos/datos_Indigenas_M.csv", sep = ";", dec = ",", row.names = FALSE)

#-------------------------------------#
# Negro_Afrodescendiente
#-------------------------------------#

Afrodescendientes_M_D<-N_x_Mu_M[,3] # Defunciones estimadas
Afrodescendientes_M_R<-N_x_Mu_x_pi_M[,3] # Defunciones Registradas
Afrodescendientes_M<-data.frame(cbind(Afrodescendientes_M_D,Afrodescendientes_M_R,Edad))
colnames(Afrodescendientes_M)<-c("Defunciones_Estimadas_D","Defunciones_Registradas_R","Edad")

# Convertir a formato largo
datos_Afrodescendientes_M <- Afrodescendientes_M %>%
  pivot_longer(cols = -Edad, names_to = "Tipo_Defuncion", values_to = "Cantidad")

# Graficar las curvas
ggplot(datos_Afrodescendientes_M, aes(x = Edad, y = Cantidad, color = Tipo_Defuncion)) +
  geom_line(size = 1) +  # Usar líneas de puntos
  labs(title = "Defunciones Negro_Afrodescendientes mujeres: Estimadas vs Registradas",
       x = "Grupos de edad",
       y = "Cantidad de defunciones") +
  theme_minimal() +
  theme(legend.title = element_blank(),  # Ocultar el título de la leyenda
        legend.position = "bottom") +    # Mover la leyenda a la parte inferior
  scale_color_manual(values = c("#6D7980", "#C5C7C6"))  # Colores sobrios y diferenciados

#write.table(datos_Afrodescendientes_M, file = "Personal/Trabajo de Grado - Demografia/Datos/datos_Afrodescendientes_M.csv", sep = ";", dec = ",", row.names = FALSE)

#-------------------------------------#
# Palenqueros
#-------------------------------------#

Palenqueros_M_D<-N_x_Mu_M[,4] # Defunciones estimadas
Palenqueros_M_R<-N_x_Mu_x_pi_M[,4] # Defunciones Registradas
Palenqueros_M<-data.frame(cbind(Palenqueros_M_D,Palenqueros_M_R,Edad))
colnames(Palenqueros_M)<-c("Defunciones_Estimadas_D","Defunciones_Registradas_R","Edad")

# Convertir a formato largo
datos_Palenqueros_M <- Palenqueros_M %>%
  pivot_longer(cols = -Edad, names_to = "Tipo_Defuncion", values_to = "Cantidad")

# Graficar las curvas
ggplot(datos_Palenqueros_M, aes(x = Edad, y = Cantidad, color = Tipo_Defuncion)) +
  geom_line(size = 1) +  # Usar líneas de puntos
  labs(title = "Defunciones Palenqueros mujeres: Estimadas vs Registradas",
       x = "Grupos de edad",
       y = "Cantidad de defunciones") +
  theme_minimal() +
  theme(legend.title = element_blank(),  # Ocultar el título de la leyenda
        legend.position = "bottom") +    # Mover la leyenda a la parte inferior
  scale_color_manual(values = c("firebrick", "#FEB7AA"))  # Colores sobrios y diferenciados

#write.table(datos_Palenqueros_M, file = "Personal/Trabajo de Grado - Demografia/Datos/datos_Palenqueros_M.csv", sep = ";", dec = ",", row.names = FALSE)

#-------------------------------------#
# Raizal
#-------------------------------------#

Raizales_M_D<-N_x_Mu_M[,5] # Defunciones estimadas
Raizales_M_R<-N_x_Mu_x_pi_M[,5] # Defunciones Registradas
Raizales_M<-data.frame(cbind(Raizales_M_D,Raizales_M_R,Edad))
colnames(Raizales_M)<-c("Defunciones_Estimadas_D","Defunciones_Registradas_R","Edad")

# Convertir a formato largo
datos_Raizales_M <- Raizales_M %>%
  pivot_longer(cols = -Edad, names_to = "Tipo_Defuncion", values_to = "Cantidad")

# Graficar las curvas
ggplot(datos_Raizales_M, aes(x = Edad, y = Cantidad, color = Tipo_Defuncion)) +
  geom_line(size = 1) +  # Usar líneas de puntos
  labs(title = "Defunciones Raizales mujeres: Estimadas vs Registradas",
       x = "Grupos de edad",
       y = "Cantidad de defunciones") +
  theme_minimal() +
  theme(legend.title = element_blank(),  # Ocultar el título de la leyenda
        legend.position = "bottom") +    # Mover la leyenda a la parte inferior
  scale_color_manual(values = c("goldenrod", "#7E5400"))  # Colores sobrios y diferenciados

#write.table(datos_Raizales_M, file = "Personal/Trabajo de Grado - Demografia/Datos/datos_Raizales_M.csv", sep = ";", dec = ",", row.names = FALSE)

