# ########################################################################### #
# ##                                                                       ## #
####                          FLYBALLOON EXPERIMENT                        ####
# ##                                                                       ## #
# ########################################################################### #

# Aluno: Lucas Pereira de Almeida
# Disciplinas: Dinamica da Atmosfera I 

                              #### EXERCICIO ####

# Um balao piloto inflado com gas 'fly balloon' e lancado na superficie e sobe 
# a uma velocidade constante de 150 m/min, previamente calibrada.
# O balao vai sendo levado pelo vento.
# Com um teodolito aerologico registra-se a posicao do balao a cada T seg,
# atraves dos angulos de azimute com relacao ao norte e elevacao com relacao
# a horizontal.

# As planilhas excel mostram as medidas feitas durante o experimento CIRSAN/
# LBA as margens do rio Tapajos.

# Determine:

# a. A altura do balao:
# b. As coordenadas x e y com relacao ao ponto de lancamento:
# c. As componentes u e v do vento em cada camada:
# d. A velocidade e direcao do vento na convencao meteorologica:
# e. A direcao do vento em pontos cardeais aproximados:
# f. Desenhe a curva hodografa do vento, ou seja o grafico de (u,v) indicando 
#    em cada ponto da curva qual a altitude do balao.

                            #### PROCEDIMENTOS ####

# Instalando pacotes necessários
library(tidyverse)
library(ggplot2)
library(readxl)
install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)
install.packages("knitr")
library(knitr)
library(tinytex)
library(openxlsx)

# Importando planilha Excel
Pindobal <- read_excel("Pindobal.xlsx", 
                       col_types = c("text", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric"))
Pindobal <- as.data.frame(Pindobal)

# Extraindo dados para o dia escolhido (19/07)
Pindobal_d1907 <- subset(Pindobal, day == 19)

Pindobal_d1907$day <- factor(Pindobal_d1907$day)
Pindobal_d1907$month <- factor(Pindobal_d1907$month)
Pindobal_d1907$location <- factor(Pindobal_d1907$location)

str(Pindobal_d1907)
View(Pindobal_d1907)

# Calculando a velocidade de subida do balao em m/s
Pindobal_d1907 <- Pindobal_d1907 %>%
  mutate(vel_mseg = vel_mmin / 60)

#### (a) ####
# Calculando a altura do balao em m
Pindobal_d1907 <- Pindobal_d1907 %>%
  mutate(h_m = vel_mseg * time)

#### (b) ####
# Calculando as coordenadas x e y em relacao ao ponto de lancamento
# E necessario converter os angulos de graus para radianos com a funcao
# NISTdegTOradian
Pindobal_d1907 <- Pindobal_d1907 %>%
  mutate(desloc_m = h_m / tan(NISTdegTOradian(elev_deg)))

for (row in 1:nrow(Pindobal_d1907)) {
  if (Pindobal_d1907[row,"desloc_m"] == "NaN") {
    Pindobal_d1907[row,"desloc_m"] = 0
  }
}

Pindobal_d1907 <- Pindobal_d1907 %>%
  mutate(x = 0,y = 0)
  
for (row in 1:nrow(Pindobal_d1907)) {
  if (Pindobal_d1907[row,"azim_deg"] >= 0 &
      Pindobal_d1907[row,"azim_deg"] < 90) {
    Pindobal_d1907[row,"x"] = 
      cos(NISTdegTOradian(90 - Pindobal_d1907[row,"azim_deg"])) * 
      Pindobal_d1907[row,"desloc_m"]
    Pindobal_d1907[row,"y"] = 
      sin(NISTdegTOradian(90 - Pindobal_d1907[row,"azim_deg"])) * 
      Pindobal_d1907[row,"desloc_m"]
    }
  
  if (Pindobal_d1907[row,"azim_deg"] >= 90 &
      Pindobal_d1907[row,"azim_deg"] < 180) {
    Pindobal_d1907[row,"x"] = 
      sin(NISTdegTOradian(180 - Pindobal_d1907[row,"azim_deg"])) * 
      Pindobal_d1907[row,"desloc_m"]
    Pindobal_d1907[row,"y"] = 
      cos(NISTdegTOradian(180 - Pindobal_d1907[row,"azim_deg"])) * 
      Pindobal_d1907[row,"desloc_m"] * -1
  }
  
  if (Pindobal_d1907[row,"azim_deg"] >= 180 &
      Pindobal_d1907[row,"azim_deg"] < 270) {
    Pindobal_d1907[row,"x"] = 
      cos(NISTdegTOradian(270 - Pindobal_d1907[row,"azim_deg"])) * 
      Pindobal_d1907[row,"desloc_m"] * -1
    Pindobal_d1907[row,"y"] = 
      sin(NISTdegTOradian(270 - Pindobal_d1907[row,"azim_deg"])) * 
      Pindobal_d1907[row,"desloc_m"] * -1
  }
  
  if (Pindobal_d1907[row,"azim_deg"] >= 270 &
      Pindobal_d1907[row,"azim_deg"] <= 360) {
    Pindobal_d1907[row,"x"] = 
      sin(NISTdegTOradian(360 - Pindobal_d1907[row,"azim_deg"])) * 
      Pindobal_d1907[row,"desloc_m"] * -1
    Pindobal_d1907[row,"y"] = 
      cos(NISTdegTOradian(360 - Pindobal_d1907[row,"azim_deg"])) * 
      Pindobal_d1907[row,"desloc_m"]
  }
}

#### (c) ####
# Calculando as componentes u e v do vento em cada camada
Pindobal_d1907 <- Pindobal_d1907 %>%
  mutate(u = 0,v = 0)

for (row in 1:nrow(Pindobal_d1907)) {
  if (Pindobal_d1907[row,"desloc_m"] != 0.0){
    x0 = Pindobal_d1907[row-1,"x"]
    y0 = Pindobal_d1907[row-1,"y"]
    t0 = Pindobal_d1907[row-1,"time"]
    Pindobal_d1907[row,"u"] = (Pindobal_d1907[row,"x"] - x0)/
      (Pindobal_d1907[row,'time']-t0)
    Pindobal_d1907[row,"v"] = (Pindobal_d1907[row,"y"] - y0)/
      (Pindobal_d1907[row,"time"]-t0)
  }
}

#### (d) ####
# Calculando a velocidade e a direcao do vendo na convencao meteorologica
Pindobal_d1907 <- Pindobal_d1907 %>%
  mutate(vel_vento_mseg = 0,dir_vento = 0)

for (row in 1:nrow(Pindobal_d1907)) {
  Pindobal_d1907[row,"vel_vento_mseg"] = sqrt((Pindobal_d1907[row,"u"])^2 +
                                                (Pindobal_d1907[row,"v"])^2)
  if (Pindobal_d1907[row,"azim_deg"] == 0) {
    Pindobal_d1907[row,"dir_vento"] = 0
  }
  
  if (Pindobal_d1907[row,"azim_deg"] > 0 &
      Pindobal_d1907[row,"azim_deg"] < 180) {
    Pindobal_d1907[row,"dir_vento"] = Pindobal_d1907[row,"azim_deg"] + 180
  }
  
  if (Pindobal_d1907[row,"azim_deg"] >= 180 &
      Pindobal_d1907[row,"azim_deg"] <= 360) {
    Pindobal_d1907[row,"dir_vento"] = Pindobal_d1907[row,"azim_deg"] - 180
  }
  
}

#### (e) ####
# Demonstrando a direcao do vento em pontos cardiais aproximados
Pindobal_d1907 <- Pindobal_d1907 %>%
  mutate(dir_ptcar = 0)

for (row in 1:nrow(Pindobal_d1907)) {
  if (Pindobal_d1907[row,"dir_vento"] > 0 &
      Pindobal_d1907[row,"dir_vento"] < 45) {
    Pindobal_d1907[row,"dir_ptcar"] = "NNE"
  }
  
  if (Pindobal_d1907[row,"dir_vento"] == 45) {
    Pindobal_d1907[row,"dir_ptcar"] = "NE"
  }
  
  if (Pindobal_d1907[row,"dir_vento"] > 45 &
      Pindobal_d1907[row,"dir_vento"] < 90) {
    Pindobal_d1907[row,"dir_ptcar"] = "ENE"
  }
  
  if (Pindobal_d1907[row,"dir_vento"] == 90) {
    Pindobal_d1907[row,"dir_ptcar"] = "E"
  }
  
  if (Pindobal_d1907[row,"dir_vento"] > 90 &
      Pindobal_d1907[row,"dir_vento"] < 135) {
    Pindobal_d1907[row,"dir_ptcar"] = "ESE"
  }
  
  if (Pindobal_d1907[row,"dir_vento"] == 135) {
    Pindobal_d1907[row,"dir_ptcar"] = "SE"
  }
  
  if (Pindobal_d1907[row,"dir_vento"] > 135 &
      Pindobal_d1907[row,"dir_vento"] < 180) {
    Pindobal_d1907[row,"dir_ptcar"] = "SSE"
  }
  
  if (Pindobal_d1907[row,"dir_vento"] == 180) {
    Pindobal_d1907[row,"dir_ptcar"] = "S"
  }
  
  if (Pindobal_d1907[row,"dir_vento"] > 180 &
      Pindobal_d1907[row,"dir_vento"] < 225) {
    Pindobal_d1907[row,"dir_ptcar"] = "SSO"
  }
  
  if (Pindobal_d1907[row,"dir_vento"] == 225) {
    Pindobal_d1907[row,"dir_ptcar"] = "SO"
  }
  
  if (Pindobal_d1907[row,"dir_vento"] > 225 &
      Pindobal_d1907[row,"dir_vento"] < 270) {
    Pindobal_d1907[row,"dir_ptcar"] = "OSO"
  }
  
  if (Pindobal_d1907[row,"dir_vento"] == 270) {
    Pindobal_d1907[row,"dir_ptcar"] = "O"
  }
  
  if (Pindobal_d1907[row,"dir_vento"] > 270 &
      Pindobal_d1907[row,"dir_vento"] < 315) {
    Pindobal_d1907[row,"dir_ptcar"] = "ONO"
  }
  
  if (Pindobal_d1907[row,"dir_vento"] == 315) {
    Pindobal_d1907[row,"dir_ptcar"] = "NO"
  }
  
  if (Pindobal_d1907[row,"dir_vento"] > 315 &
      Pindobal_d1907[row,"dir_vento"] < 360) {
    Pindobal_d1907[row,"dir_ptcar"] = "NNO"
  }
  
  if (Pindobal_d1907[row,"dir_vento"] == 360) {
    Pindobal_d1907[row,"dir_ptcar"] = "N"
  }
}

for (row in 1:nrow(Pindobal_d1907)) {
  if (Pindobal_d1907[row,"dir_vento"] == 0) {
    Pindobal_d1907[row,"dir_ptcar"] = Pindobal_d1907[row+1,"dir_ptcar"]
  }
}

View(Pindobal_d1907)

write.xlsx(Pindobal_d1907, 'flyballoon_pindobal.xlsx')

#### (f) ####
# Desenhando a curva hodografa do vento, grafico de x e y indicando em cada 
# ponto da curva qual a altitude do balao
Pindobal_perfil1 <- Pindobal_d1907[1:40,]
Pindobal_perfil2 <- Pindobal_d1907[41:80,]
Pindobal_perfil3 <- Pindobal_d1907[81:115,]

View(Pindobal_perfil1)
View(Pindobal_perfil2)
View(Pindobal_perfil3)

perfil1 <- ggplot(data = Pindobal_perfil1,aes(x = x, y = y)) +
  geom_point(aes(color = dir_ptcar)) +
  ggtitle("Hodrografa do Vento - Pindobal (19/07/2001) - Perfil 1") +
  labs(color='Pontos Cardiais')+
  xlab("x (m)") +
  ylab("y (m)") +
  geom_text(aes(label = h_m),hjust=0, vjust=-0.5,size=3,
            check_overlap = TRUE) +
  theme_bw()
  
perfil2 <- ggplot(data = Pindobal_perfil2,aes(x = x, y = y)) +
  geom_point(aes(color = dir_ptcar)) +
  ggtitle("Hodrografa do Vento - Pindobal (19/07/2001) - Perfil 2") +
  labs(color='Pontos Cardiais')+
  xlab("x (m)") +
  ylab("y (m)") +
  geom_text(aes(label = h_m),hjust=0, vjust=-0.5,size=3,
            check_overlap = TRUE) +
  theme_bw()

perfil3 <- ggplot(data = Pindobal_perfil3,aes(x = x, y = y)) +
  geom_point(aes(color = dir_ptcar)) +
  ggtitle("Hodrografa do Vento - Pindobal (19/07/2001) - Perfil 3") +
  labs(color='Pontos Cardiais')+
  xlab("x (m)") +
  ylab("y (m)") +
  geom_text(aes(label = h_m),hjust=0, vjust=-0.5,size=3,
            check_overlap = TRUE) +
  theme_bw()

perfis_pindobal <- ggplot(data = Pindobal_d1907,aes(x = x, y = y)) +
  geom_point(aes(color = dir_ptcar)) +
  ggtitle("Curva Hodrografa do Vento - Pindobal (19/07/2001)") +
  labs(color='Pontos Cardiais')+
  xlab("x (m)") +
  ylab("y (m)") +
  geom_text(aes(label = h_m),hjust=0, vjust=-0.5,size=3,
            check_overlap = TRUE) +
  theme_bw()

plot(perfis_pindobal)

# Desenhando a curva hodografa do vento, grafico de u e v indicando em cada 
# ponto da curva qual a altitude do balao
vel_pindobal <- ggplot(data = Pindobal_d1907,aes(x = u, y = v)) +
  geom_point(aes(color = dir_ptcar)) +
  ggtitle("Curva Hodrografa do Vento - Pindobal (19/07/2001)") +
  labs(color='Pontos Cardiais')+
  xlab("u (m/s)") +
  ylab("v (m/s)") +
  geom_text(aes(label = h_m),hjust=0, vjust=-0.5,size=3,
            check_overlap = TRUE) +
  theme_bw()

plot(vel_pindobal)

perfil1 <- ggplot(data = Pindobal_perfil1,aes(x = u, y = v)) +
  geom_point(aes(x = u, y = v)) +
  geom_point(aes(color = dir_ptcar)) +
  geom_path() +
  ggtitle("Hodografa do Vento - Pindobal (19/07/2001) - Perfil 1") +
  labs(color='Pontos Cardiais')+
  xlab("u (m/s)") +
  ylab("v (m/s)") +
  geom_text(aes(label = h_m),hjust=0, vjust=-0.5,size=3,
            check_overlap = TRUE) +
  theme_bw()

plot(perfil1)

perfil2 <- ggplot(data = Pindobal_perfil2,aes(x = u, y = v)) +
  geom_point(aes(x = u, y = v)) +
  geom_point(aes(color = dir_ptcar)) +
  geom_path() +
  ggtitle("Hodografa do Vento - Pindobal (19/07/2001) - Perfil 2") +
  labs(color='Pontos Cardiais')+
  xlab("u (m/s)") +
  ylab("v (m/s)") +
  geom_text(aes(label = h_m),hjust=0, vjust=-0.5,size=3,
            check_overlap = TRUE) +
  theme_bw()

plot(perfil2)

perfil3 <- ggplot(data = Pindobal_perfil3,aes(x = u, y = v)) +
  geom_point(aes(x = u, y = v)) +
  geom_point(aes(color = dir_ptcar)) +
  geom_path() +
  ggtitle("Hodografa do Vento - Pindobal (19/07/2001) - Perfil 3") +
  labs(color='Pontos Cardiais')+
  xlab("u (m/s)") +
  ylab("v (m/s)") +
  geom_text(aes(label = h_m),hjust=0, vjust=-0.5,size=3,
            check_overlap = TRUE) +
  theme_bw()

plot(perfil3)
