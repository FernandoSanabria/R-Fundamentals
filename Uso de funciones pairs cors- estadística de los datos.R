pairs(mtcars)

pairs(mtcars[,2:4])

#

newData <- subset(mtcars, select = c(2,7:8,11,12))
pairs(newData)


pairs(mtcars[,-c(1,3,5,5,7,11,13)])


# filtering

eficientes <- filter(mtcars, mpg>=30)

#

pairs(eficientes)
pairs(eficientes[,2:6])

#

merc <- mtcars %>% 
  filter(str_detect(model,"Merc"))

pairs(merc)
pairs(merc[,2:6])


#confirm correlations
pairs(mtcars[,2:6])
cor(mtcars[,2:6])

pairs(newData)
cor(newData)

pairs(merc[,2:6])
cor(merc[,2:6])


#

pairs(orangeec[,2:6])
cor(orangeec[,2:6])

# without N/A
cor(orangeec[,2:6],use = "complete.obs")



pairs(orangeec[,5:10])
cor(orangeec[,5:10],use = "complete.obs")


newOrange <- subset(orangeec,select = c(2,11,10))
pairs(newOrange)
cor(newOrange,use = "complete.obs")


summary(mtcars)

sd(mtcars$mpg)

 desv = sd(mtcars$mpg)

mean(mtcars$mpg)


prom = mean(mtcars$mpg)



coefVar = desv/prom*100


desv = sd(orangeec$Internet.penetration...population)

prom = mean(orangeec$Internet.penetration...population)

coefVar = desv/prom*100

summary(orangeec)

#
mean(orangeec$Creat.Ind...GDP)

mean(orangeec$Creat.Ind...GDP, na.rm=TRUE)

prom = mean(orangeec$Creat.Ind...GDP, na.rm=TRUE)

desv = sd(orangeec$Creat.Ind...GDP, na.rm=TRUE)

coefVar = (desv/prom)*100

#Ajustando data to improve visualization


prom = mean(mtcars$mpg)

mtcars = mtcars %>%
  mutate(eficienciaPromGrupal=ifelse(mpg<prom,
                     "bajo prom",
                        ifelse(mpg==prom,
                         'en prom',
                               "sobre prom")))


mtcars = mtcars %>% 
  mutate(rapidezRelativa = ifelse(qsec < 16,
                                  "1/4 de milla en menos de 16 segs",
                                  "1/4 de milla en mas de 16 segs"))

mtcars = mtcars %>% 
  mutate(pesoKilos=(wt/2)*1000)


mtcars = mtcars %>% 
  mutate(tipoPeso= ifelse(pesoKilos <= 1500,
                          "liviano",
                          "pesado"))


orangeec = orangeec %>% 
  mutate(crecimientoGDP = ifelse(GDP.Growth..>=2.5,
                                 'Superior a 2.5%',
                                 'Inferior a 2.5%'))


orangeec = orangeec %>% 
  mutate(CreatividadReferencial = ifelse(Creat.Ind...GDP>5,
                                 'Mas creativo',
                                 'Menos creativo'))

orangeec = orangeec %>% 
  mutate(Cumple2 = ifelse(Creat.Ind...GDP>5 & Unemployment<5,
                                         'Cumple',
                                         'No cumple'))




orangeec %>% 
  arrange(desc(Creat.Ind...GDP))


TopCreativos = orangeec %>%
  filter(Country %in% c("Brazil","Argentina","Panama","Mexico","Colombia","Paraguay"))


TopCreativos %>% 
  arrange(desc(Creat.Ind...GDP))

#

mtcars %>% 
  arrange(desc(pesoKilos))



masHeavies = mtcars %>%
  filter(model %in% c("Lincoln Continental","Chrysler Imperial","Cadillac Fleetwood",
                    "Merc 450SE"))

ggplot(masHeavies, aes(x=hp, y = mpg))+
  geom_point()+
  facet_wrap(~model)


ggplot(mtcars, aes(x=cyl, y=mpg, size= pesoKilos))+
  geom_point()+
    facet_wrap(~ am)


ggplot(TopCreativos, aes(x=Internet.penetration...population,
                         y=Services...GDP, size = GDP.PC))+
  geom_point()+
  facet_wrap(~GDP.Growth..)



ggplot(TopCreativos, aes(x=Education.invest...GDP,
                         y=Creat.Ind...GDP, size = Unemployment))+
  geom_point()+
  facet_wrap(~Country)


myColors = brewer.pal(9,"Reds")


ggplot(TopCreativos, aes(x=Internet.penetration...population,
                          y = GDP.PC, fill=Creat.Ind...GDP))+
  geom_tile()+
  facet_wrap(~Country)
  scale_fill_gradientn(colors=myColors)



plot(orangeec$GDP.Growth.. ~ orangeec$Creat.Ind...GDP,
     xlab="Creat.Ind...GDP",ylab = "GDP.Growth..",
     main = "Relación inversión en educación  y desempleo")






plot(mtcars$qsec ~ mtcars$hp,
     xlab="hp",ylab = "qsec",
     main = "Relación qsec  y hp")

ggplot(orangeec, aes(x=Creat.Ind...GDP))+
  geom_histogram(binwidth = 0.5)+
  labs(x="d", 
       y="d",
       title = "Distribución de carros según cilindros")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

glimpse(mtcars)
str(mtcars)
ggplot(orangeec, aes( x= descriptiveGDP.PC, y = Internet.penetration...population,
                      fill= descriptiveGDP.PC ))+
  geom_boxplot(alpha=0.4)+
  labs(x="Tipo de país", y="% penetration",
       title = "Contribución de la economia naranja según el PIB per cap.")
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



ggplot()+geom_histogram(data = orangeec,
                        aes(x=GDP.PC),fill="green",color="blue",
                        binwidth=2000)+
  labs(x="pib per capita", 
       y="Dolares",
       title = "GDP.PC Latam")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())




ggplot(orangeec, aes(Education.invest...GDP,Creat.Ind...GDP))+
  geom_point(aes(color=factor(descriptiveGDP.PC)))+
  labs( x= "Education.invest...GDP", y = "% total GDP orange economy",
        title = "% GDP orange economy andeducation invest ")


################################################################################################
ggplot(orangeec, aes(Creat.Ind...GDP,GDP.Growth..))+
  geom_point(aes(color=factor(descriptiveGDP.PC)))+
  labs( x= "Creat.Ind...GDP", y = "% total GDP orange economy",
        title = "% GDP orange economy andeducation invest ")



plot(orangeec$Creat.Ind...GDP ~ orangeec$Education.invest...GDP,
     xlab="Education.invest...GDP",ylab = "Creat.Ind...GDP",
     main = "Relación inversión Creat.Ind...GDP  y Education.invest...GDP")+
  geom_point()+
  facet_wrap(~GDP.Growth)

newOrange <- subset(mtcars,select = c(3,5))

newOrange <- subset(orangeec,select = c(2,5))

pairs(newOrange)
cor(newOrange, use = "complete.obs")

ggplot(orangeec, aes(x=Internet.penetration...population,
                         y=Services...GDP, size = GDP.PC))+
  geom_point()+
  facet_wrap(~CreatividadReferencial)





