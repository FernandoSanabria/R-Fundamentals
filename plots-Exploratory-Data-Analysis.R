#plot es para graficas de distribución
plot(mtcars$mpg ~ mtcars$cyl,
     xlab="cyl",ylab = "mpg",
     main = "Relación cilindros y millas por galón")

plot(mtcars$mpg ~ mtcars$hp,
     xlab="hp",ylab = "mpg",
     main = "Relación caballos de fuerza y millas por galón")

plot(orangeec$Unemployment ~ orangeec$Education.invest...GDP,
     xlab="educationInvestment",ylab = "Unemployment",
     main = "Relación inversión en educación  y desempleo")

plot(orangeec$GDP.PC ~ orangeec$Creat.Ind...GDP,
     xlab="CreativeIndustryGDP",ylab = "GDP.PC",
     main = "Relación CreativeIndustryGDP  y GDP.PC")

#histograms
qplot(mtcars$hp,
      geom = "histogram",
      xlab = "Caballos de fuerza",
      main = "Distribución de carros según caballos de fuerza")

myGraphPlotly2= qplot(orangeec$GDP.PC,label = row.names(orangeec),
      geom = "histogram",
      ylab = "Caballos de fuerza",
      main = "Distribución de GDP.PC")

myGraphPlotly2l = ggplotly(myGraphPlotly2)



ggplot(mtcars, aes(x=hp))+
      geom_histogram()+
      labs(x="Caballos de fuerza", 
           y="Número de carros",
           title = "Distribución de carros según caballos de fuerza")  

ggplot(mtcars, aes(x=hp))+
  geom_histogram()+
  labs(x="Caballos de fuerza", 
       y="Número de carros",
       title = "Distribución de carros según caballos de fuerza")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


ggplot(mtcars, aes(x=hp))+
  geom_histogram(binwidth = 10)+
  labs(x="Caballos de fuerza", 
       y="Número de carros",
       title = "Distribución de carros según caballos de fuerza")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



ggplot() + geom_histogram(data = mtcars,
                          aes(x=hp),fill="green",color="blue",
                          binwidth=20)+
  labs(x="Caballos de fuerza", 
       y="Número de carros",
       title = "Distribución de carros según caballos de fuerza")+
  xlim(c(80,280))+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
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
  
  
  
  ggplot()+geom_histogram(data = orangeec,
                          aes(x=Creat.Ind...GDP),fill="green",color="blue",
                          binwidth=1)+
    labs(x="%", 
         y="NUmero de paises",
         title = "%  GDP Industry Creat.")+
    theme(legend.position = "none")+
    theme(panel.background = element_blank(),panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  ggplot()+geom_histogram(data = orangeec,
                          aes(x=Internet.penetration...population),fill="green",color="blue",
                          binwidth=5)+
    labs(x="Porcentaje de la población", 
         y="NUmero de paise",
         title = "Penetración de INternet")+

    theme(legend.position = "none")+
    theme(panel.background = element_blank(),panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  
  
  ggplot()+geom_histogram(data = orangeec,
                          aes(x=Internet.penetration...population),fill="green",color="blue",
                          binwidth=5)+
    labs(x="Porcentaje de la población", 
         y="NUmero de paise",
         title = "Penetración de INternet")+
    scale_x_continuous(name = "Porcentaje de la población",breaks = seq(40,100,5))+
  
    
    theme(legend.position = "none")+
    theme(panel.background = element_blank(),panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  
  
  # box plot
  
  boxplot(mtcars$hp,
          ylab="caballos de  fuerza",
          main = "hp en carros de mtcars")
  
  ggplot(mtcars,aes(x=as.factor(cyl),y=hp, fill=cyl))+
    geom_boxplot(alpha=0.6)+
    labs(x="cilindros", y="caballos de fuerza",
         title = "Caballlos de fuerza segun cilindros")+
    theme(legend.position = "none")+
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  ggplot(mtcars,aes(x = am, y = mpg, fill= am))+
    geom_boxplot()+
    labs(x="Tipo de transmisión", y = "millas por galon",
         title = "mpg según transmisión")+
    theme(legend.position = "none")+
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  #
  mtcars$am <- factor(mtcars$am, levels = c(TRUE,FALSE),
                      labels = c("Manual", "Automático"))
  
  #
  
  economy <- mean(orangeec$GDP.PC)
  
  
  #Add column to dataset that fulfilled requirement
  orangeec <- orangeec %>% 
                    mutate(descriptiveGDP.PC =ifelse(GDP.PC < economy,
                                                     "Por debajo del promedio",
                                                     "Por encima del promedio"))
  
  
#
ggplot(orangeec, aes( x= descriptiveGDP.PC, y = Creat.Ind...GDP,
                     fill= descriptiveGDP.PC ))+
  geom_boxplot(alpha=0.4)+
  labs(x="Tipo de país", y="% Total GDP Orange Industry",
       title = "Contribución de la economia naranja según el PIB per cap.")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  


#
ggplot(orangeec, aes( x= descriptiveGDP.PC, y = Internet.penetration...population,
                      fill= descriptiveGDP.PC ))+
  geom_boxplot(alpha=0.4)+
  labs(x="Tipo de país", y="% de población cubierta con internet",
       title = "Contribución de la economia naranja según el PIB per cap.")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


#   dispersion graphics de mas de dos variables


ggplot(mtcars, aes(mpg,hp))+
  geom_point()+
  labs( x= "horse power", y= "miles per gallon",
        title = "Horse power and mpg relation")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  

ggplot(mtcars, aes(wt,hp))+
  geom_point()+
  labs( x= "weight", y= "hp",
        title = "weight car and horse power relation")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  
  
  
# con 4 variables
ggplot(mtcars, aes(hp,qsec))+
  geom_point(aes(color=am, size= cyl))+
  labs( x= "horse power", y = "time 1/4 mile",
        title = "Caballos de fuerza segun cilindraje y tipo de transmisión")
  
  
  
ggplot(orangeec, aes(Internet.penetration...population,Creat.Ind...GDP))+
  geom_point(aes(color=factor(descriptiveGDP.PC), size= GDP.Growth..))+
  labs( x= "Internet penetration", y = "% total GDP orange economy",
        title = "% GDP orange economy and GDP growth accoirding to % internet penetration and ")


ggplot(orangeec, aes(Education.invest...GDP,Unemployment))+
  geom_point(aes(color=factor(descriptiveGDP.PC), size= X..pop.below.poverty.line))+
  labs( x= "% GDP education investment", y = "% Unemployment",
        title = "% Unemployment according to education invesment")



myGraph <- ggplot(orangeec, aes(Internet.penetration...population,
                     Creat.Ind...GDP, label = row.names(orangeec)))+
  geom_point()+
  labs( x= "INternet penetration", y = "% Total GDG orange economy",
        title = "% de pobación con unternet vs % total GDP orange economy")
  
myGraphPlotly = ggplotly(myGraph)

  
  
  
  
  
  
