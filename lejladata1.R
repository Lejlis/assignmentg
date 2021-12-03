
#Pricipal component analysis - Lejla Fazlic´


library(MVA)
library(HSAUR2)
library(lattice)
library(tidyverse)
library(dplyr)
library(gridExtra)
library(psych)
library(reshape2)
library(ggplot2)
library(gsheet)
library(magrittr)

data(PAQ_Lejla)
View(PAQ_Lejla)
head(PAQ_Lejla)
summary(PAQ_Lejla)





PAQ_Lejla %>%
  ggplot() +
  aes(x = var,
      y = value) +
  geom_point()

#Let's make a plot to see what is going on!


PAQ_Lejla %>%
  select(-var) %>%
  describe()




PAQ_Lejla %>%
  select(var, value) %>%
  summary()

PAQ_Lejla %>% 
  select(-var) %>% 
  describe()

PAQ_Lejla <- PAQ_Lejla%>%
  mutate(var = factor(var))


#We have a factor and it needs to become a numerical so let us try to convert it to become a numerical

 levels(PAQ_Lejla$var)

 as.numeric(var)   
 
 PAQ_Lejla <- PAQ_Lejla %>% drop_na()
 
 PAQ_Lejla <- PAQ_Lejla %>%
 
     filter(!is.na(var))
 na.omit(PAQ_Lejla) 
 
 
 table(PAQ_Lejla$var)
 
 
 PAQ_Lejla$var

 
 PAQ_Lejla %>%
   select(var, value) %>%
   summary()
         

 
 data <- PAQ_Lejla
 
 data
 
 
 row.names(PAQ_Lejla)

 
 
 class(PAQ_Lejla[,"var"])
 
 #We need to create a vector for our row variables
 
 # Data Vector 'V'
 var = c("xage", "xsex", "xcry", "xhelp", "xbreathe", "xfreeze", "xalien", "xinferior", "xweep", "Support", "XNerd")
 
 # Convert vector 'V' into a factor
 Var <- factor(var)
 
 # Converting a factor into a numeric vector 
 as.numeric(var)
 
 class(PAQ_Lejla[Rows, "var", "value"])
 
 
 
 PAQ_Lejla <- PAQ_Lejla %>% drop_na()
 
 PAQ_Lejla <- PAQ_Lejla %>%
   filter(!is.na(var))
 
 
 #______
 
 PAQ_Lejla.pca <- prcomp(PAQ_Lejla, scale=TRUE)
 
 
 Rows %>%
   mean() %>%
   log() %>%
   round(digits = 1)
 
 
 
 
#We have to create vectors for the different rownames. But the rows are factors so we need to convert factors into numericals otherwise we can not run the analysis. :(
 



PAQ_Lejla

 xsex = c(1:300)
xsex

xage = c(301:600)
xage

xCry = c(601:900)
xCry      

xhelp = c(901:1200)
xhelp

xbreathe = c(1201:1500)
xbreathe

xfreeze = c(1501:1800)
xfreeze

xalien = (1801:2100)
xalien

xinferior = c(2101:2400)
xinferior

xweep = c(2401:2700)
xweep

xSupport = c(2701:3000)
xSupport

xNerd = c(3001:3300)
xNerd


#___________________

mean(xage)
mean(xsex)
mean(xcry)
mean(xhelp)
mean(xbreathe)
mean(xfreeze)
mean(xalien)
mean(xinferior)
mean(xweep)
mean(xSupport)
mean(xNerd)




head(PAQ_Lejla)
drop.na(PAQ_Lejla)



library(reshape)

library(dplyr)
library(tibble)
library(tidyselect)
PAQ_Lejla <- PAQ_Lejla %>% drop_na()

is.na(PAQ_Lejla)
na.omit(PAQ_Lejla)
 na.omit(PAQ_Lejla)   

 

 PAQ_Lejla %>%
   ggplot() +
   aes(x = var,
       y = value) +
   geom_point()
 
 PAQ_Lejla <- na.omit(PAQ_Lejla)
 Dataset = PAQ_Lejla

 str(PAQ_Lejla) 

 
 #Here is the analysis
 
 PCA.model <- princomp(~ var + value + id + xage + xhelp + xfreeze + 
                         xalien + xinferior + xweep + xSupport + xNerd, cor = TRUE, 
                       data = PAQ_Lejla)
 
 summary(PCA.model)

 
 #We can see below in the plot that comp 1 makes up for almost all the variance, approximitley 80%
 
 screeplot(PCA.model, npcs = 7)

  
 names(PCA.model)

 PCA.model$sdev^2

 print(unclass(loadings(PCA.model)))
 
 plot.CA(loadings(PCA.model))
 
summary(PCA.model)

names(PCA.model)

# We need to get our eigenvalues! Eigenvectors


eigenvectors <- PCA.model$loadings
eigenvalues <- PCA.model$sdev *PCA.model$dev

cor(PCA.model$scores)

round(cor(PCA.model$scores), 3)  #round up our digits to 3 digits!


#_______________________________________________________________

print(unclass(loadings(PCA.model)))

library(factoextra)
library(FactoMineR)

df <- subset(Dataset, select = -c(var + id + xage + xhelp + xfreeze + 
                                    xalien + xinferior + xweep + xSupport + xNerd))
PCA.model <- PCA(df, graph = FALSE)
get_eig(PCA.model)


fviz_screeplot(PCA.model, addlabels = TRUE, ylim = c(0, 50))



p6 <- fviz_pca_ind(PCA.model)
p7 <- fviz_pca_biplot(PCA.model)
grid.arrange(p6, p7, nrow = 1)



fviz_pca_var(PCA.model)


# Contributions of variables to PC1
fviz_contrib(PCA.model, choice = "var", axes = 3)


fviz_contrib(PCA.model, choice = "var", axes = 2)

fviz_pca_var(PCA.model, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", 
                                                                "#FC4E07"), repel = TRUE)
fviz_pca_biplot(PCA.model, label = "var", value = Dataset$PAQ_Lejla, addEllipses = FALSE, 
                ellipse.level = 0.95)


library(gridExtra)

  biplot(PCA.model, cex = c(0,07, 0.6))

  library(ggfortify)
  
  library(tidyverse)
library(palmerpenguins)

  head(PCA.model)
summary(PCA.model)  


library(ape)

screeplot(PCA.model)

plot(PCA.model$scores)  

screeplot(PCA.model$scale)  

plot(PCA.model$scores)


     autoplot(PCA.model, label = TRUE, label.size = 3,
              loadings = TRUE, loadings.label = TRUE, loadings.label.size  = 3)
library(lfda)
     
     
    
  text(PCA.model$scores, label = PCA.model, cex = 0.5)

    Lejla <- prcomp (~ xhelp + xfreeze + xbreathe + xinferior + xSupport + xNerd, data = PAQ_Lejla, scale = TRUE)

    eigenvalues <- Lejla$sdev * Lejla$sdev                      
eigenvectors <- Lejla$rotation    

summary(Lejla)


library(psych)
p6 <- fviz_pca_ind(Lejla)
p7 <- fviz_pca_biplot(Lejla)
grid.arrange(p6, p7, nrow = 3)

fviz_contrib(Lejla, choice = "var", axes = 3)

screeplot(Lejla, npcs = 6)

names(PCA.model)

PCA.model$sdev^2



summary(Lejla)
