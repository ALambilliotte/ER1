data=read.table("tournesol.txt",sep="\t",h=T,dec=".")
teneur=data$teneur
testeur=data$testeur
origine=data$origine

#GRAPHIQUES 

#Influence de chaque variable séparée
ggplot(data,aes(x=as.numeric(testeur), y=teneur))+geom_point()
ggplot(data,aes(x=as.numeric(origine), y=teneur))+geom_point()

#Influence des deux variables simultanées avec un interaction plot classique
interaction.plot(testeur,origine,teneur)

#Reproduction d'un interaction plot avec ggplot
newdata=as.data.frame.table(with(data, tapply(teneur, list(testeur, origine), mean)),dnn=c("newtesteur",'neworigine','newteneur'))
newteneur=newdata$Freq
newtesteur=newdata$Var1
neworigine=newdata$Var2
ggplot(newdata,aes(x=newtesteur,y=newteneur,group=neworigine,col=neworigine))+geom_line()+ylab("Teneur moyenne")+xlab("Testeur")+labs(colour = "Origine")


par(mfrow=c(2,2))
#MODELES LINEAIRES

#influence du testeur
lm1=lm(teneur~testeur)
anova(lm1)
summary(lm1)
plot(lm1)

#influence de l'origine
lm2=lm(teneur~origine)
anova(lm2)
summary(lm2)
plot(lm2)

#influence des deux avec interaction
lm3=lm(teneur~ftesteur*forigine)
anova(lm3)
summary(lm3)
plot(lm3)

#influence des deux sans interaction
lm4=lm(teneur~ftesteur+forigine)
anova(lm4)
summary(lm4)
plot(lm4)
