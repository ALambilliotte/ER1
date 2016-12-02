data=read.table("tournesol.txt",sep="\t",h=T,dec=".")
teneur=data$teneur
testeur=data$testeur
origine=data$origine

#Influence de chaque variable séparée

#influence de l'origine
ggplot(data,aes(x=origine, y=teneur))+geom_point()
lm2=lm(teneur~origine)
summary(lm2)
plot(lm2)

#influence du testeur
ggplot(data,aes(x=testeur, y=teneur))+geom_point()
lm1=lm(teneur~testeur)
summary(lm1)
plot(lm1)

#Influence des deux variables simultanées

#Reproduction d'un interaction plot avec ggplot
newdata=as.data.frame.table(with(data, tapply(teneur, list(testeur, origine), mean)),dnn=c("newtesteur",'neworigine','newteneur'))
newteneur=newdata$Freq
newtesteur=newdata$Var1
neworigine=newdata$Var2
ggplot(newdata,aes(x=newtesteur,y=newteneur,group=neworigine,col=neworigine))+geom_line()+ylab("Teneur moyenne")+xlab("Testeur")+labs(colour = "Origine")

#influence des deux avec interaction
lm3=lm(teneur~testeur*origine)
summary(lm3)
plot(lm3)
