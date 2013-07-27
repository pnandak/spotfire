pommes=data.frame(
  rendement=c(48,46,52,50,52,50,49,49,53,51,55,57),
  variete= factor(rep(c("Golden","Delicious","Jonagold"),
    rep(4,3)
    )
    )
  )

attach(pommes)

fromage=data.frame(
  pH=c(5.5, 6.2, 5.4, 5.6, 6.2, 
       5.3, 6.2, 5.2, 5.4, 6.0,
       5.5, 6.4, 5.4, 5.4, 6.0, 
       5.3, 6.2, 5.4, 5.4, 6.0,
       5.6, 6.0, 5.3, 5.6, 6.3,
       5.2, 6.2, 5.1, 5.5, 6.1),
  lignes=factor(rep(c("L1","L2","L3"),rep(10,3))),
  citernes=factor(
    rep(c("C1","C2","C3","C4","C5"),6))
  )
attach(fromage)

plot(rendement~variete)

plot(pH~citernes*lignes)

interaction.plot(citernes,lignes,pH)

x11()
interaction.plot(lignes,citernes,pH)


pommes.aov=aov(rendement~variete,data=pommes)

summary(pommes.aov)

fromage.aov=aov(pH~citernes*lignes)
summary(fromage.aov)


esti.fromage=model.tables(fromage.aov,type="means",se=T)
esti.fromage

estieffets.fromage=model.tables(fromage.aov,se=T)
estieffets.fromage

pairwise.t.test(pH,citernes,p.adj="bonf")
pairwise.t.test(pH,citernes,p.adj="holm")

plot(fitted(pommes.aov),resid(pommes.aov))

hist(resid(pommes.aov))

qqnorm(resid(pommes.aov))
qqline(resid(pommes.aov))



plot(fromage.aov)


library('Rcmdr')

levene.test(pH,citernes:lignes)



# Ex 1
poulet=read.table("poulet.txt",header=T)
attach(poulet)

plot(acgras~ajout*eleva)
# différence des boxplot pour "ajout"
# donc certainement effet principal "ajout"
# peu de différence pour "eleva" donc
# certainement pas d'effet principal "eleva"

interaction.plot(ajout,eleva,acgras)
x11()
interaction.plot(eleva,ajout,acgras)
# presque parallélisme donc certainement pas d'interaction

poulet.aov=aov(acgras~ajout*eleva)
summary(poulet.aov)
# effet principal hautement significatif de "ajout"
# pas d'effet principal "eleva"
# pas d'interaction

esti.poulet=model.tables(poulet.aov,type="means",se=T)
esti.poulet

pairwise.t.test(acgras,ajout,p.adj="bonf")
# On rejette A1=A2 A2=A3 A1=A4 A2=A4
# On accepte A1=A3 A3=A4

pairwise.t.test(acgras,ajout,p.adj="holm")
# On rejette A1=A2 A1=A3 A2=A3 A1=A4 A2=A4
# On accepte  A3=A4

plot(poulet.aov)


# Ex 2
conso.dat=read.table("donneesconso.txt",header=T)
attach(conso.dat)

# Pates
plot(Pates~Region*Age)
# différence des boxplot pour "Age"
# donc certainement effet principal "Age"
# peu de différence pour "Region" donc
# certainement pas d'effet principal "Region"

interaction.plot(Age,Region,Pates)
x11()
interaction.plot(Region,Age,Pates)
# presque parallélisme donc certainement pas d'interaction

pates.aov=aov(Pates~Age*Region)
summary(pates.aov)
# effet principal hautement significatif de "Age"
# pas d'effet principal "Region"
# pas d'interaction

esti.pates=model.tables(pates.aov,type="means",se=T)
esti.pates

pairwise.t.test(Pates,Age,p.adj="bonf")
pairwise.t.test(Pates,Age,p.adj="holm")
#On rejette TA1=TA2

plot(pates.aov)

# Loisirs
plot(Loisirs~Region*Age)
# différence des boxplot pour "Age"
# donc certainement effet principal "Age"
# différence pour "Region" donc
# certainement effet principal "Region"

interaction.plot(Age,Region,Loisirs)
x11()
interaction.plot(Region,Age,Loisirs)
# presque parallélisme donc certainement pas d'interaction

loisirs.aov=aov(Loisirs~Age*Region)
summary(loisirs.aov)
# effet principal hautement significatif de "Age"
# effet principal hautement significatif de "Region"
# pas d'interaction

esti.loisirs=model.tables(loisirs.aov,type="means",se=T)
esti.loisirs

pairwise.t.test(Loisirs,Age,p.adj="bonf")
pairwise.t.test(Loisirs,Age,p.adj="holm")
#On rejette TA1=TA2

pairwise.t.test(Loisirs,Region,p.adj="bonf")
pairwise.t.test(Loisirs,Region,p.adj="holm")
# On rejette GR1=GR2 GR1=GR3
# On accepte GR2=GR3

plot(loisirs.aov)


# Revenus
plot(Revenu~Region*Age)
# différence des boxplot pour "Region"
# donc certainement effet principal "Region"
# peu de différence pour "Age" donc
# certainement pas d'effet principal "Age"

interaction.plot(Age,Region,Revenu)
x11()
interaction.plot(Region,Age,Revenu)
# presque parallélisme donc certainement pas d'interaction

revenu.aov=aov(Revenu~Age*Region)
summary(revenu.aov)
# effet principal hautement significatif de "Region"
# pas d'effet principal de "Age"
# pas d'interaction

esti.revenu=model.tables(revenu.aov,type="means",se=T)
esti.revenu

pairwise.t.test(Revenu,Region,p.adj="bonf")
pairwise.t.test(Revenu,Region,p.adj="holm")
# On rejette GR1=GR2
# On accepte GR1=GR3 GR2=GR3 !!!!

plot(revenu.aov)
