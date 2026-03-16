#Import xlsx
date3<-read.xlsx(here("Date_finale_AD.xlsx"),
                 colNames=T,
                 na.strings=c("NA", ":"," "))



row.names(date3)<-date3$`GEO.(Codes)`
head(date3)
#head(date1)
date3<-date3[,-1]
head(date3)

#Eliminare NA
date3<-na.omit(date3)
str(date3)

date3<-date3[,-1]

install.packages("corrplot")
library("corrplot")

R<-cor(date3)
round(R,3)
windows()
corrplot(R,method="number", type="upper")

xs<-scale(date3[,-1])
xs<-scale(date3)

box<-boxplot(xs, cpl="lightblue", border="purple", main="Distributii")
box$out

hist(xs, col="lightblue", main="Histograma")

apply(date3[-1], 2, mean)

##grafic ggplot
install.packages("ggplot2")
library(ggplot2)
ggplot(data=date3, aes(x=date3$PIB,y=date3$Rata_somaj))+
  geom_point(aes(size=date3$Arrears, col=date3$Risc_saracie), alpha=0.5)+
  geom_text(label=date3[,1],
            vjust=0,hjust=0,col="purple")

#Matricea produselor incrucisate
#PI=Xs transpus* Xs
PI<-t(xs)%*%xs
PI

#Matrice de covarianta
cov<-cov(xs)
cov
#Covarianta = Arata legatura, dependenta dintre 2 var
#Coef de cov poate lua orice val
#Pt mat de CORELATIE ->coef sunt intre (-1,1)
#PI/(n-1)=cov

n<-dim(date3)[1]
n
PI/(n-1)
dif<-PI/(n-1)-cov
round(dif,2)

#Putem interpreta summary ul, de facut concluzii, coef de asimetrie, histograma etc

#--------------------------------------------------------------Seminarul 4

#Extrageți vectorul coloană pentru care se înregistrează cea mai mare abatere standard
vectorSD<-apply(date3, 2, sd)
vectorSD
coloana1<-date3[,which.max(vectorSD)]#PIB
coloana2<-date3[,which.min(vectorSD)]#Masini electrice

cor(coloana1,coloana2)

#Norma unui vector = distanta unui vector de la vector pana la origine

#||x||=d(0,x)=sqrt(x transpus*x)= suma din (xi patrat)^1/2

coloana1C<-scale(coloana1, center=T, scale=F)
coloana2C<-scale(coloana2, center=T, scale=F)

#Norma unui vector
norm(coloana1, type="2")

#cos=(x transpus * y)/(norma din x * norma din y)
cos<-(t(coloana1C)%*%coloana2C)/(norm(coloana1C,type="2")*norm(coloana2C, type="2"))
cos
#(R dintre PIB si masini electrice = cos)
#cand lucram cu date centrate, val coef de cor = val cos

#3. Calculați distanța euclidiană dintre 2 regiuni din România
#Distanta euclidiana = ne arata distanta dintre 2 vectori
#x=(x1,x2)
#y=(y1,y2)
#dE(x,y)=sqrt((x1-y1)^2+(x2-y2)^2)

X<-date3
row.names(X)
X[87,]
X[88,]

norm(X[87,-1], type="2")
dist(rbind(X[87,-1], X[87,-1]), method="euclidian")

#Extrageți valorile proprii ale matricii de corelație
R<-cor(X[,-1])
R<-cor(X)

desc<-eigen(R)
desc
E<-eigen(R)
E
valori<-desc$values
valori
#Prima componenta exprima cea mai mare cantitate din inf mele
vectori<-desc$vectors
vectori

#Verificați dacă matricea formata din vectorii proprii este ortogonală
I<-t(vectori)%*%vectori
round(I,2)

#Scrieti descompunerea matricii de corelație folosind vectorii proprii. Comparati matricile.
#𝐴 = 𝑉 𝑑𝑖𝑎𝑔(𝜆)𝑉−1
C<-vectori%*%diag(valori)%*%t(vectori)
C

round(R-C,2)

X<-date3
Xs<-scale(X)
E<-eigen(R)

lambda<-E$values
lambda
vectori<-E$vectors
vectori

desc<-vectori%*%diag(lambda)%*%t(vectori)
round(desc,2)
round(R,2)
round(desc-R,2)

norm(vectori[,1], type="2")

I<-t(vectori)%*%vectori
round(I,2)
#Ar trebii sa verifiam daca norma unui vector propriu e 1

#----------------------------------------------------------------------------------Seminar 5

#Analiza componentelor principale, princomp()
#Inputul nostru: variabilele initiale Xn*p
#Rp*p ->
#Output: Z1 Z2 ...Zp (nr a cate var avem)
#cov (Zi,Zj)=0

#Zi=X*fi
#X= mat initiala de date
#fi 1=vector ponderi

acp<-princomp(Xs,cor=T, scores=T)
acp

summary(acp)
#Proportion of variance - cantitatea de inf exprimata de fiecare componenta in parte
#Proportion of variance -> Prima componenta sintetizeaza aproximativ 43,7% din cantitatea initiala de inf, cea de-a doua componenta explica aproximativ 16,9%, iar cea  de-a treia, 10,6%
#Cumulative Proportion -> Primele 2 comp principale explica (cumulat, impreuna) 60,6% din informatia initiala
#Daca dupa primele 3 componente > 60 % e bine => nu se pierd multe inf

########TEMA: Summary ACP de interpretat si ce cantitate de inf avem stocat in primele 3 componente  

#Abateri standard
stdev<-acp$sdev
stdev

#Care este legătura dintre valorile cu abaterile standard de mai sus și valorile Lambda?
lambda
#lamba  reprez var/dispersia componentelor principale
#(stdev)^2=lambda
#Z1=X*fi1
#var Z1 = lambda 1
#lambda 1 >lambda 2> lambda p
#Cum se calculeaza proc de inf explicat/preluat de prima componenta

#$ infi i= lambdai/(suma din lambda i de la i la p )
lambda[1]/sum(lambda)
sum(lambda)
#Conservam var totala din var initiale

phi<-acp$loadings
phi
vectori

#Cele 2 mat sunt egale, exista diferenta de semn
#R*fi=lamba*fi 1
#inmultim cu o constanta care este tot -1, nu se schimba calittaea de vector propriu, interpretarea nu se moidifica
#loadings = vectori proprii ai mat de corel/ vectori de ponderi folositi pt a construi componentele principale

###Cum se obtine prima componenta: Z1=0.313-0.273-0.215-0.318.....

##Scoruri
scoruri<-acp$scores
#Mat factor ne arata relatia dintre componentele principale si var initiale
#Mat scorurilor are dimensiunea de n*p
Z<-Xs%*%phi
Z

#extragem scorul pt regiunea BE10 pe componenta 1
BE10<-Xs[row.names(Xs)=="BE10", ]%*%phi[,1]
BE10
Xs[1,]

#scree plot - elbow
windows()
plot(acp, type="l")
#Vedem cu descresc inf variabilelor
#Facem o taietura si alegem acel nr de componente pentru care plusul de inf este redus, luam primul nr la stanga

#TEMA: summary(acp) - screenshor, word, compilat si cat % din inf initiala este explicata de primele 3 componente principale

#------------------------------------------------------Seminarul 6
#Analiza componentelor principale
acp<-princomp(Xs,cor=T, scores=T)
#Extragem scorul pt reg RO#@ pt componenta 2
#metoda1
scoruri<-acp$scores
scoruri[row.names(scoruri)=="RO32", 2]
acp$loadings
#comp 2<- 0.293*PIB+0.428*Rata_somaj +0.184*Arrears....
RO31<-Xs[row.names(Xs)=="RO31", ]%%acp$loadings[,2]


summary(acp)
#Criteriul lui kaiser: alegem componente care au lambda > 1
acp$sdev^2
acp2$eig #e la fel prima coloana cu acp$sdev^2 doar ca pot sa folosesc acp2
#(iau 3 componente , sunt mai mari decat 1 doar 3!)
plot(acp, type="l")
#In cazul meu pot lua doar 3 variabile (varianta se stabilizeaza dupa)

#matricea factor arata relatia (corelatia) dintre var initiale standardizate si noile componente
MF<-cor(Xs, scoruri)
#Componenta 1 se coreleaza puternic cu variabila HRST,PIB...
#Componenta 2 se coreleaza putenic cu var Rata_somaj
MF
#TEMA: Dau un nume componentelor (ca in documntul postat)
#Comp 1: PIB, Risc_saracie, Rata_somaj,
#Vad cum le bag si le asez
windows()
biplot(acp)
#Interpretam unghiul => ascutit=> var puternic corelate
#Putem identifica posibile clustere (regiuni asemanatoare) si outlieri (BE10, SE11, RO31)
install.packages("factoextra")
install.packages("FactoMineR")
library(factoextra)
library(FactoMineR)

#--------------------------------------- Seminar 7

# functia PCA

?PCA
acp2<-PCA(date3, scale.unit=T, ncp=13)
#cos^2 = calitatea reprezentarii fiecarei regiuni
#contrib ne arata contributia pe care var respectiva o are in componenta 
#ind = individuals = regiunile mele 
#coord = coordonatele in noul spatiu
#dist = distantele fata de centrul de greutate
# svd = singular value decomposition = descompunerea matricii
#Dim 1 = Comp 1 
summary(acp) # aici avem ab standard
summary(acp2) #aici avem varianta = dispersie 
#dim 1 = scorurile componentei 1 
#daca ctr mai mare => regiunea este bine reprezentata pe componenta mea 
acp2$ind$coord
#ar trebui sa fie egala cu acp$scores - pot exita diferente de semn doar, in rest, sunt la fel
acp$scores
#cos^2 mare => buna reprezentare
###acp2$svd$vs - coordonatele coef de corelatie ai vectorilor proprii (vectorii proprii * sqrt(lambda))
###acp$loadings - vectorii proprii
#Variables: 
#Dim = Matricea factor => Dim 1 = coef de corelatie 
#cos^2 = dim^2= => 0.555 = -0.745^2
#daca adunam pe coloana toate val de pe cos^2, trebuie sa obtinem lambda 
#0.555+0.424+0.262 +.....=lambda
#suma de cos^2 pe linie trebuie sa dea 1!
#0.555+0.189+0.002 + ...= 1
lambda
#X13 = HRST = 12,49 % din variatia comp pincipal
#contrib(X13, Z1) = contrib(HRST, Z1)
#12,49=0.710/(0.555+0.424+0.262+ ...0.710)*100

#scree plot
windows()
fviz_eig(acp2)

#Cercul corelatiilor 

fviz_pca_var(acp2)
#Cu cat unghiul e mai ascutit => cor mai mare
#Cu cat vectorul se apropie mai mult de circumferinta cercului (lungimea mai mare) => mai bine reprezentat pentru componentele mele 
#unaffordable_meal pare slab reprezentat
#lungimea vectorului se calculeaza prin adunarea cos^2 pt var respectiva -> ca sa vedem care se apropie cel mai mult de circumferinat cercului

fviz_pca_var(acp2, col.var="contrib") #am adaugat si contributia => Arrears si unaffordable_meal cam slab reprezentate
windows()
fviz_pca_var(acp2,axes=c(1,3), col.var="contrib") #am pus Dim 3 = Componenta 3  
fviz_pca(acp2)
windows()
fviz_pca_ind(acp2)
#in partea dreapta sus => val mari pentru ambele componente
#in coltul din drepata jos => val mari pt dim 1 si val mici pt dim 2 
windows()
biplot(acp)
windows()
fviz_pca_ind(acp2, axes=c(1,3), col.ind="cos2")
windows()
fviz_pca_biplot(acp2)

#tema -> Reprezentați regiunile analizate astfel încât să surprindeți valorile înregistrate pentru
#primele 4 componente principale. Folosiți ggplot().

#folosim gg plot, cu geom point, scoruri = dim 1, dim 2, dim 3 = col, dim 4 - size
#seminar 2-3 (pag 3 pe platforma grafic exemplu)

acp2$ind$coord #aici avem scorurile
#scriem o concluzie pe baza graficului, daca avem outlieri ii identificam si de ce apar
library(ggplot2)
scoruri_df<-as.data.frame(acp2$ind$coord)
windows()
ggplot(scoruri_df, aes(x=scoruri_df$Dim.1,y=scoruri_df$Dim.2))+
  geom_point(aes(size=scoruri_df$Dim.3, col=scoruri_df$Dim.4), alpha=0.5)+
  geom_text(label=rownames(scoruri_df),
            vjust=0,hjust=0,col="purple")


#---------------------------------------------------Seminar 8 

#Analiza corespondentelor

#Risc saracie - Abandon scolar

#trebuie sa le tranformam in var categ, cu cel putin 3 categorii

#ca sa calculam nr maxim de dimensiuni, se calculeaza prin fromula: min(nr linii-1, nr col-1)

#quartile = impart distributia in 4 parti: 25,50,75,100
#quantile = impart distributia in 3 parti: 33,66,99

#X4=Risc_saracie 
#X7= Abandon_scolar
R
#Calculam quantilele (pragurile noastre)
pragRS<-quantile(date3$Risc_saracie, probs =c(0.33,0.66,0.99))
pragRS
pragAbandon<-quantile(date3$Abandon_scolar, probs=c(0.33,0.66,0.99))
pragAbandon

#construim noile var categoriale
RSc<-rep(0,101)
Abandonc<-rep(0,101)

#construim RS=Risc saracie
RSc[date3$Risc_saracie<pragRS[1]]<-"Risc_sarac.low"
RSc[date3$Risc_saracie>=pragRS[1]&date3$Risc_saracie<pragRS[2]]<-"Risc_sarac.medium"
RSc[date3$Risc_saracie>=pragRS[2]]<-"Risc_sarac.high"
table(RSc)

#construim x4c
Abandonc[date3$Abandon_scolar<pragAbandon[1]]<-"Abandon_scolar.low"
Abandonc[date3$Abandon_scolar>=pragAbandon[1]&date3$Abandon_scolar<pragAbandon[2]]<-"Abandon_scolar.medium"
Abandonc[date3$Abandon_scolar>=pragAbandon[2]]<-"Abandon_scolar.high"
table(Abandonc)

RSf<-factor(RSc)
table(RSf)
Abandonf<-factor(Abandonc)
#construim tabelul de contigenta
table(RSf, Abandonf)

tab<-table (RSf,Abandonf)
library(openxlsx)
write.xlsx(tab, "tabel_conting.xlsx")

#Testul Chi ^2
#X0= cele 2 var sunt independente nu exista asociere intre randuri/col
#Xi= exista o asociere intre cele 2 var categoriale
#Chi^2 = suma i suma j ((nij-nij caciula)^2)/nij caciula

install.packages("ca")
library(ca)
ac<-ca(tab)#in tab am stocat tabelul de contigenta
summary(ac)

#prima dimensiune preia aprox 95% din inertia initiala
#total = 0.205599 = inertia total din excel

#k= ,coordonatele in noul spatiu
#mass = frecv relative marginale ponderi, sunt la fel ca in excel
#qlt= valoarea reprezentarii punctului in grafic=> daca e aproape de 1 => e bine=> bine reprezentat
#inr = contributia pe care fiecare linie/coloana o are la inertie
#la randuri, cea mai mare contrib o are Rsc_srclc = 0,565; iar pe col  Abndn_sclrh = 0,547

#tema: summary , cat la % din inf e preluata de prima componenta sau a doua componenta (inf din inertie) , de incarcat si excelul cu calculele facute 

#Tema: Prima componenta preia 95.3% din inertie 
#A doua componenta preia 4,7% din inertia totala
#Iar cumulat, se preia 100% din inertie
#-------------------------------------------------Seminar 9
#cor = calitatea reprezentarii
#qlt=suma cor
#ctr = contibutia pe care o are fiecare componenta in parte
windows()
plot(ac)
install.packages("factoextra")
library(factoextra)
windows()
fviz_ca(ac)
#putem vedea categoriile care contribuie cel mai mult la inertia totala. 
#(categ care se afla in jurul originii nu se diferentiaza foarte mult de profilul mediu, cele care se distanteaza=> explica cea mai mare parte din inertie - in cazul meu: Risc_sarac.low si Abandon_scolar.low)
#Risc_sarac.low are o contributie mare pt dim 1 si mica pt dim 2 
#ASOCIERI dintre categoriile variabilei mele: Avem regiuni cu Risc al saraciei mic si cu abandon scolar, regiuni unde avem Risc_saracie mare si Abandon scolar mare, la fel si pentru Mediu
#Pe cele2 axe vedem inertia explicata : Dim 1 (95,3)
#daca nu ies apropiate categ ca sa putem interpreta, putem face 4 - 5 categorii si impartim la quarile si ne putem uita la distributie 
#sau ac intre alte variabile.

#Tema: Ne extragem cele 2 outputuri (cel numeric si cel grafic), sa l pun intr un pdf si pe baza analizei sa fac niste concluzii.

#----------------------------- Analiza factoriala
#AF cauta niste structuri latente in spatele datelor (exista path uri ascunse pe care trebuie sa le aflam)

#Corelatia partiala

m1<-lm(Rata_somaj~Risc_saracie, data=date3)
r1<-m1$residuals
m2<-lm(Risc_saracie~inability_warm_house, data=date3)
r2<-m2$residuals
cp<-cor(r1,r2)
cp

R<-cor(date3)
R
P<-solve(R)
P

#Rata_somas si Risc_saracie
cpRS_RS<--1*P[2,4]/sqrt(P[2,2]*P[4,4])
cpRS_RS

#Teste
#Indicele KMO
install.packages("psych")
library(psych)
KMO(R)
#overall MSA = indice global
#in functie de valoarea data, vedem daca analiza se preteaza sau nu => are sens sa aplic analiza factoriala pe datele mele 
#daca val e prea mica, putem sa ne uitam care variabila are coef mai mic si o eliminam si mai facem inca o data testul 
#Daca avem KMO <0,5=> nu este recomandata AF

#Testul Bartlett (sfericitate) => vedem daca mat este ortogonala 
#H0: mat cor = mat unitate => nu are sens sa facem analiza factoriala
cortest.bartlett(R,n=101)

#$p.value = 1.133817e-150 => se respinge ip nula => avem cel putin un factor comun care sa explice variatia din variabilele mele 

#ANALIZA FACTORIALA
#metoda verosimiliotatii maxime, fara rotirea axelor 
windows()
model1<-fa(R, nfactors=2, n.obs=101, rotate="none", fm="ml")
fa.diagram(model1)
#Primul factor latent imprima variatia variab HRST, Rata_somaj, Risc_saracie....unaffordable meal
#Al doilea factor latent imprima variatia variab Studii_tertiare si Natural_Change

model1
#In ML2 si Ml1 avem ponderile/intensitatiile (Q urile) 
#cor(F,X)=Q
#H^2 = factorii comuni, comunalitatea 
#Var(Xi)comunalitate + specificitate 
#Daca com mare => Variatia var nu este bine explicata de cei 2 factori 
install.packages("nFactors")
library(nFactors)
windows()
fa.parallel(scale(date3))
#Pastrez 3 factori 
#Dupa al trilea factor, valorile simulate sunt mai mari decat cele reale, asadar pastrez pentru analiza mea doar 3 factori.

model2<-fa(R, nfactors=3, n.obs=101, rotate="none", fm="ml")
fa.diagram(model2)
model2
#extragem matricea de loadings (de Q)
Q<-model2$loadings
comunalitateRsom<-sum(Q[2,]^2)
comunalitateRsom
comRsom<-comunalitateRsom^2/sum(Q[2,]^4)#nu trebuia sa fie la a doua in loc de a patra?
comRsom

#met pa, fara rotirea axelor
model3<-fa(R, nfactors=3, n.obs=101, rotate="none", fm="pa")
windows()
fa.diagram(model3)
model3

#Aleg sa folosesc mai departe in analiza mea metoda 3, deoarece are 3 factori si explica 64% 
#din varianta totala, in timp ce modelul 1 cu 2 factori explica doar 55% din varianta.
#Desi modelul 2 explica tot 64%, variabila Rata_somaj are o comunalitate (h^2) de 1 
#Dupa ce ne alegem sol optima, trebuie sa denumim factorii dupa corelatii

#---------------------------Seminar 13
#---------------------------Analiza cluster 
install.packages("cluster")
library(cluster)
#folosim pentru a grupa obiectele, regiunile , in grupe sau clustere pe baza unor caracteristici similare
#Met folosite pentru a calcula distanta dintre clase
#Metoda celor mai apropiati vecini
#Metoda celor mai indepartati vecini 

#Refacem analiza comp princ cu primcomp si ne extragem primele 2 comp principale
#Extragem scorurile pt primele 2 componente
acp<-princomp(Xs,cor=T, scores=T)
scoruri<-acp$scores[,1:2]
scoruri

#Calculam mat distantelor
d<-dist(scoruri)
View(as.matrix(d))
#distanta totala pt  este de 5050: (n^2-nr observatii (n))/2

#Alg de clusterizare ierarhica
#ierarhie prin metoda single
ierarhieS<-hclust(d, method="single")
ierarhieC<-hclust(d, method="centroid")
ierarhieW<-hclust(d, method="ward.D2")

ierarhieW$height
#height = reprez distanta la care s a facut agregarea (distantele de agregare)

ierarhieW$merge
#Pasii prin care s au realizat comasarile, pasii de agregare ai fiecarei iteratii, ce obs a functionat cu ce obs, cine a format cluster cu cine si asa mai departe 
#La prima obs au fuzionat regiunile -19 si -98 la primul cluster
#Pana la iteratia 14 este doar un cluster, apoi, se formeaza alt cluster (valoarea 9 este cu + si de aceea)
#dendograma
plot(ierarhieS, main="Single")
plot(ierarhieC, main="Centroid")
plot(ierarhieW, main= "Ward")
#Cea mai buna metoda este Ward 
#Ar trebui sa facem o taietura unde identificam cel mai mare salt 
#Pastram o sulutie cu 3 clustere
solutie3<-cutree(ierarhieW, k=3)
table(solutie3)
rect.hclust(ierarhieW, k=3, border=2:4)
#putem sa extragem solutia si cu hcut
library(factoextra)
sol3<-hcut(scoruri, k=3,hc_method = "ward.D2")
#reprezentare grafica
fviz_cluster(sol3)
fviz_cluster(list(data=scoruri, cluster=solutie3))
#ca sa putem interpreta mai bine, putem sa ne uitam la media variabilelor din fiecare cluster 
#media variabilelor 
aggregate(scoruri, list(solutie3), mean)
#Regiunile din primul cluster prezinta Prosperitate si potential uman mare ....
#Variabilitatea dintre clustere sa fie catr mai mare, iar in interirorul clusterelor cat mai mica, variabilele sa fie cat mai omogene (similare una cu alta)

sol3$cluster

#Evaluarea solutiei
#evaluam daca o regiune este bine incadrata in clusterul in care a fost ea definita
#Valorile sunt cuprinse intre -1 si 1
#Daca avem o valoare aproape de 1 , inseamna ca regiunea a fost bine incadrata in acel cluster
#Daca avem o val apropiata de -1 , regiunea respectiva a fost incorect clasificata
#Daca avem o val in jurul lui 0 => Obs se afla la granita dintre 2 clustere (putea sa fie clasificata fie in clusterul X, fie in clusterul Y)

s<-silhouette(solutie3, d)
View(s)
plot(s)
row.names(date3)[s[, "sil_width"] < 0]
#asa pot vedea regiunile care nu sunt bine clasificate in clusterul meu
#solutie cu 2 clustere 
sol2<-cutree(ierarhieW, k=2)
s2<-silhouette(sol2, d)
plot(s2)
fviz_cluster(list(data=scoruri, cluster=sol2))

#--------------------------------------------Seminar 14
km <- kmeans(scoruri,3)
clase <- km$cluster
library(cluster)
s_km<-silhouette(clase, d)
plot(s_km, col=c("lightpink", "lightblue", "aquamarine3"))
View(s_km)#ca sa vad daca sunt valori negative in latimea siluetei (este sub forma de tabel/matrice)
row.names(date3)[s_km[, "sil_width"] < 0]#aici vad codul regiunii care are latimea siluetei negativa si care nu e bine clasificata in cluster 

fviz_cluster(palette=c("lightpink", "lightblue3", "aquamarine3"),list(data=scoruri, cluster=clase))
km$centers #putem vedea centroizii, putem compara cu aggregate, se schimba distributia pe clase? e posibil ca regiunile sa nu se mai aseze la fel 
table(clase)


#Trebuie sa vad ce metoda aleg, cea ierarhica sau asta cu kmeans => aleg metoda kmeans
#Ca sa se pastreze solutia mereu, putem pune un set.seed 
