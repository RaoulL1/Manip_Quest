                                      
#       ANALYSE ADO BODY CONSCIOUSNESS               
#                      network analysis SCRIPTS       

renv::activate()
library(psych)
#library(foreign)
#library(qgraph)
#library(bootnet)
#library(glasso)
#library(mgm)  #for mixed model networks 
#library(igraph)
#library(psych)
#library(GPArotation)
#library(ggplot2)
#library(ggcorrplot)
#library(corpcor)
#library(Matrix)
library(naniar) ##lille MCAR test
library(mice, warn.conflicts = FALSE) # imputer les données manquante
library(visdat)#visualiser les données manquantes
library(networktools)
library(qgraph)
library(bootnet)
library(glasso)
library(dplyr)
library(conflicted) # pour forcer à utiliser un des package quand il y a un  conflit pour une fonction


library("IsingSampler")
library("IsingFit")
library("NetworkComparisonTest" )


conflict_prefer("alpha", "psych")
#save.image("../R_Env_Quest/validity.RData")

##### 1 DATA 
Item_bysub <- read.csv("../../Share/Analyses/Item_bysub_all.csv")
dataglobal <- read.csv("../../Share/Analyses/exporterdansR.csv")
miss_subscale_bcp<- read.csv("../../Share/Analyses/miss_subscale_bcp.csv")



# VALIDITY AND CONSISTENCY ------------------------------





# Sélectionner les colonnes que vous souhaitez inclure dans le calcul de l'alpha de Cronbach
Dcons_col <- c("Dcons01", "Dcons02", "Dcons04", "Dcons05", "Dcons06", "Dcons07", "Dcons08", "Dcons09", "Dcons10", "Dcons11", 
          "Dcons12", "Dcons13", "Dcons14", "Dcons15", "Dcons16", "Dcons17", "Dcons18", "Dcons19", "Dcons20", "Dcons21", "Dcons22", "Dcons23")
Dcons <- Item_bysub[,Dcons_col]
# Calculer l'alpha de Cronbach pour les données sélectionnées
alpha_Dcons0<-alpha(Dcons)
alpha_Dcons<-round(alpha_Dcons0$total$raw_alpha,digit=2)
#Some items ( Dcons03 ) were negatively correlated with the total scale and =>> donc je supprime le D3 dans le script spyder
#raw alpha 0.86

Dpriv_col<- c("Dcons01","Dcons05", "Dcons07", "Dcons09", "Dcons13", "Dcons15", "Dcons18","Dcons20","Dcons22")
Dpub_col<- c("Dcons02","Dcons06", "Dcons11", "Dcons14", "Dcons17", "Dcons19", "Dcons21")
Danx_col<- c("Dcons04","Dcons08", "Dcons10", "Dcons12", "Dcons16", "Dcons23")
Dpriv <- Item_bysub[,Dpriv_col]
Dpub <- Item_bysub[,Dpub_col]
Danx <- Item_bysub[,Danx_col]
alpha_Dpriv<-round(alpha(Dpriv)$total$raw_alpha,digit=2)
alpha_Dpub<-round(alpha(Dpub)$total$raw_alpha,digit=2)
alpha_Danx<-round(alpha(Danx)$total$raw_alpha,digit=2)


EIRI_col<-c('EIRI01','EIRI02','EIRI03','EIRI04','EIRI05','EIRI06','EIRI07','EIRI08','EIRI09',
            'EIRI10','EIRI11','EIRI12','EIRI13','EIRI14')
EIRI<- Item_bysub[,EIRI_col]
alpha_EIRI0<-alpha(EIRI)
alpha_EIRI<-round(alpha_EIRI0$total$raw_alpha,digit=2)
#raw alpha 0.79
EPT_col<-c('EIRI02','EIRI04','EIRI06','EIRI08','EIRI11','EIRI13')
EEC_col<-c('EIRI01','EIRI03','EIRI05','EIRI07','EIRI09','EIRI10','EIRI12','EIRI14')
EPT <- Item_bysub[,EPT_col]
EEC <- Item_bysub[,EEC_col]
alpha_EPT<-round(alpha(EPT)$total$raw_alpha,digit=2)
alpha_EEC<-round(alpha(EEC)$total$raw_alpha,digit=2)


Greseau_col<-c('Greseau01', 'Greseau02','Greseau03','Greseau04', 'Greseau05','Greseau06',
            'Greseau07', 'Greseau08','Greseau09','Greseau10', 'Greseau11')
Greseau<- Item_bysub[,Greseau_col]
alpha_Greseau0<-alpha(Greseau)
alpha_Greseau<-round(alpha_Greseau0$total$raw_alpha,digit=2)
#raw alpha 0.89
Gcomp_col<-c('Greseau01', 'Greseau02','Greseau03','Greseau04')
Gcog_col<-c('Greseau05','Greseau06','Greseau07')
Gaffec_col<-c( 'Greseau08','Greseau09','Greseau10', 'Greseau11')
Gcomp <- Item_bysub[,Gcomp_col]
Gcog <- Item_bysub[,Gcog_col]
Gaffec <- Item_bysub[,Gaffec_col]
alpha_Gcomp<-round(alpha(Gcomp)$total$raw_alpha,digit=2)
alpha_Gcog<-round(alpha(Gcog)$total$raw_alpha,digit=2)
alpha_Gaffec<-round(alpha(Gaffec)$total$raw_alpha,digit=2)


Hest_col<-c( 'Hest01','Hest02','Hest03','Hest04','Hest05','Hest06','Hest07','Hest08','Hest09','Hest10',
                'Hest11','Hest12','Hest13','Hest14','Hest15','Hest16','Hest17','Hest18','Hest19','Hest20','Hest21','Hest22','Hest23')
Hest<- Item_bysub[,Hest_col]
alpha_Hest0<-alpha(Hest)
alpha_Hest<-round(alpha_Hest0$total$raw_alpha,digit=2)
##Some items ( Hest05 ) were negatively correlated with the total scale and probably should be reversed.
#raw alpha 0.85
Happa_col<-c('Hest01','Hest06','Hest07','Hest09','Hest11','Hest13','Hest15','Hest17','Hest21','Hest23')
Hwei_col<-c('Hest03','Hest04','Hest08','Hest10','Hest16','Hest18','Hest19','Hest22')
Hattri_col<-c('Hest02','Hest05','Hest12','Hest14','Hest20')
Happa <- Item_bysub[,Happa_col]
Hwei <- Item_bysub[,Hwei_col]
Hattri <- Item_bysub[,Hattri_col]
alpha_Happa<-round(alpha(Happa)$total$raw_alpha,digit=2)
alpha_Hwei<-round(alpha(Hwei)$total$raw_alpha,digit=2)
alpha_Hattri<-round(alpha(Hattri)$total$raw_alpha,digit=2)

Iobj_col<-c('Iobj01','Iobj02','Iobj03','Iobj04','Iobj05','Iobj06','Iobj07','Iobj08')
Iobj<- Item_bysub[,Iobj_col]
alpha_Iobj0<-alpha(Iobj)
alpha_Iobj<-round(alpha_Iobj0$total$raw_alpha,digit=2)



IIpress_col<-c('IIpress01','IIpress02','IIpress03','IIpress04','IIpress05','IIpress06','IIpress07','IIpress08','IIpress09','IIpress10','IIpress11','IIpress12')
IIpress<- Item_bysub[,IIpress_col]
alpha_IIpress0<-alpha(IIpress)
alpha_IIpress<-round(alpha_IIpress0$total$raw_alpha,digit=2)
IIfam_col<-c('IIpress01','IIpress02','IIpress03','IIpress04')
IIpee_col<- c('IIpress05','IIpress06','IIpress07','IIpress08')
IImed_col<- c('IIpress09','IIpress10','IIpress11','IIpress12')
IIfam <- Item_bysub[,IIfam_col]
IIpee <- Item_bysub[,IIpee_col]
IImed <- Item_bysub[,IImed_col]
alpha_IIfam<-round(alpha(IIfam)$total$raw_alpha,digit=2)
alpha_IIpee<-round(alpha(IIpee)$total$raw_alpha,digit=2)
alpha_IImed<-round(alpha(IImed)$total$raw_alpha,digit=2)


Jcons_col<-c('Jcons01','Jcons02','Jcons03','Jcons04','Jcons05')
Jcons<- Item_bysub[,Jcons_col]
alpha_Jcons0<-alpha(Jcons)
alpha_Jcons<-round(alpha_Jcons0$total$raw_alpha,digit=2)

Kinter_col<-c('Kinter01','Kinter02','Kinter03','Kinter04','Kinter05','Kinter06','Kinter07','Kinter08','Kinter09','Kinter10',
             'Kinter11','Kinter12','Kinter13','Kinter14','Kinter15','Kinter16','Kinter17','Kinter18','Kinter19','Kinter20','Kinter21')
Kinter<- Item_bysub[,Kinter_col]
alpha_Kinter0<-alpha(Kinter)
alpha_Kinter<-round(alpha_Kinter0$total$raw_alpha,digit=2)
#Some items ( Kinter06 Kinter07 Kinter08 Kinter09 ) were negatively correlated with the total scale and 
#raw alpha 0.76

K1inter_col<-c('Kinter01','Kinter02','Kinter03','Kinter04')
K1inter<- Item_bysub[,K1inter_col]
alpha_K1inter0<-alpha(K1inter)
alpha_Knoticing<-round(alpha(K1inter)$total$raw_alpha,digit=2) #raw alpha 0.67

K2inter_col<-c('Kinter05','Kinter06','Kinter07')
K2inter<- Item_bysub[,K2inter_col]
alpha_K2inter0<-alpha(K2inter)
alpha_Knotdistact<-round(alpha(K2inter)$total$raw_alpha,digit=2) #raw alpha 0.66

K3inter_col<-c('Kinter08','Kinter09','Kinter10')
K3inter<- Item_bysub[,K3inter_col]
alpha(K3inter)
alpha_K3inter0<-alpha(K3inter)
alpha_Knotworry<-round(alpha(K3inter)$total$raw_alpha,digit=2) #raw alpha 0.55

K4inter_col<-c('Kinter11','Kinter12','Kinter13','Kinter14','Kinter15')
K4inter<- Item_bysub[,K4inter_col]
alpha(K4inter)
alpha_K4inter0<-alpha(K4inter)
alpha_Kemotion<-round(alpha(K4inter)$total$raw_alpha,digit=2) #raw alpha 0.78

K5inter_col<-c('Kinter16','Kinter17','Kinter18')
K5inter<- Item_bysub[,K5inter_col]
alpha(K5inter)
alpha_K5inter0<-alpha(K5inter)
alpha_Klistening<-round(alpha(K5inter)$total$raw_alpha,digit=2) #raw alpha 0.77

K6inter_col<-c('Kinter19','Kinter20','Kinter21')
K6inter<- Item_bysub[,K6inter_col]
alpha(K6inter)
alpha_K6inter0<-alpha(K6inter)
alpha_Ktrusting<-round(alpha(K6inter)$total$raw_alpha,digit=2) #raw alpha 0.79



NLpairs_col<-c('NLpairs01','NLpairs02','NLpairs03','NLpairs04','NLpairs05','NLpairs06','NLpairs07','NLpairs08','NLpairs09','NLpairs10')
NLpairs<- Item_bysub[,NLpairs_col]
alpha_NLpairs0<-alpha(NLpairs)
alpha_NLpairs<-round(alpha(NLpairs)$total$raw_alpha,digit=2) 
#raw alpha 0.67



#############################SOCIAL 

L2tact_soc_col<-c('L2tact01','L2tact02','L2tact03','L2tact04','L2tact05','L2tact06','L2tact07','L2tact08')
L2tact_soc<- Item_bysub[,L2tact_soc_col]
alpha(L2tact_soc)
#raw alpha 0.68
L2tact_soc_col<-c('L2tact01','L2tact02','L2tact03','L2tact04','L2tact05','L2tact06','L2tact08') #supprimer le 07
L2tact_soc<- Item_bysub[,L2tact_soc_col]
alpha_L2tact_soc0<-alpha(L2tact_soc)#raw alpha 0.73
alpha_L2tact_soc<-round(alpha(L2tact_soc)$total$raw_alpha,digit=2) 


##################BASE SUR PCA

RC1_col<-c('L2tact01','L2tact02','L2tact03','L2tact04','L2tact05','L2tact06','L2tact08','L4act07','L4act04') #supprimer le 07
RC1<- Item_bysub[,RC1_col]
alpha(RC1)#raw alpha 0.77

RC2_col<-c('L2tact01','L2tact04','L2tact05','L3mouv02','L3mouv06','L4act08','L1tact01') #supprimer le 07
RC2<- Item_bysub[,RC2_col]
alpha(RC2)#raw alpha 0.54


RC3_col<-c('L4act05','L4act09','L4act06','L4act01','L4act02') 
RC3<- Item_bysub[,RC3_col]
alpha(RC3)#raw alpha 0.52

RC4_col<-c('L4act01','L3mouv04','L1tact07','L3mouv08','L4act03') 
RC4<- Item_bysub[,RC4_col]
alpha(RC4,check.keys=TRUE)#raw alpha 0.54




##################BASE SUR 
LL_col<-c('L3mouv01','L3mouv02','L3mouv03','L3mouv04','L3mouv05','L3mouv06','L3mouv07','L3mouv08',
      'L4act01','L4act02','L4act03','L4act04','L4act05','L4act06','L4act07','L4act08','L4act09','L4act10')
LL<- Item_bysub[,LL_col]
alpha_LDunn0<- alpha(LL,check.keys=TRUE) #0.70
alpha_LDunn<- round(alpha(LL,check.keys=TRUE)$total$raw_alpha,digit=2) 

L_mouv_activite_col<-c('L3mouv01','L3mouv03','L3mouv04','L3mouv05','L3mouv06','L3mouv07','L3mouv08', #'L3mouv02'
                       'L4act02','L4act04','L4act05','L4act06','L4act07','L4act09','L4act10') #'L4act01',,'L4act03''L4act08',
L_mouv_activite<- Item_bysub[,L_mouv_activite_col]
alpha(L_mouv_activite,check.keys=TRUE) #0.72

Lmouv_col<-c('L3mouv01','L3mouv02','L3mouv03','L3mouv04','L3mouv05','L3mouv06','L3mouv07','L3mouv08')
Lact_col<-c('L4act01','L4act02','L4act03','L4act04','L4act05','L4act06','L4act07','L4act08','L4act09','L4act10')
Lmouv<- Item_bysub[,Lmouv_col]
Lact<- Item_bysub[,Lact_col]
alpha_Lmouv0<-alpha(Lmouv,check.keys=TRUE) #0.56
alpha_Lact0<-alpha(Lact,check.keys=TRUE) #0.578 ite
alpha_Lmouv<-round(alpha(Lmouv,check.keys=TRUE)$total$raw_alpha,digit=2) 
alpha_Lact<-round(alpha(Lact,check.keys=TRUE)$total$raw_alpha,digit=2) 





#####ANALYSE DUNN 
enregistrementfaible_col<-c('L3mouv04','L3mouv07','L4act02','L4act05','L4act06')#'L1tact06','L1tact07',
recherche_col<-c('L4act01','L4act03','L4act08','L3mouv02','L3mouv06') #L1tact01','L1tact03','L2tact01'
sensibilitite_col<-c('L3mouv01','L3mouv05','L3mouv08','L4act09')#'L1tact02','L1tact04','L1tact05',
evitement_col<-c('L3mouv03','L4act08','L4act04','L4act07')#'L3mouv03','L4act08','L2tact02','L2tact03'

recherche<- Item_bysub[,recherche_col]
sensibilitite<- Item_bysub[,sensibilitite_col]
evitement<- Item_bysub[,evitement_col]
enregistrementfaible<- Item_bysub[,enregistrementfaible_col]

alpha(recherche) # 0.41
alpha(sensibilitite) #0.57
alpha(evitement) #0.61 # peut monter à0.73 si on enlève L4act08 et L3mouv03
alpha(enregistrementfaible,check.keys=TRUE)  #0.54 peut monter à 0.56


#Recherche/evitement 
comp_col0<-c('L4act01','L4act03','L4act08','L3mouv02','L3mouv06','L3mouv03','L4act04','L4act07')#'L2tact01','L2tact02','L2tact03','L1tact01','L1tact03')#
comp_col1<-c('L4act01','L4act03','L4act08','L3mouv02','L3mouv06','L3mouv03','L4act04','L4act07','L2tact01','L2tact02','L2tact03')#'L1tact01','L1tact03'
comp_col2<-c('L4act01','L4act03','L4act08','L3mouv02','L3mouv06','L3mouv03','L4act07','L1tact01','L1tact03')#,'L4act04','L2tact01','L2tact02','L2tact03'
comp_col3<-c('L4act01','L4act03','L4act08','L3mouv02','L3mouv06','L2tact01','L3mouv03','L4act04','L4act07','L2tact02','L2tact03','L1tact01','L1tact03')#
comp_col5<-c('L3mouv02','L3mouv06','L3mouv03')
comp0<- Item_bysub[,comp_col0]
comp1<- Item_bysub[,comp_col1]
comp2<- Item_bysub[,comp_col2]
comp3<- Item_bysub[,comp_col3]
comp5<- Item_bysub[,comp_col5]
alpha(comp0,check.keys=TRUE)##0.39
alpha(comp1,check.keys=TRUE)##0.56
alpha(comp2,check.keys=TRUE)##0.39
alpha(comp3,check.keys=TRUE)##0.54
alpha(comp5,check.keys=TRUE)##0.54
alpha_behav<- round(alpha(comp2,check.keys=TRUE)$total$raw_alpha,digit=2) 



comp_col4<-c('L4act08','L3mouv02','L3mouv06','L2tact01','L3mouv03','L4act04','L4act07','L2tact02','L2tact03','L1tact01')#'L4act01','L1tact03','L4act03',
comp4<- Item_bysub[,comp_col4]
alpha(comp4,check.keys=TRUE)##0.60

#Low/high sensitivity
sensi_col0<-c('L3mouv01','L3mouv05','L3mouv08','L4act09','L3mouv04','L3mouv07','L4act02','L4act05','L4act06') #'L1tact06','L1tact07','L1tact02','L1tact04','L1tact05',
sensi_col1<-c('L3mouv01','L3mouv05','L3mouv08','L4act09','L3mouv04','L3mouv07','L4act02','L4act05','L4act06','L1tact06','L1tact07','L1tact02','L1tact04','L1tact05') #
sensi_col2<-c('L3mouv01','L3mouv05','L3mouv08','L3mouv04','L3mouv07')
sensi_col3<-c('L4act09','L4act02','L4act05','L4act06')
sensi0<- Item_bysub[,sensi_col0]
sensi1<- Item_bysub[,sensi_col1]
alpha(sensi0,check.keys=TRUE) # 0.65
alpha_threshold<- round(alpha(sensi1,check.keys=TRUE)$total$raw_alpha,digit=2) 


####################Citherlet 2021 

#high threshold component combines ‘‘low registration” and ‘‘sensation seeking” items
highthreshold_col<-c('L1tact03','L1tact06','L1tact07','L3mouv02','L3mouv04','L3mouv07','L4act01','L4act02','L4act03','L4act05','L4act06')#,'L1tact01','L4act08''L2tact01','L3mouv06',
highthreshold<- Item_bysub[,highthreshold_col]
alpha(highthreshold,check.keys=TRUE) # 0.46 passage à 0.54 en suprrimant

#the low threshold component combines ‘‘sensory sensitivity” and ‘‘sensation avoiding”
lowthreshold_col<-c('L1tact02','L1tact04','L1tact05','L3mouv01','L3mouv03','L3mouv05','L3mouv08','L4act04','L4act07','L4act09','L4act08','L2tact02','L2tact03')
lowthreshold<- Item_bysub[,lowthreshold_col]
alpha(lowthreshold,check.keys=TRUE) # 0.7

#passive behavior component combines ‘‘sensory sensitivity” and ‘‘low registration” 
passive_col<-c('L3mouv01','L3mouv05','L3mouv08','L1tact02','L1tact04','L1tact05','L4act09','L3mouv04','L3mouv07','L1tact06','L1tact07','L4act02','L4act05','L4act06')
passive<- Item_bysub[,passive_col]
alpha(passive,check.keys=TRUE) # 0.69

#and the active behavior component combines ‘‘sensations avoiding” and ‘‘sensation seeking” 
active_col<-c('L1tact01','L1tact03','L4act01','L4act03','L4act08','L3mouv02','L3mouv06','L2tact01','L3mouv03','L4act04','L4act07','L4act08','L2tact02','L2tact03')
active<- Item_bysub[,active_col]
alpha(active,check.keys=TRUE) # 0.61


####################Citherlet 2021 en utilisant que les L3 mouv et L4act

#high threshold component combines ‘‘low registration” and ‘‘sensation seeking” items
highthreshold_col<-c('L3mouv02','L3mouv04','L3mouv06','L3mouv07','L4act01','L4act02','L4act03','L4act05','L4act06','L4act08')#,'L1tact01','L2tact01','L1tact03','L1tact06','L1tact07',
highthreshold<- Item_bysub[,highthreshold_col]
alpha(highthreshold,check.keys=TRUE) # 0.52

#the low threshold component combines ‘‘sensory sensitivity” and ‘‘sensation avoiding”
lowthreshold_col<-c('L3mouv01','L3mouv03','L3mouv05','L3mouv08','L4act04','L4act07','L4act09','L4act08') #'L1tact02','L1tact04','L1tact05',,'L2tact02','L2tact03'
lowthreshold<- Item_bysub[,lowthreshold_col]
alpha(lowthreshold,check.keys=TRUE) # 0.54

#passive behavior component combines ‘‘sensory sensitivity” and ‘‘low registration” 
passive_col<-c('L3mouv01','L3mouv05','L3mouv08','L3mouv04','L3mouv07','L4act09','L4act02','L4act05','L4act06')#'L1tact02','L1tact04','L1tact05','L1tact06','L1tact07',
passive<- Item_bysub[,passive_col]
alpha(passive,check.keys=TRUE) # 0.65

#and the active behavior component combines ‘‘sensations avoiding” and ‘‘sensation seeking” 
active_col<-c('L4act01','L4act03','L4act08','L3mouv02','L3mouv06','L3mouv03','L4act04','L4act07','L4act08')#'L1tact01','L1tact03','L2tact01','L2tact02','L2tact03'
active<- Item_bysub[,active_col]
alpha(active,check.keys=TRUE) # 0.49

####################Citherlet 2021 en rajoutant L1

#high threshold component combines ‘‘low registration” and ‘‘sensation seeking” items
highthreshold_col<-c('L3mouv02','L3mouv04','L3mouv06','L3mouv07','L4act01','L4act02','L4act03','L4act05','L4act06','L4act08','L1tact01','L1tact03','L1tact06','L1tact07')#,'L1tact01','L2tact01','L1tact03','L1tact06','L1tact07',
highthreshold<- Item_bysub[,highthreshold_col]
alpha(highthreshold,check.keys=TRUE) # 0.52 passage à 0.48 avec L1

#the low threshold component combines ‘‘sensory sensitivity” and ‘‘sensation avoiding”
lowthreshold_col<-c('L3mouv01','L3mouv03','L3mouv05','L3mouv08','L4act04','L4act07','L4act09','L4act08','L1tact02','L1tact04','L1tact05') #,,'L2tact02','L2tact03'
lowthreshold<- Item_bysub[,lowthreshold_col]
alpha(lowthreshold,check.keys=TRUE) # 0.54 passage à 0.63 avec L1

#passive behavior component combines ‘‘sensory sensitivity” and ‘‘low registration” 
passive_col<-c('L3mouv01','L3mouv05','L3mouv08','L3mouv04','L3mouv07','L4act09','L4act02','L4act05','L4act06','L1tact02','L1tact04','L1tact05','L1tact06','L1tact07')#
passive<- Item_bysub[,passive_col]
alpha(passive,check.keys=TRUE) # 0.65 passage à 0.69 avec L1

#and the active behavior component combines ‘‘sensations avoiding” and ‘‘sensation seeking” 
active_col<-c('L4act01','L4act03','L4act08','L3mouv02','L3mouv06','L3mouv03','L4act04','L4act07','L4act08','L1tact01','L1tact03')#'L2tact01','L2tact02','L2tact03'
active<- Item_bysub[,active_col]
alpha(active,check.keys=TRUE) # 0.49 passage à  0.49 avec L1








# Deal Missing values   ------------------------------

Measures_bysub<-read.csv("../../Share/Analyses/Measures_bysub.csv")


###Visualiser les données manquantes
vis_dat(Measures_bysub)
gg_miss_var(Measures_bysub)
gg_miss_fct(Measures_bysub, A1_sexe) 

MCAR_measure<-mcar_test(Measures_bysub)


# Imputer les valeurs manquantes avec la fonction "mice"
n_imputations=10
imputation <- mice(Measures_bysub, m= n_imputations, method = NULL, , ignore = NULL, where = NULL, , visitSequence = NULL, blots = NULL,
         post = NULL, maxit = 5, printFlag = TRUE,seed = NA, data.init = NULL)
# Accéder aux données imputées pour la première imputation
#imputed_data1 <- complete(imp, 1)
#imputed_data2 <- complete(imp, 2)

# Prends les données manquantes imputées et les rajoutes dans le dataframe original 
Measure_imputed<-complete(imputation)






###
##
# analyse lm --------------------------------
"""
library(ggeffects)
############D_cons_soi
lm1_age_D1<-lm(D1_priv ~ A3_age_m, data = Measure_Network )
lm2_age_D1<-lm(D1_priv ~ poly(A3_age_m,2), data = Measure_Network )
lm3_age_D1<-lm(D1_priv ~ poly(A3_age_m,3), data = Measure_Network )
lm4_age_D1<-lm(D1_priv ~ log(A3_age_m), data = Measure_Network )

anova(lm1_age_D1,lm2_age_D1,lm3_age_D1,lm4_age_D1)

modtoplot=ggpredict(lm2_age_D1, c('A3_age_m[all]')) 
plot(modtoplot,rawdata = TRUE)

modtoplot=ggpredict(lm4_age_D1, c('A3_age_m[all]')) 
plot(modtoplot,rawdata = TRUE)

lm1_pub_D1<-lm(D1_priv ~ B_puberte, data = Measure_Network )
lm2_pub_D1<-lm(D1_priv ~ poly(B_puberte,2), data = Measure_Network )
lm3_pub_D1<-lm(D1_priv ~ poly(B_puberte,3), data = Measure_Network )
lm4_pub_D1<-lm(D1_priv ~ log(B_puberte), data = Measure_Network )

anova(lm1_pub_D1,lm2_pub_D1,lm3_pub_D1,lm4_pub_D1)

modtoplot=ggpredict(lm2_pub_D1, c('B_puberte[all]')) 
plot(modtoplot,rawdata = TRUE)


lm2_age_D1_sexe<-lm(D1_priv ~ poly(A3_age_m,2)*A1_sexe, data = Measure_Network )
anova(lm2_age_D1,lm2_age_D1_sexe)
modtoplot=ggpredict(lm2_age_D1_sexe, c('A3_age_m[all]','A1_sexe')) 
plot(modtoplot,rawdata = TRUE)



##recherche inflexion age puberté

mod_age<-lm(D1_priv ~ poly(A3_age_m,2,raw=TRUE), data = subset(Measure_Network, A1_sexe == '1') )
modtoplot=ggpredict(mod_age, c('A3_age_m[all]')) 
plot(modtoplot,rawdata = TRUE)

sum_age<-summary(mod_age,ddf = 'Kenward-Roger')
a<-sum_age$coefficients['(Intercept)','Estimate']
b1<-sum_age$coefficients['poly(A3_age_m, 2, raw = TRUE)1','Estimate']
b2<-sum_age$coefficients['poly(A3_age_m, 2, raw = TRUE)2','Estimate']

f <- function(x) { a +(b1*x)+(b2*x^2) }
curve(f(x),xlim = c(100,300))

#calcul de la dérivé
f_prime <- function(x) { b1 + 2*b2*x }
curve(f_prime(x), xlim = c(100,300))
p_inf1_RV2_touch_age<- (uniroot(f_prime, interval = c(150,300))[[1]])/12


mod_pub<-lm(D1_priv ~ poly(B_puberte,2,raw=TRUE), data =  subset(Measure_Network, A1_sexe == '1') )
modtoplot=ggpredict(mod_pub, c('B_puberte[all]')) 
plot(modtoplot,rawdata = TRUE)

sum_pub<-summary(mod_pub,ddf = 'Kenward-Roger', raw = TRUE)
a<-sum_pub$coefficients['(Intercept)','Estimate']
b1<-sum_pub$coefficients['poly(B_puberte, 2, raw = TRUE)1','Estimate']
b2<-sum_pub$coefficients['poly(B_puberte, 2, raw = TRUE)2','Estimate']

f <- function(x) { a +(b1*x)+(b2*x^2) }
curve(f(x), xlim = c(1,5))

#calcul de la dérivé
f_prime <- function(x) { b1 + 2*b2*x }
curve(f_prime(x), xlim = c(1,5))
p_inf1_RV2_touch_pub<- uniroot(f_prime, interval = c(4,5))[[1]] 


############K6
lm1_age_K6<-lm(K6_trust ~ A3_age_m*A1_sexe, data = Measure_Network )
lm2_age_K6<-lm(K6_trust ~ poly(A3_age_m,2)*A1_sexe, data = Measure_Network )
lm3_age_K6<-lm(K6_trust ~ poly(A3_age_m,3)*A1_sexe, data = Measure_Network )
lm4_age_K6<-lm(K6_trust ~ log(A3_age_m)*A1_sexe, data = Measure_Network )

anova(lm1_age_K6,lm2_age_K6,lm3_age_K6,lm4_age_K6)



modtoplot=ggpredict(lm2_age_K6, c('A3_age_m[all]', 'A1_sexe')) 
plot(modtoplot,rawdata = TRUE)

modtoplot=ggpredict(lm3_age_K6, c('A3_age_m[all]', 'A1_sexe')) 
plot(modtoplot,rawdata = TRUE)

modtoplot=ggpredict(lm4_age_K6, c('A3_age_m[all]')) 
plot(modtoplot,rawdata = TRUE)

lm1_pub_K6<-lm(K6_trust ~ B_puberte, data = Measure_Network )
lm2_pub_K6<-lm(K6_trust ~ poly(B_puberte,2), data = Measure_Network )
lm3_pub_K6<-lm(K6_trust ~ poly(B_puberte,3), data = Measure_Network )
lm4_pub_K6<-lm(K6_trust ~ log(B_puberte), data = Measure_Network )

anova(lm1_pub_K6,lm2_pub_K6,lm3_pub_K6,lm4_pub_K6)
anova(lm3_pub_K6)

modtoplot=ggpredict(lm3_pub_K6, c('B_puberte[all]')) 
plot(modtoplot,rawdata = TRUE)


lm2_age_K6_sexe<-lm(K6_trust ~ poly(A3_age_m,2)*A1_sexe, data = Measure_Network )
anova(lm2_age_K6,lm2_age_K6_sexe)
modtoplot=ggpredict(lm2_age_K6_sexe, c('A3_age_m[all]','A1_sexe')) 
plot(modtoplot,rawdata = TRUE)

"""










# Topological overlap analysis ---------------------------------

##### on data imputed for missing values


gb_dataset<-goldbricker(
  Measure_imputed,
  p = 0.05,
  method = "hittner2003",
  threshold = 0.30,
  corMin = 0.5,
  progressbar = TRUE)

Measure_Network0 <- net_reduce(data=Measure_imputed, badpairs=gb_dataset,method=c("best_goldbricker"))

gb_red_nodes1<-goldbricker(
  Measure_Network0,
  p = 0.05,
  method = "hittner2003",
  threshold = 0.30,
  corMin = 0.5,
  progressbar = TRUE)


if (gb_red_nodes1$suggested_reductions == "No suggested reductions") {
  Measure_Network <- Measure_Network0
} else {
  Measure_Network <- net_reduce(data = Measure_Network0, badpairs = gb_red_nodes1, method = c("best_goldbricker"))
  
  gb_red_nodes2<-goldbricker(
    Measure_Network,
    p = 0.05,
    method = "hittner2003",
    threshold = 0.30,
    corMin = 0.5,
    progressbar = TRUE)
  
}

supprimer_overlap<-setdiff(names(Measure_imputed), names(Measure_Network))


##Remmetre les mesures réduite au bon endroit
Measure_Network<-Measure_Network %>% select(order(colnames(Measure_Network)))

####utiliser dans raw seulement les colonnes qui reste après overlap
Measure_Network_col<- colnames(Measure_Network)
Measure_Network_raw <- Measures_bysub[, intersect(Measure_Network_col, colnames(Measures_bysub))]


"""
NOrmalement doit donner no suggested reductio 
gb_red_nodes1_raw<-goldbricker(
  Measure_Network_raw,
  p = 0.05,
  method = 'hittner2003',
  threshold = 0.25,
  corMin = 0.5,
  progressbar = TRUE)

"""


# Noms--------------------------------

####Renommers measures

names_list_pairs<- c("A1_sexe","Sex","Sex ", 'General',
                      "A2_diff", "Cisgender", "Cis", 'General',
                      "A3_age_m","Age", "Age ", 'General',
                      "A6_IMC", "BMI",  "BMI ", 'General',     
                      "B_puberte", "Puberty","Pub",'General',
                      "C_AP",    "Physical_Activity","P-A", 'Physical Activity ',
                      "D1_priv",   "Private_Self-Consciousness","PriSC", 'Self Consciousness',
                      "D2_pub", "Public_Self-Consciousness",  "PubSC",  'Self Consciousness',
                      "D3_anx_soc","Social_Anxiety", "SocAnx",'Self Consciousness',
                      "E1_perspec", "Perspective_Taking","PerTak", 'Interpersonal Reactivity',
                      "E2_empat", "Empathic_Concern",    "Emp", 'Interpersonal Reactivity',
                      "G1_comp",  "Behavioral_engagement_in_Social_Media", "BehSM",  'Social Media Engagement ', 
                      "G2_cog", "Cognitive_engagement_in_Social_Media", "CogSM", 'Social Media Engagement ',
                      "G3_affec",   "Affective_engagement_in_Social_Media",  "AffSM", 'Social Media Engagement ',
                      "H1_appa",  "Body_Esteem_for_Appearance",  "ApBE",'Body Esteem',
                      "H2_attri","Body_Esteem_Attribution",  "AtBE",   'Body Esteem',
                      "H3_poids","Body_Esteem_for_Weight_satisfaction",   "WeiBE",  'Body Esteem',
                      "I_objectifi","Body_Objectification", "BoObj", 'Body Esteem',
                      "II1_fam", "Family_Pressure",   "FamPree",  'Sociocultural Attitudes',
                      "II2_pairs",  "Peers_Pressure", "PeePres",'Sociocultural Attitudes',
                      "II3_reseau","Media_Pressure", "MedPres", 'Sociocultural Attitudes',
                      "J_cons_corps",'Private_Body_consciousness', "PriBC",'Multidimensional Interoception',
                      "K1_notice", "Noticing_body_sensations","NotiB",'Multidimensional Interoception',
                      "K2_nodistract",   "Not-Distracting", "NoDiB",'Multidimensional Interoception',
                      "K3_notworry","Not-Worrying","NoWoB",'Multidimensional Interoception',
                      "K4_emotion", "Emotional_Awareness", "EmoB",'Multidimensional Interoception',
                      "K5_listing","Body_Listening",  "ListB",'Multidimensional Interoception',
                      "K6_trust",  "Body_Trusting", "TrustB", 'Multidimensional Interoception',
                      "L2_tact_soc","Social_touch",  "SoTou","Social Touch",
                      "L_threshold" ,"Sensitivy Threshold","Sensi", "Sensory Processing",
                       "L_behav" ,"Sensitivy Behavior","Behav", "Sensory Processing",
                      "M1_douleurs_nb" , "Chronic_Pain",  "CPain", "Chronic Pain",
                      "N_influence_pairs","Resistance_to_Peer_Influence","ResPeer", "Resistance to Peer Influence", 
                      "sit_eco", "Economic_Status","Eco", "Economic Status")

for (col in names(Measure_Network)) {
  index <- which(names_list_pairs == col)[1]
  index_new<-(index+1)
  new_name<-names_list_pairs[index_new]
  colnames(Measure_Network)[which(names(Measure_Network) == col)] <- new_name
  }

for (col in names(Measure_Network_raw)) {
 index <- which(names_list_pairs == col)[1]
  index_new<-(index+1)
  new_name<-names_list_pairs[index_new]
  colnames(Measure_Network_raw)[which(names(Measure_Network_raw) == col)] <- new_name
}


#pour savoir automatiquement quelle mesure ont été supprimer 
supp_overlap_list<-list()
for (measure in supprimer_overlap) {
  index <- which(names_list_pairs == measure)[1]
  index_new<-(index+1)
  mesure_name<-names_list_pairs[index_new]
 supp_overlap_list <- append(supp_overlap_list, mesure_name[1])
}


# WHOLE net Age -------------------

# * Whole Age -------------------

###SUPPRESSION DES MESURES POUR NETWORK!!!!!!!!!!!!!!

Measure_final_age<-subset(Measure_Network, select = - c(Puberty))
names(Measure_final_age)

Measure_final_raw_age<-subset(Measure_Network_raw, select = - c(Puberty))
names(Measure_final_raw_age)


### POUR OBTENIR GR1 index et nom de group

longnamesnodes_age<-names(Measure_final_age)
shortnames_nodes_age<- list()
for (col in names(Measure_final_age)) {
  index <- which(names_list_pairs == col)[1]
  index_new<-(index+1)
  new_name<-names_list_pairs[index_new][1]
  shortnames_nodes_age<-append(shortnames_nodes_age, new_name[1])
}

names_groups_tot_age<- list()
for (col in names(Measure_final_age)) {
  index <- which(names_list_pairs == col)[1]
  index_new<-(index+2)
  new_name<-names_list_pairs[index_new][1]
  names_groups_tot_age<-append(names_groups_tot_age, new_name[1])
  }

names_groups_unique_age<-unique(names_groups_tot_age)
grSub1_age <- split(x = 1:length(names_groups_tot_age), f = unlist(names_groups_tot_age))


### Make correlation matrix
cor_tot_age<-cor(Measure_final_age) #compute the correlation Matrix
Measure_final_age.cor<-cor_auto(Measure_final_age) #compute the correlation Matrix with qgraph Packages 

cor_tot_raw_age<-cor(Measure_final_raw_age) #compute the correlation Matrix
Measure_final_age.cor_raw<-cor_auto(Measure_final_raw_age) #compute the correlation Matrix with qgraph Packages 

##### Network analysis


png("Figures_Quest/Network_Whole_imputed_age.png", width=2000, height=1400)
graph_imputed_age<-qgraph(Measure_final_age.cor, graph="glasso", layout="spring",labels=shortnames_nodes_age,
                 vsize=7, cut=0, maximum=.45, sampleSize = nrow(Measure_final_age),
                 border.width=0.1, border.color="#a8a8a8", minimum=.03, 
                 groups=grSub1_age, color=c("#377eb8", "#fb9a99", "#4daf4a", "#ffffbf", "#ff7f00", "#ffff33", "#bcf60c", "#c77cff", "#a65628", 
                                        "#66c2a5", "#fc8d62", "#46f0f0", "#e78ac3"),title= "Whole sample Network",
                 legend=TRUE,legend.mode='style1',GLratio=2.5,layoutScale=1,legend.cex=0.8,
                 nodeNames=longnamesnodes_age)
dev.off()

Layeoutimputed<-averageLayout(graph_imputed_age)

png("Figures_Quest/Network_Whole_raw_age.png", width=2000, height=1400)
graph_raw_age<-qgraph(Measure_final_age.cor_raw, graph="glasso", layout=Layeoutimputed,labels=shortnames_nodes_age,
                       vsize=7, cut=0, maximum=.45, sampleSize = nrow(Measure_final_age),
                       border.width=0.1, border.color="#a8a8a8", minimum=.03, 
                       groups=grSub1_age, color=c("#377eb8", "#fb9a99", "#4daf4a", "#ffffbf", "#ff7f00", "#ffff33", "#bcf60c", "#c77cff", "#a65628", 
                                              "#66c2a5", "#fc8d62", "#46f0f0", "#e78ac3"),title= "Whole sample Network",
                       legend=TRUE,legend.mode='style1',GLratio=2.5,layoutScale=1,legend.cex=0.8,
                       nodeNames=longnamesnodes_age)
dev.off()


### Centrality analysis

png("Figures_Quest/Centrality_Whole_imputed_age.png", width=2000, height=1400)
centrality_imputed<-centralityPlot(graph_imputed_age,include="All",orderBy = "Betweenness")
dev.off()

png("Figures_Quest/Centrality_Whole_raw_age.png", width=2000, height=1400)
centrality_raw<-centralityPlot(graph_raw_age,include="All",orderBy = "Betweenness")
dev.off()



# * Whole Pub -------------------

###SUPPRESSION DES MESURES POUR NETWORK!!!!!!!!!!!!!!

Measure_final_pub<-subset(Measure_Network, select = - c(Age))
names(Measure_final_pub)

Measure_final_pub_raw<-subset(Measure_Network_raw, select = - c(Age))
names(Measure_final_pub_raw)


### POUR OBTENIR GR1 index et nom de group

longnamesnodes_pub<-names(Measure_final_pub)
shortnames_nodes_pub<- list()
for (col in names(Measure_final_pub)) {
  index <- which(names_list_pairs == col)[1]
  index_new<-(index+1)
  new_name<-names_list_pairs[index_new][1]
  shortnames_nodes_pub<-append(shortnames_nodes_pub, new_name[1])
}

names_groups_tot_pub<- list()
for (col in names(Measure_final_pub)) {
  index <- which(names_list_pairs == col)[1]
  index_new<-(index+2)
  new_name<-names_list_pairs[index_new][1]
  names_groups_tot_pub<-append(names_groups_tot_pub, new_name[1])
}

names_groups_unique_pub<-unique(names_groups_tot_pub)
grSub1_pub <- split(x = 1:length(names_groups_tot_pub), f = unlist(names_groups_tot_pub))


### Make correlation matrix
cor_tot_pub<-cor(Measure_final_pub) #compute the correlation Matrix
Measure_final_pub.cor<-cor_auto(Measure_final_pub) #compute the correlation Matrix with qgraph Packages 

cor_tot_raw_pub<-cor(Measure_final_pub_raw) #compute the correlation Matrix
Measure_final_pub.cor_raw<-cor_auto(Measure_final_pub_raw) #compute the correlation Matrix with qgraph Packages 

##### Network analysis

png("Figures_Quest/Network_Whole_imputed_pub.png", width=2000, height=1400)
graph_imputed<-qgraph(Measure_final_pub.cor, graph="glasso", layout=Layeoutimputed,labels=shortnames_nodes_pub,
                      vsize=7, cut=0, maximum=.45, sampleSize = nrow(Measure_final_pub),
                      border.width=0.1, border.color="#a8a8a8", minimum=.03, 
                      groups=grSub1_pub, color=c("#377eb8", "#fb9a99", "#4daf4a", "#ffffbf", "#ff7f00", "#ffff33", "#bcf60c", "#c77cff", "#a65628", 
                                             "#66c2a5", "#fc8d62", "#46f0f0", "#e78ac3"),title= "Whole sample Network",
                      legend=TRUE,legend.mode='style1',GLratio=2.5,layoutScale=1,legend.cex=0.8,
                      nodeNames=longnamesnodes_pub)
dev.off()


png("Figures_Quest/Network_Whole_raw_pub.png", width=2000, height=1400)
graph_raw<-qgraph(Measure_final_pub.cor_raw, graph="glasso", layout=Layeoutimputed,labels=shortnames_nodes_pub,
                  vsize=7, cut=0, maximum=.45, sampleSize = nrow(Measure_final_pub),
                  border.width=0.1, border.color="#a8a8a8", minimum=.03, 
                  groups=grSub1_pub, color=c("#377eb8", "#fb9a99", "#4daf4a", "#ffffbf", "#ff7f00", "#ffff33", "#bcf60c", "#c77cff", "#a65628", 
                                         "#66c2a5", "#fc8d62", "#46f0f0", "#e78ac3"),title= "Whole sample Network",
                  legend=TRUE,legend.mode='style1',GLratio=2.5,layoutScale=1,legend.cex=0.8,
                  nodeNames=longnamesnodes_pub)
dev.off()


### Centrality analysis

png("Figures_Quest/Centrality_Whole_imputed.png", width=2000, height=1400)
centrality_imputed<-centralityPlot(graph_imputed,include="All",orderBy = "Betweenness")
dev.off()

png("Figures_Quest/Centrality_Whole_raw.png", width=2000, height=1400)
centrality_raw<-centralityPlot(graph_raw,include="All",orderBy = "Betweenness")
dev.off()







# SEX comparaison  -------------------

############# division analyse par sex 

Measure_Fem_0<- subset(Measure_final_age[Measure_final_age$Sex == 1, ], select = - c(Sex,Cisgender))
Measure_Mal_0<- subset(Measure_final_age[Measure_final_age$Sex == 2, ], select = - c(Sex,Cisgender))

#Measure_final_age_col<- colnames(Measure_final_age)
#Measure_Fem_0 <- Measure_Fem[, intersect(Measure_final_age_col, colnames(Measure_Fem))]
#Measure_Mal_0 <- Measure_Mal[, intersect(Measure_final_age_col, colnames(Measure_Mal))]

# * Sex Check de l'overlap --------------

Measure_Mal<-Measure_Mal_0

gb_dataset_mal <- data.frame(suggested_reductions = character(), stringsAsFactors = FALSE)
nouvelle_ligne <- data.frame(suggested_reductions = "0", stringsAsFactors = FALSE)# Ajouter une ligne avec une valeur spécifique dans la colonne 'suggested_reductions'
gb_dataset_mal <- rbind(gb_dataset_mal, nouvelle_ligne)

#gb_dataset_mal$suggested_reductions <- ""

while (gb_dataset_mal$suggested_reductions[1] != "No suggested reductions") {
  gb_dataset_mal <- goldbricker(
    Measure_Mal,
    p = 0.05,
    method = "hittner2003",
    threshold = 0.30,
    corMin = 0.5,
    progressbar = TRUE
      )
  # Réduire le réseau avec les nouvelles suggestions de réduction
  
  if (gb_dataset_mal$suggested_reductions[1] == "No suggested reductions") {
    break  # Sortir de la boucle
  }
  
    Measure_Mal <- net_reduce(data = Measure_Mal, badpairs = gb_dataset_mal, method = c("best_goldbricker"))

}



supprimer_overlap_mal<-setdiff(names(Measure_Mal_0), names(Measure_Mal))

Measure_Network_sex_col<- colnames(Measure_Mal)
Measure_Fem <- Measure_Fem_0[, intersect(Measure_Network_sex_col, colnames(Measure_Fem_0))]

###label sex
nodenames_sex<-names(Measure_Fem)
labels_sex<- list()

for (col in nodenames_sex) {
  index <- which(names_list_pairs == col)[1]
  index_new<-(index+1)
  new_name<-names_list_pairs[index_new][1]
  labels_sex<-append(labels_sex, new_name[1])
}

names_groups_sex<- list()
for (col in nodenames_sex) {
  index <- which(names_list_pairs == col)[1]
  index_new<-(index+2)
  new_name<-names_list_pairs[index_new][1]
  names_groups_sex<-append(names_groups_sex, new_name[1])
}

names_groups_unique<-unique(names_groups_sex)
gr_sexe <- split(x = 1:length(names_groups_sex), f = unlist(names_groups_sex))


# * Sex Network --------------
Measure_Fem.cor<-cor_auto(Measure_Fem) #compute the correlation Matrix with qgraph Packages 
Measure_Mal.cor<-cor_auto(Measure_Mal) #compute the correlation Matrix with qgraph Packages 

png("Figures_Quest/Network_fem.png", width=2000, height=1400)
graphFem<-qgraph(Measure_Fem.cor, graph="glasso", layout="spring",labels=labels_sex,
                  maximum=.45,minimum=.03,tuning=0.25,
                  vsize=7, cut=0,  sampleSize = nrow(Measure_Fem),
                  border.width=0.1, border.color="#a8a8a8",  
                  groups=gr_sexe, color=c("#377eb8", "#fb9a99", "#4daf4a", "#ffffbf", "#ff7f00", "#ffff33", "#bcf60c", "#c77cff", "#a65628", 
                                           "#66c2a5", "#fc8d62", "#46f0f0", "#e78ac3"),title= "Whole sample Network",
                  legend=TRUE,legend.mode='style1',GLratio=2.5,layoutScale=1,legend.cex=0.8,
                  nodeNames=nodenames_sex)
dev.off()

layout_fem<-averageLayout(graphFem)

png("Figures_Quest/Network_mal.png", width=2000, height=1400)
graphMal<-qgraph(Measure_Mal.cor, graph="glasso", layout=layout_fem,labels=labels_sex,
                  maximum=.45,minimum=.03,tuning=0.25,
                  vsize=7, cut=0,sampleSize = nrow(Measure_Mal),
                  border.width=0.1, border.color="#a8a8a8", 
                  groups=gr_sexe, color=c("#377eb8", "#fb9a99", "#4daf4a", "#ffffbf", "#ff7f00", "#ffff33", "#bcf60c", "#c77cff", "#a65628", 
                                           "#66c2a5", "#fc8d62", "#46f0f0", "#e78ac3"),title= "Whole sample Network",
                  legend=TRUE,legend.mode='style1',GLratio=2.5,layoutScale=1,legend.cex=0.8,
                  nodeNames=nodenames_sex)
dev.off()

# * Sex NCT  --------------


NCTFemvsMal_fdr<-NCT(Measure_Fem, Measure_Mal, 
                   it = 1000, # The number of iterations (permutations).
                   binary.data=FALSE, 
                   paired=FALSE, 
                   weighted=TRUE, 
                   abs=TRUE,
                   test.edges=TRUE, 
                   edges="all", 
                   progressbar=TRUE, 
                   make.positive.definite=TRUE,
                   p.adjust.methods= c("fdr"), #,"holm","hochberg","hommel", "bonferroni","BH","BY","fdr"), 
                   test.centrality=TRUE, 
                   centrality=c("betweenness","strength"), #'betweenness', 'strength', 'expectedInfluence', 'bridgeStrength', 'bridgeCloseness', 'bridgeBetweenness', 'bridgeExpectedInfluence'
                   nodes="all",
                   communities=gr_sexe,
                   useCommunities="all",
                   #estimator,
                   #estimatorArgs = list(), 
                   verbose = TRUE)




p_diffstrenght_fdr <- NCTFemvsMal_fdr$glstrinv.pval # 	 The p value resulting from the permutation test concerning difference in global strength.

p_globaledgeweight_fdr <- NCTFemvsMal_fdr$nwinv.pval  # The p value resulting from the permutation test concerning the maximum difference in edge weights.

p_alledgeweight_fdr <- NCTFemvsMal_fdr$einv.pvals # p-values (corrected for multiple testing or not according to 'p.adjust.methods') per edge from the permutation test concerning differences in edges weights
p_alledgeweight_fdr <- as.data.frame(p_alledgeweight_fdr)
names(p_alledgeweight_fdr)[3]<- 'pval'
edgeweight_signi_fdr <- p_alledgeweight_fdr %>% dplyr::filter(pval < 0.050)


p_allcent_fdr <- NCTFemvsMal_fdr$diffcen.pval #	p-values(corrected for multiple testing or not according to 'p.adjust.methods') per node from the permutation test concerning differences in centralities. Only if test.centrality = TRUE.
p_allcent_fdr<- as.data.frame(p_allcent_fdr)

diffbetwee_signi_fdr_p<-rownames(p_allcent_fdr%>% dplyr::filter(betweenness  < 0.050 ))
diffstren_cent_signi_fdr_p<-rownames(p_allcent_fdr%>% dplyr::filter(strength < 0.050))


diffbetwee_signi_fdr <- subset(NCTFemvsMal_fdr$diffcen.real, row.names(NCTFemvsMal_fdr$diffcen.real) %in% diffbetwee_signi_fdr_p)
diffstren_cent_signi_fdr <- subset(NCTFemvsMal_fdr$diffcen.real, row.names(NCTFemvsMal_fdr$diffcen.real) %in% diffstren_cent_signi_fdr_p)











NCTFemvsMal_unco<-NCT(Measure_Fem, Measure_Mal, 
                     it = 1000, # The number of iterations (permutations).
                     binary.data=FALSE, 
                     paired=FALSE, 
                     weighted=TRUE, 
                     abs=TRUE,
                     test.edges=TRUE, 
                     edges="all", 
                     progressbar=TRUE, 
                     make.positive.definite=TRUE,
                     p.adjust.methods= c("none"), #,"holm","hochberg","hommel", "bonferroni","BH","BY","fdr"), 
                     test.centrality=TRUE, 
                     centrality=c("betweenness","strength"), #'betweenness', 'strength', 'expectedInfluence', 'bridgeStrength', 'bridgeCloseness', 'bridgeBetweenness', 'bridgeExpectedInfluence'
                     nodes="all",
                     communities=gr_sexe,
                     useCommunities="all",
                     #estimator,
                     #estimatorArgs = list(), 
                     verbose = TRUE)


### test diff global strength.
p_diffstrenght_unco <- NCTFemvsMal_unco$glstrinv.pval # 	 

###test diff maximum difference in edge weights.
p_globaledgeweight_unco <- NCTFemvsMal_unco$nwinv.pval  # The p value resulting from the permutation test concerning the maximum difference in edge weights.


###test & trouver diff all edge weights.

p_alledgeweight_unco <- NCTFemvsMal_unco$einv.pvals # p-values (corrected for multiple testing or not according to 'p.adjust.methods') per edge from the permutation test concerning differences in edges weights
p_alledgeweight_unco<- as.data.frame(p_alledgeweight)
names(p_alledgeweight_unco)[3]<- 'pval'
edgeweight_signi_unco_p <- p_alledgeweight_unco %>% dplyr::filter(pval < 0.050)


NCTFemvsMal_unco$einv.real	

###trouver diffférence de centralité
p_allcent_unco <- NCTFemvsMal_unco$diffcen.pval #	p-values(corrected for multiple testing or not according to 'p.adjust.methods') per node from the permutation test concerning differences in centralities. Only if test.centrality = TRUE.
p_allcent_unco<- as.data.frame(p_allcent_unco)
diffbetwee_signi_unco_p<-rownames(p_allcent_unco%>% dplyr::filter(betweenness  < 0.050 ))
diffstren_cent_signi_unco_p<-rownames(p_allcent_unco%>% dplyr::filter(strength < 0.050))


diffbetwee_signi_unco <- subset(NCTFemvsMal_unco$diffcen.real, row.names(NCTFemvsMal_unco$diffcen.real) %in% diffbetwee_signi_unco_p)
diffstren_cent_signi_unco <- subset(NCTFemvsMal_unco$diffcen.real, row.names(NCTFemvsMal_unco$diffcen.real) %in% diffstren_cent_signi_unco_p)







# PUB comparaison  -------------------

Measure_final_pub<-subset(Measure_Network, select = - c(Age))

cut_points <- quantile(Measure_final_pub$Puberty, probs = seq(0, 1, 0.25))

Pub1 <- Measure_final_pub[Measure_final_pub$Puberty <= cut_points[[2]], ]
Pub2 <- Measure_final_pub[Measure_final_pub$Puberty > cut_points[[2]] & Measure_final_pub$Puberty <= cut_points[[3]],]
Pub3 <- Measure_final_pub[Measure_final_pub$Puberty > cut_points[[3]] & Measure_final_pub$Puberty <= cut_points[[4]],]
Pub4 <- Measure_final_pub[Measure_final_pub$Puberty  > cut_points[[4]]& Measure_final_pub$Puberty <= cut_points[[5]],]



Pub1<-subset(Pub1, select = - c(Puberty))
Pub2<-subset(Pub2, select = - c(Puberty))
Pub3<-subset(Pub3, select = - c(Puberty))
Pub4<-subset(Pub4, select = - c(Puberty))




gb_dataset_Pub1 <- goldbricker(
  Pub1,
  p = 0.05,
  method = "hittner2003",
  threshold = 0.30,
  corMin = 0.5,
  progressbar = TRUE
)

gb_dataset_Pub4 <- goldbricker(
  Pub4,
  p = 0.05,
  method = "hittner2003",
  threshold = 0.30,
  corMin = 0.5,
  progressbar = TRUE
)





# * NCT Puberty  -------------


NCTPub1vsPub2<-NCT(Pub1, Pub2, 
                   it = 1000, # The number of iterations (permutations).
                   binary.data=FALSE, 
                   paired=FALSE, 
                   weighted=TRUE, 
                   abs=TRUE,
                   test.edges=TRUE, 
                   edges="all", 
                   progressbar=TRUE, 
                   make.positive.definite=TRUE,
                   p.adjust.methods= c("none"), #,"holm","hochberg","hommel", "bonferroni","BH","BY","fdr"), 
                   test.centrality=TRUE, 
                   centrality=c("betweenness","strength"), #'betweenness', 'strength', 'expectedInfluence', 'bridgeStrength', 'bridgeCloseness', 'bridgeBetweenness', 'bridgeExpectedInfluence'
                   nodes="all",
                   communities=gr3,
                   useCommunities="all",
                   #estimator,
                   #estimatorArgs = list(), 
                   verbose = TRUE)



p_diffstrenght_Pub1vs2 <- NCTPub1vsPub2$glstrinv.pval # 	 The p value resulting from the permutation test concerning difference in global strength.

p_globaledgeweight_Pub1vs2  <- NCTPub1vsPub2$nwinv.pval  # The p value resulting from the permutation test concerning the maximum difference in edge weights.

p_alledgeweight_Pub1vs2  <- NCTPub1vsPub2$einv.pvals # p-values (corrected for multiple testing or not according to 'p.adjust.methods') per edge from the permutation test concerning differences in edges weights
p_alledgeweight_Pub1vs2  <- as.data.frame(p_alledgeweight_Pub1vs2 )
names(p_alledgeweight_Pub1vs2 )[3]<- 'pval'
edgeweight_signi_Pub1vs2  <- p_alledgeweight_Pub1vs2  %>% dplyr::filter(pval < 0.050)


p_allcent_Pub1vs2  <- NCTPub1vsPub2$diffcen.pval #	p-values(corrected for multiple testing or not according to 'p.adjust.methods') per node from the permutation test concerning differences in centralities. Only if test.centrality = TRUE.
p_allcent_Pub1vs2 <- as.data.frame(p_allcent_Pub1vs2 )


diffbetwee_signi_fdr_p<-rownames(p_allcent_fdr%>% dplyr::filter(betweenness  < 0.050 ))
diffstren_cent_signi_fdr_p<-rownames(p_allcent_fdr%>% dplyr::filter(strength < 0.050))


diffbetwee_signi_fdr <- subset(NCTFemvsMal_fdr$diffcen.real, row.names(NCTFemvsMal_fdr$diffcen.real) %in% diffbetwee_signi_fdr_p)
diffstren_cent_signi_fdr <- subset(NCTFemvsMal_fdr$diffcen.real, row.names(NCTFemvsMal_fdr$diffcen.real) %in% diffstren_cent_signi_fdr_p)








NCTPub2vsPub3<-NCT(Pub2, Pub3, 
                   it = 1000, # The number of iterations (permutations).
                   binary.data=FALSE, 
                   paired=FALSE, 
                   weighted=TRUE, 
                   abs=TRUE,
                   test.edges=TRUE, 
                   edges="all", 
                   progressbar=TRUE, 
                   make.positive.definite=TRUE,
                   p.adjust.methods= c("none"), #,"holm","hochberg","hommel", "bonferroni","BH","BY","fdr"), 
                   test.centrality=TRUE, 
                   centrality=c("betweenness","strength"), #'betweenness', 'strength', 'expectedInfluence', 'bridgeStrength', 'bridgeCloseness', 'bridgeBetweenness', 'bridgeExpectedInfluence'
                   nodes="all",
                   communities=gr3,
                   useCommunities="all",
                   #estimator,
                   #estimatorArgs = list(), 
                   verbose = TRUE)


NCTPub3vsPub4<-NCT(Pub3, Pub4, 
                   it = 1000, # The number of iterations (permutations).
                   binary.data=FALSE, 
                   paired=FALSE, 
                   weighted=TRUE, 
                   abs=TRUE,
                   test.edges=TRUE, 
                   edges="all", 
                   progressbar=TRUE, 
                   make.positive.definite=TRUE,
                   p.adjust.methods= c("none"), #,"holm","hochberg","hommel", "bonferroni","BH","BY","fdr"), 
                   test.centrality=TRUE, 
                   centrality=c("betweenness","strength"), #'betweenness', 'strength', 'expectedInfluence', 'bridgeStrength', 'bridgeCloseness', 'bridgeBetweenness', 'bridgeExpectedInfluence'
                   nodes="all",
                   communities=gr3,
                   useCommunities="all",
                   #estimator,
                   #estimatorArgs = list(), 
                   verbose = TRUE)


NCTPub1vsPub3<-NCT(Pub1, Pub3, 
                   it = 1000, # The number of iterations (permutations).
                   binary.data=FALSE, 
                   paired=FALSE, 
                   weighted=TRUE, 
                   abs=TRUE,
                   test.edges=TRUE, 
                   edges="all", 
                   progressbar=TRUE, 
                   make.positive.definite=TRUE,
                   p.adjust.methods= c("none"), #,"holm","hochberg","hommel", "bonferroni","BH","BY","fdr"), 
                   test.centrality=TRUE, 
                   centrality=c("betweenness","strength"), #'betweenness', 'strength', 'expectedInfluence', 'bridgeStrength', 'bridgeCloseness', 'bridgeBetweenness', 'bridgeExpectedInfluence'
                   nodes="all",
                   communities=gr3,
                   useCommunities="all",
                   #estimator,
                   #estimatorArgs = list(), 
                   verbose = TRUE)



NCTPub2vsPub4<-NCT(Pub2, Pub4, 
                   it = 1000, # The number of iterations (permutations).
                   binary.data=FALSE, 
                   paired=FALSE, 
                   weighted=TRUE, 
                   abs=TRUE,
                   test.edges=TRUE, 
                   edges="all", 
                   progressbar=TRUE, 
                   make.positive.definite=TRUE,
                   p.adjust.methods= c("none"), #,"holm","hochberg","hommel", "bonferroni","BH","BY","fdr"), 
                   test.centrality=TRUE, 
                   centrality=c("betweenness","strength"), #'betweenness', 'strength', 'expectedInfluence', 'bridgeStrength', 'bridgeCloseness', 'bridgeBetweenness', 'bridgeExpectedInfluence'
                   nodes="all",
                   communities=gr3,
                   useCommunities="all",
                   #estimator,
                   #estimatorArgs = list(), 
                   verbose = TRUE)



































nodenamesfemages<-names(Fem1)
labels_femage<- list()
for (col in names(Fem1)) {
  index <- which(names_list_pairs == col)[1]
  index_new<-(index+1)
  new_name<-names_list_pairs[index_new][1]
  labels_femage<-append(labels_femage, new_name[1])
}

names_groups_tot<- list()
names_groups_unique<- list()

for (col in names(Fem1)) {
  index <- which(names_list_pairs == col)[1]
  index_new<-(index+2)
  new_name<-names_list_pairs[index_new][1]
  names_groups_tot<-append(names_groups_tot, new_name[1])
}

names_groups_unique<-unique(names_groups_tot)
grfemage <- split(x = 1:length(names_groups_tot), f = unlist(names_groups_tot))


png("Figures_Quest/graphFem1.png", width=2000, height=1400)
graphFem1<-qgraph(Fem1.cor, graph="glasso", layout="spring",labels=labels_femage,
                  vsize=7, cut=0, maximum=.45, sampleSize = nrow(Fem1),
                  border.width=0.1, border.color="#a8a8a8", minimum=.03, 
                  groups=grfemage, color=c("#377eb8", "#fb9a99", "#4daf4a", "#ffffbf", "#ff7f00", "#ffff33", "#bcf60c", "#c77cff", "#a65628", 
                                           "#66c2a5", "#fc8d62", "#46f0f0", "#e78ac3"),title= "Whole sample Network",
                  legend=TRUE,legend.mode='style1',GLratio=2.5,layoutScale=1,legend.cex=0.8,
                  nodeNames=nodenamesfemages)
dev.off()

png("Figures_Quest/graphFem2.png", width=2000, height=1400)
graphFem2<-qgraph(Fem2.cor, graph="glasso", layout="spring",labels=labels_femage,
                  vsize=7, cut=0, maximum=.45, sampleSize = nrow(Fem2),
                  border.width=0.1, border.color="#a8a8a8", minimum=.03, 
                  groups=grfemage, color=c("#377eb8", "#fb9a99", "#4daf4a", "#ffffbf", "#ff7f00", "#ffff33", "#bcf60c", "#c77cff", "#a65628", 
                                           "#66c2a5", "#fc8d62", "#46f0f0", "#e78ac3"),title= "Whole sample Network",
                  legend=TRUE,legend.mode='style1',GLratio=2.5,layoutScale=1,legend.cex=0.8,
                  nodeNames=nodenamesfemages)
dev.off()
































# FEM & AGE -------------------------


# DIFF AGE fem


cut_points <- quantile(Measure_Fem_0$Age, probs = seq(0, 1, 0.20))

Fem1 <- Measure_Network[Measure_Network$Age <= cut_points[[2]], ]
Fem2 <- Measure_Network[Measure_Network$Age > cut_points[[2]] & Measure_Network$Age <= cut_points[[3]],]
Fem3 <- Measure_Network[Measure_Network$Age > cut_points[[3]] & Measure_Network$Age <= cut_points[[4]],]
Fem4<- Measure_Network[Measure_Network$Age  > cut_points[[4]]& Measure_Network$Age <= cut_points[[5]],]
Fem5<- Measure_Network[Measure_Network$Age >= cut_points[[5]],]


Fem1<-subset(Fem1, select = - c(Age,Puberty,Cisgender))
Fem1.cor<-cor_auto(Fem1) #compute the correlation Matrix with qgraph Packages 

Fem2<-subset(Fem2, select = - c(Age,Puberty,Cisgender))
Fem2.cor<-cor_auto(Fem2) #compute the correlation Matrix with qgraph Packages 

Fem3<-subset(Fem3, select = - c(Age,Puberty,Cisgender))
Fem3.cor<-cor_auto(Fem3) #compute the correlation Matrix with qgraph Packages 

Fem4<-subset(Fem4, select = - c(Age,Puberty,Cisgender))
Fem4.cor<-cor_auto(Fem4) #compute the correlation Matrix with qgraph Packages 

Fem5<-subset(Fem5, select = - c(Age,Puberty,Cisgender))
Fem5.cor<-cor_auto(Fem5) #compute the correlation Matrix with qgraph Packages 


nodenamesfemages<-names(Fem1)
labels_femage<- list()
for (col in names(Fem1)) {
  index <- which(names_list_pairs == col)[1]
  index_new<-(index+1)
  new_name<-names_list_pairs[index_new][1]
  labels_femage<-append(labels_femage, new_name[1])
}

names_groups_tot<- list()
names_groups_unique<- list()

for (col in names(Fem1)) {
  index <- which(names_list_pairs == col)[1]
  index_new<-(index+2)
  new_name<-names_list_pairs[index_new][1]
  names_groups_tot<-append(names_groups_tot, new_name[1])
}

names_groups_unique<-unique(names_groups_tot)
grfemage <- split(x = 1:length(names_groups_tot), f = unlist(names_groups_tot))


png("Figures_Quest/graphFem1.png", width=2000, height=1400)
graphFem1<-qgraph(Fem1.cor, graph="glasso", layout="spring",labels=labels_femage,
                  vsize=7, cut=0, maximum=.45, sampleSize = nrow(Fem1),
                  border.width=0.1, border.color="#a8a8a8", minimum=.03, 
                  groups=grfemage, color=c("#377eb8", "#fb9a99", "#4daf4a", "#ffffbf", "#ff7f00", "#ffff33", "#bcf60c", "#c77cff", "#a65628", 
                                         "#66c2a5", "#fc8d62", "#46f0f0", "#e78ac3"),title= "Whole sample Network",
                  legend=TRUE,legend.mode='style1',GLratio=2.5,layoutScale=1,legend.cex=0.8,
                  nodeNames=nodenamesfemages)
dev.off()

png("Figures_Quest/graphFem2.png", width=2000, height=1400)
graphFem2<-qgraph(Fem2.cor, graph="glasso", layout="spring",labels=labels_femage,
                  vsize=7, cut=0, maximum=.45, sampleSize = nrow(Fem2),
                  border.width=0.1, border.color="#a8a8a8", minimum=.03, 
                  groups=grfemage, color=c("#377eb8", "#fb9a99", "#4daf4a", "#ffffbf", "#ff7f00", "#ffff33", "#bcf60c", "#c77cff", "#a65628", 
                                         "#66c2a5", "#fc8d62", "#46f0f0", "#e78ac3"),title= "Whole sample Network",
                  legend=TRUE,legend.mode='style1',GLratio=2.5,layoutScale=1,legend.cex=0.8,
                  nodeNames=nodenamesfemages)
dev.off()

png("Figures_Quest/graphFem3.png", width=2000, height=1400)
graphFem3<-qgraph(Fem3.cor, graph="glasso", layout="spring",labels=labels_femage,
                  vsize=7, cut=0, maximum=.45, sampleSize = nrow(Fem3),
                  border.width=0.1, border.color="#a8a8a8", minimum=.03, 
                  groups=grfemage, color=c("#377eb8", "#fb9a99", "#4daf4a", "#ffffbf", "#ff7f00", "#ffff33", "#bcf60c", "#c77cff", "#a65628", 
                                         "#66c2a5", "#fc8d62", "#46f0f0", "#e78ac3"),title= "Whole sample Network",
                  legend=TRUE,legend.mode='style1',GLratio=2.5,layoutScale=1,legend.cex=0.8,
                  nodeNames=nodenamesfemages)
dev.off()

png("Figures_Quest/graphFem4.png", width=2000, height=1400)
graphFem4<-qgraph(Fem4.cor, graph="glasso", layout="spring",labels=labels_femage,
                  vsize=7, cut=0, maximum=.45, sampleSize = nrow(Fem4),
                  border.width=0.1, border.color="#a8a8a8", minimum=.03, 
                  groups=grfemage, color=c("#377eb8", "#fb9a99", "#4daf4a", "#ffffbf", "#ff7f00", "#ffff33", "#bcf60c", "#c77cff", "#a65628", 
                                         "#66c2a5", "#fc8d62", "#46f0f0", "#e78ac3"),title= "Whole sample Network",
                  legend=TRUE,legend.mode='style1',GLratio=2.5,layoutScale=1,legend.cex=0.8,
                  nodeNames=nodenamesfemages)
dev.off()

png("Figures_Quest/graphFem5.png", width=2000, height=1400)
graphFem5<-qgraph(Fem5.cor, graph="glasso", layout="spring",labels=labels_femage,
                  vsize=7, cut=0, maximum=.45, sampleSize = nrow(Fem5),
                  border.width=0.1, border.color="#a8a8a8", minimum=.03, 
                  groups=grfemage, color=c("#377eb8", "#fb9a99", "#4daf4a", "#ffffbf", "#ff7f00", "#ffff33", "#bcf60c", "#c77cff", "#a65628", 
                                         "#66c2a5", "#fc8d62", "#46f0f0", "#e78ac3"),title= "Whole sample Network",
                  legend=TRUE,legend.mode='style1',GLratio=2.5,layoutScale=1,legend.cex=0.8,
                  nodeNames=nodenamesfemages)
dev.off()








##########################
########################      NetworkComparisonTest: 
##########################
##########################



NCTFem1vsFem2<-NCT(Fem1, Fem2, 
                         it = 1000, # The number of iterations (permutations).
                         binary.data=FALSE, 
                         paired=FALSE, 
                         weighted=TRUE, 
                         abs=TRUE,
                         test.edges=TRUE, 
                         edges="all", 
                         progressbar=TRUE, 
                         make.positive.definite=TRUE,
                         p.adjust.methods= c("none"), #,"holm","hochberg","hommel", "bonferroni","BH","BY","fdr"), 
                         test.centrality=TRUE, 
                         centrality=c("betweenness","strength"), #'betweenness', 'strength', 'expectedInfluence', 'bridgeStrength', 'bridgeCloseness', 'bridgeBetweenness', 'bridgeExpectedInfluence'
                         nodes="all",
                         communities=gr3,
                         useCommunities="all",
                         #estimator,
                         #estimatorArgs = list(), 
                         verbose = TRUE)
summaryFem1vsFem2<-summary(NCTFem1vsFem2)


NCTFem2vsFem3<-NCT(Fem2, Fem3, 
                   it = 1000, # The number of iterations (permutations).
                   binary.data=FALSE, 
                   paired=FALSE, 
                   weighted=TRUE, 
                   abs=TRUE,
                   test.edges=TRUE, 
                   edges="all", 
                   progressbar=TRUE, 
                   make.positive.definite=TRUE,
                   p.adjust.methods= c("none"), #,"holm","hochberg","hommel", "bonferroni","BH","BY","fdr"), 
                   test.centrality=TRUE, 
                   centrality=c("betweenness","strength"), #'betweenness', 'strength', 'expectedInfluence', 'bridgeStrength', 'bridgeCloseness', 'bridgeBetweenness', 'bridgeExpectedInfluence'
                   nodes="all",
                   communities=gr3,
                   useCommunities="all",
                   #estimator,
                   #estimatorArgs = list(), 
                   verbose = TRUE)
summaryFem2vsFem3<-summary(NCTFem2vsFem3)




NCTFem3vsFem4<-NCT(Fem3, Fem4, 
                   it = 1000, # The number of iterations (permutations).
                   binary.data=FALSE, 
                   paired=FALSE, 
                   weighted=TRUE, 
                   abs=TRUE,
                   test.edges=TRUE, 
                   edges="all", 
                   progressbar=TRUE, 
                   make.positive.definite=TRUE,
                   p.adjust.methods= c("none"), #,"holm","hochberg","hommel", "bonferroni","BH","BY","fdr"), 
                   test.centrality=TRUE, 
                   centrality=c("betweenness","strength"), #'betweenness', 'strength', 'expectedInfluence', 'bridgeStrength', 'bridgeCloseness', 'bridgeBetweenness', 'bridgeExpectedInfluence'
                   nodes="all",
                   communities=gr3,
                   useCommunities="all",
                   #estimator,
                   #estimatorArgs = list(), 
                   verbose = TRUE)
summaryFem3vsFem4<-summary(NCTFem3vsFem4)



NCTFem4vsFem5<-NCT(Fem4, Fem5, 
                   it = 1000, # The number of iterations (permutations).
                   binary.data=FALSE, 
                   paired=FALSE, 
                   weighted=TRUE, 
                   abs=TRUE,
                   test.edges=TRUE, 
                   edges="all", 
                   progressbar=TRUE, 
                   make.positive.definite=TRUE,
                   p.adjust.methods= c("none"), #,"holm","hochberg","hommel", "bonferroni","BH","BY","fdr"), 
                   test.centrality=TRUE, 
                   centrality=c("betweenness","strength"), #'betweenness', 'strength', 'expectedInfluence', 'bridgeStrength', 'bridgeCloseness', 'bridgeBetweenness', 'bridgeExpectedInfluence'
                   nodes="all",
                   communities=gr3,
                   useCommunities="all",
                   #estimator,
                   #estimatorArgs = list(), 
                   verbose = TRUE)
summaryFem4vsFem5<-summary(NCTFem4vsFem5)



















save.image("../R_Env_Quest/NetworkAnalyseTOUT.RData")

