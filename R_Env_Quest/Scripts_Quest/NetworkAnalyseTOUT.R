                                      
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



#load("../R_Env_Quest/NetworkAnalyseTOUT.RData")

#save.image("../R_Env_Quest/NetworkAnalyseTOUT.RData")



conflict_prefer("alpha", "psych")
#save.image("../R_Env_Quest/validity.RData")


Item_bysub <- read.csv("../Analyses/Item_bysub_all.csv")
dataglobal <- read.csv("../Analyses/exporterdansR.csv")
miss_subscale_bcp<- read.csv("../Analyses/miss_subscale_bcp.csv")
Measures_bysub<-read.csv("../Analyses/Measures_bysub.csv")

Measures_bysub$A1_sexe<-as.factor(Measures_bysub$A1_sexe)
Measures_bysub$A2_diff<-as.factor(Measures_bysub$A2_diff)

#Measures_bysub<-subset(Measure_Network, select = - c(Puberty))


#combine les niveaux 1 et 2 de la variable economic stattus 
Measures_bysub$sit_eco[Measures_bysub$sit_eco == 1] <- 2


# Missing values by subscale ------------------------------


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


MCAR_measure2<-mcar_test(Measure_imputed)



# Validity and consistency  ------------------------------


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













# Topological overlap analysis ---------------------------------

##### on data imputed for missing values



Measure_Network<-Measure_imputed

gb_dataset_all <- list('','')
gb_dataset_all['suggested_reductions'] <- list('')


while (gb_dataset_all$suggested_reductions[1] != "No suggested reductions") {
  gb_dataset_all <- goldbricker(
    Measure_Network,
    p = 0.05,
    method = "hittner2003",
    threshold = 0.30,
    corMin = 0.5,
    progressbar = TRUE
  )
  # Réduire le réseau avec les nouvelles suggestions de réduction
  
  if (gb_dataset_all$suggested_reductions[1] == "No suggested reductions") {
    break  # Sortir de la boucle
  }
  
  Measure_Network <- net_reduce(data = Measure_Network, badpairs = gb_dataset_all, method = c("best_goldbricker"))
  
}

supprimer_overlap<-setdiff(names(Measure_imputed), names(Measure_Network))


##Remmetre les mesures réduite au bon endroit
Measure_Network<-Measure_Network %>% select(order(colnames(Measure_Network)))

####utiliser dans raw seulement les colonnes qui reste après overlap
Measure_Network_col<- colnames(Measure_Network)
Measure_Network_raw <- Measures_bysub[, intersect(Measure_Network_col, colnames(Measures_bysub))]




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



#pour savoir automatiquement quelle mesure ont été supprimées 
supp_overlap_list<-list()
for (measure in supprimer_overlap) {
  index <- which(names_list_pairs == measure)[1]
  index_new<-(index+1)
  mesure_name<-names_list_pairs[index_new]
 supp_overlap_list <- append(supp_overlap_list, mesure_name[1])
}

#fonction pour préparerles labels et groupes pour network 

extractlabelnet<- function (data)    {
  
      nodenames<-names(data)
      labels<- list()

    for (i in nodenames) {
      index <- which(names_list_pairs == i)[1]
      index_new<-(index+1)
      new_name<-names_list_pairs[index_new][1]
      labels<-append(labels, new_name[1])
    }
    
    names_groups<- list()
    for (j in nodenames) {
      index <- which(names_list_pairs == j)[1]
      index_new<-(index+2)
      new_name<-names_list_pairs[index_new][1]
      names_groups<-append(names_groups, new_name[1])
    }
    
    names_groups_unique<-unique(names_groups)
    gr<- split(x = 1:length(names_groups), f = unlist(names_groups))
    
    result<- list()
    result[['nodeNames']]<-nodenames
    result[['labels']]<-labels
    result[['groups']]<-gr

 return(result) 
}

# WHOLE net  -------------------

# * AGE Whole -------------------

###SUPPRESSION DES MESURES 

Measure_final_age<-subset(Measure_Network, select = - c(Puberty))
Measure_final_raw_age<-subset(Measure_Network_raw, select = - c(Puberty))





#** Network GMG non mixed --------------- 
Measure_final_age.cor<-cor_auto(Measure_final_age) #Automatically compute an apppropriate correlation matrix  qgraph Packages 
Measure_final_raw_age.cor<-cor_auto(Measure_final_raw_age) #Automatically compute an apppropriate correlation matrix  with qgraph Packages 


max(Measure_final_age.cor[upper.tri(Measure_final_age.cor,diag=FALSE)]) # 0.72


png("Figures_Quest/Network_Whole_imputed_age_ggm.png", width=2000, height=1400)
graph_imputed_age_ggm<-qgraph(Measure_final_age.cor, graph="glasso", layout=Layeoutimputed,labels=label_whole_age[['labels']],
                              tuning=0.50, 
                              maximum=.75,minimum=.0,
                              vsize=7, cut=0,  sampleSize = nrow(Measure_final_age),
                              border.width=0.1, border.color="#a8a8a8",  
                              groups=label_whole_age[['groups']], color=c("#377eb8", "#fb9a99", "#4daf4a", "#ffffbf", "#ff7f00", "#ffff33", "#bcf60c", "#c77cff", "#a65628", 
                                                                          "#66c2a5", "#fc8d62", "#46f0f0", "#e78ac3"),title= "Whole sample Network",
                              legend=TRUE,legend.mode='style1',GLratio=2.5,layoutScale=1,legend.cex=0.8,
                              nodeNames=label_whole_age[['nodeNames']])
dev.off()

Layeoutimputed<-averageLayout(graph_imputed_age_ggm)



png("Figures_Quest/Centrality_Whole_imputed_age_ggm.png", width=2000, height=1400)
centrality_imputed<-centralityPlot(graph_imputed_age_ggm,include="All",orderBy = "Betweenness")
dev.off()



png("Figures_Quest/Network_Whole_raw_ggm_age.png", width=2000, height=1400)
graph_raw_age_ggm<-qgraph(Measure_final_raw_age.cor, graph="glasso", layout=Layeoutimputed,labels=label_whole_age[['labels']],  ## graph="glasso" Will run EBICglasso to obtain an optimal sparse estimate of the partial correlation matrix using the glasso package 
                          tuning=0.50,  maximum=.75,minimum=.0, 
                          vsize=7, cut=0,  sampleSize = nrow(Measure_final_age),
                          border.width=0.1, border.color="#a8a8a8",  
                          groups=label_whole_age[['groups']], color=c("#377eb8", "#fb9a99", "#4daf4a", "#ffffbf", "#ff7f00", "#ffff33", "#bcf60c", "#c77cff", "#a65628", 
                                                                      "#66c2a5", "#fc8d62", "#46f0f0", "#e78ac3"),title= "Whole sample Network",
                          legend=TRUE,legend.mode='style1',GLratio=2.5,layoutScale=1,legend.cex=0.8,
                          nodeNames=label_whole_age[['nodeNames']])
dev.off()





png("Figures_Quest/Centrality_Whole_raw_age.png", width=2000, height=1400)
centrality_raw<-centralityPlot(graph_raw_age,include="All",orderBy = "Betweenness")
dev.off()



### **** imputed and raw data : check the correlation between the 2 matrix ------------------------------
mat_imputed_whole_ggm<- getWmat(Measure_final_age.cor) 
cor_imputed_whole_ggm<-mat_imputed_whole_ggm[upper.tri(mat_imputed_whole_ggm,diag=FALSE)]; 


mat_raw_whole_ggm<- getWmat(Measure_final_raw_age.cor)
cor_raw_whole_ggm<-mat_raw_whole_ggm[upper.tri(mat_raw_whole_ggm,diag=FALSE)]; 


cor_pears_imput_raw <- cor(cor_imputed_whole_ggm,cor_raw_whole_ggm,  method = "pearson"); # 0.9987925
cor_spear_imput_raw <- cor(cor_imputed_whole_ggm,cor_raw_whole_ggm,  method = "spearman"); # 0.9983824



# *** Robustness and Stability for centrality analyses imputed --------------------------------

# A : a priori sample size analysis

#recreate network with estimateNetwork function from package bootnet 
net_imputed_age <- estimateNetwork(Measure_final_age,   default = "EBICglasso",   corMethod = "cor_auto",   tuning = 0.5,   refit = TRUE)


#sample size analysis
simRes <- netSimulator(net_imputed_age$graph,
                       dataGenerator = ggmGenerator(
                         ordinal = TRUE, nLevels = 5),
                       default = "EBICglasso",
                       nCases = c(100,300,600,1000),
                       tuning = 0.5,
                       nReps =100  )

"plot"
plot(simRes)
plot(simRes, yvar = c("strength","betweenness"))




# B  : post-hoc 
#a: edge weight accuracy
set.seed(3)
Wholageimputed_accuracy <- bootnet(Measure_final_age, default = "EBICglasso", type = "nonparametric", nCores = 8, nBoots = 2000, statistics = c("edge", "strength","expectedInfluence","betweenness"))
plot(Wholageimputed_accuracy$sample, layout = Layeoutimputed, fade = FALSE)
plot(Wholageimputed_accuracy, labels = TRUE, order = "sample")
plot(Wholageimputed_accuracy, labels = FALSE, order = "sample")

#b: network centrality coefficient stability
set.seed(4)
Wholageimputed_stability <- bootnet(Measure_final_age, default = "EBICglasso", type = "case", nCores = 8, caseN = 25, nBoots =2000, statistics = c("strength","expectedInfluence","betweenness"))
plot(Wholageimputed_stability$sample, layout = Layeoutimputed, fade = FALSE)
plot(Wholageimputed_stability, statistics = c("strength", "expectedInfluence","betweenness"))

#c: stability (CS) coefficient
corStability(Wholageimputed_stability, statistics = c("strength", "expectedInfluence","betweenness"), cor = 0.7)

#f: difference tests of edge weights for all pairs of 
#pdf(file = "bootnet_edge_difference_CA.pdf")
plotacc <- plot(Wholageimputed_accuracy, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample"); 
#dev.off()
#pdf(file = "bootnet_edge_difference_CA_withoutLabels.pdf")
plotacc2 <- plot(Wholageimputed_accuracy, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample", labels = FALSE); 
#dev.off()

#g: difference test for node strength 
#pdf(file = "bootnet_strength_difference_CA.pdf")
plotstrength <- plot(Wholageimputed_accuracy, "strength"); 
#dev.off()
#pdf(file = "bootnet_expectedInfluence_difference_CA.pdf")
plotexpectedInfluence <- plot(Wholageimputed_accuracy, "expectedInfluence"); 
#dev.off()
plotexpectedInfluence <- plot(Wholageimputed_accuracy,"betweenness"); 



# *** Robustness and Stability for centrality analyses raw data --------------------------------

# A : a priori sample size analysis

#recreate network with estimateNetwork function from package bootnet 
net_raw_age <- (Measure_final_raw_age,   default = "EBICglasso",   corMethod = "cor_auto",   tuning = 0.5,   refit = TRUE )


#sample size analysis
simRes_raw <- netSimulator(net_raw_age$graph,
                       dataGenerator = ggmGenerator(
                         ordinal = TRUE, nLevels = 5),
                       default = "EBICglasso",
                       nCases = c(100,300,600,1000),
                       tuning = 0.5,
                       nReps = 100  )

# plot 
plot(simRes_raw)
plot(simRes_raw, yvar = c("strength","expectedInfluence","betweenness"))




# B  : post-hoc 
#a: edge weight accuracy
set.seed(3)
Wholageraw_accuracy <- bootnet(Measure_final_raw_age, default = "EBICglasso", type = "nonparametric", nCores = 8, nBoots = 2000, statistics = c("edge", "strength","expectedInfluence","betweenness"))
plot(Wholageraw_accuracy$sample, layout = Layeoutimputed, fade = FALSE)
plot(Wholageraw_accuracy, labels = TRUE, order = "sample")
plot(Wholageraw_accuracy, labels = FALSE, order = "sample")

#b: network centrality coefficient stability
set.seed(4)
Wholageraw_stability <- bootnet(Measure_final_age, default = "EBICglasso", type = "case", nCores = 8, caseN = 25, nBoots =2000, statistics = c("strength","expectedInfluence","betweenness"))
plot(Wholageraw_stability$sample, layout = PP_Layout, fade = FALSE)
plot(Wholageraw_stability, statistics = c("strength", "expectedInfluence","betweenness"))

#c: stability (CS) coefficient
corStability(Wholageraw_stability, statistics = c("strength", "expectedInfluence","betweenness"), cor = 0.7)

#f: difference tests of edge weights for all pairs of 
#pdf(file = "bootnet_edge_difference_CA.pdf")
plotacc <- plot(Wholageraw_accuracy, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample"); 
#dev.off()
#pdf(file = "bootnet_edge_difference_CA_withoutLabels.pdf")
plotacc2 <- plot(Wholageraw_accuracy, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample", labels = FALSE); 
#dev.off()

#g: difference test for node strength 
#pdf(file = "bootnet_strength_difference_CA.pdf")
plotstrength <- plot(Wholageraw_accuracy, "strength"); 
#dev.off()
#pdf(file = "bootnet_expectedInfluence_difference_CA.pdf")
plotexpectedInfluence <- plot(Wholageraw_accuracy, "expectedInfluence"); 
#dev.off()
plotexpectedInfluence <- plot(Wholageraw_accuracy,"betweenness"); 

























#** Network MGM pairwise ---------------------------------



library(mgm) ## for mixed graphical model : mix between ordinal and continuous variable 

#**  Make correlation matrix
type_final_age<- c("c", "c", "g", "g",
                   "g", "g", "g", "g",
                   "g", "g", "g","g",
                   "g", "g", "g","g",
                   "g", "g", "g","g",
                   "g", "g", "g","g",
                   "g", "g", "g","g",
                   "p", "g", "g")

cat_final_age <-c(length(unique(Measure_final_age$Sex)),length(unique(Measure_final_age$Cisgender)), 1, 1,
                  1, 1, 1, 1,
                  1, 1, 1,1,
                  1, 1, 1,1,
                  1, 1, 1,1,
                  1, 1, 1,1,
                  1, 1, 1,1,
                  1, 1, 1)


Measure_final_age.mgm <- mgm(Measure_final_age, type=type_final_age,level=cat_final_age, lamda.sel="EBIC",lambdaGam =.50,threshold = "none", k=2, scale = TRUE,
                             saveModels = TRUE,thresholdCat = TRUE)


label_whole_age<-extractlabelnet(Measure_final_age)

png("Figures_Quest/Network_Whole_imputed_age_mgm++.png", width=2000, height=1400)

graph_imputed_age_mgm<- qgraph(Measure_final_age.mgm$pairwise$wadj,  layout=Layeoutimputed,labels=label_whole_age[['labels']],
                         maximum=75,minimum=.0,
                         vsize=7, cut=0,  sampleSize = nrow(Measure_final_age),
                         border.width=0.1, border.color="#a8a8a8",  
                         groups=label_whole_age[['groups']], color=c("#377eb8", "#fb9a99", "#4daf4a", "#ffffbf", "#ff7f00", "#ffff33", "#bcf60c", "#c77cff", "#a65628", 
                                                                     "#66c2a5", "#fc8d62", "#46f0f0", "#e78ac3"),title= "Whole sample Network",
                         legend=TRUE,legend.mode='style1',GLratio=2.5,layoutScale=1,legend.cex=0.8,
                         edge.color = Measure_final_age.mgm$pairwise$edgecolor,
                         nodeNames=label_whole_age[['nodeNames']])
dev.off()



png("Figures_Quest/Centrality_Whole_imputed_age_mgm++.png", width=2000, height=1400)
centrality_imputed_mgm<-centralityPlot(graph_imputed_age_mgm,include="All",orderBy = "Betweenness")
dev.off()


#*** Network MGM Node predictability  -------------------------------


pred_mgm <- predict(object = Measure_final_age.mgm,
                    data =Measure_final_age,
                    errorCon = c("RMSE", "R2"),
                    errorCat = c("CC", "nCC"))

errors <- c(pred_mgm$errors[1:2, 5],pred_mgm$errors[3:31, 3])

png("Figures_Quest/Network_Whole_imputed_age_mgm_error.png", width=2000, height=1400)

graph_imputed_age_mgm_error<- qgraph(Measure_final_age.mgm$pairwise$wadj,  layout=Layeoutimputed,labels=label_whole_age[['labels']],
                               maximum=.50,minimum=.05,
                               pie = errors,
                               vsize=7, cut=0,  sampleSize = nrow(Measure_final_age),
                               border.width=0.1, border.color="#a8a8a8",  
                               groups=label_whole_age[['groups']], color=c("#377eb8", "#fb9a99", "#4daf4a", "#ffffbf", "#ff7f00", "#ffff33", "#bcf60c", "#c77cff", "#a65628", 
                                                                           "#66c2a5", "#fc8d62", "#46f0f0", "#e78ac3"),title= "Whole sample Network",
                               legend=TRUE,legend.mode='style1',GLratio=2.5,layoutScale=1,legend.cex=0.8,
                               edge.color = Measure_final_age.mgm$pairwise$edgecolor,
                               nodeNames=label_whole_age[['nodeNames']])
dev.off()








#** Network MGM order3 ---------------------------------



Measure_final_age_order3.mgm <- mgm(Measure_final_age, type=type,level=cat, lamda.sel="EBIC",lambdaGam =.50,threshold = "none", k=3, scale = TRUE,
                                    saveModels = TRUE,thresholdCat = TRUE)


Measure_final_age_order3.mgm$interactions$indicator
Measure_final_age_order3.mgm$interactions$weights



FactorGraph(object = Measure_final_age_order3.mgm, PairwiseAsEdge = TRUE)




















# * PUB whole  -------------------

###SUPPRESSION DES MESURES POUR NETWORK!!!!!!!!!!!!!!

Measure_final_pub<-subset(Measure_Network, select = - c(Age))
Measure_final_pub_raw<-subset(Measure_Network_raw, select = - c(Age))

### Make correlation matrix
Measure_final_pub.cor<-cor_auto(Measure_final_pub) #compute the correlation Matrix with qgraph Packages 
Measure_final_pub.cor_raw<-cor_auto(Measure_final_pub_raw) #compute the correlation Matrix with qgraph Packages 

##### Network analysis

label_whole_pub<-extractlabelnet(Measure_final_pub)


png("Figures_Quest/Network_Whole_imputed_pub.png", width=2000, height=1400)
graph_imputed<-qgraph(Measure_final_pub.cor, graph="glasso", layout=Layeoutimputed,labels=label_whole_pub[['labels']],
                      tuning=0.50, 
                      vsize=7, cut=0, maximum=.45, sampleSize = nrow(Measure_final_pub),
                      border.width=0.1, border.color="#a8a8a8", minimum=.03, 
                      groups=label_whole_pub[['groups']], color=c("#377eb8", "#fb9a99", "#4daf4a", "#ffffbf", "#ff7f00", "#ffff33", "#bcf60c", "#c77cff", "#a65628", 
                                             "#66c2a5", "#fc8d62", "#46f0f0", "#e78ac3"),title= "Whole sample Network",
                      legend=TRUE,legend.mode='style1',GLratio=2.5,layoutScale=1,legend.cex=0.8,
                      nodeNames=label_whole_pub[['nodeNames']])
dev.off()


png("Figures_Quest/Network_Whole_raw_pub.png", width=2000, height=1400)
graph_raw<-qgraph(Measure_final_pub.cor_raw, graph="glasso", layout=Layeoutimputed,labels=label_whole_pub[['labels']],
                  tuning=0.50, 
                  vsize=7, cut=0, maximum=.45, sampleSize = nrow(Measure_final_pub),
                  border.width=0.1, border.color="#a8a8a8", minimum=.03, 
                  groups=label_whole_pub[['groups']], color=c("#377eb8", "#fb9a99", "#4daf4a", "#ffffbf", "#ff7f00", "#ffff33", "#bcf60c", "#c77cff", "#a65628", 
                                         "#66c2a5", "#fc8d62", "#46f0f0", "#e78ac3"),title= "Whole sample Network",
                  legend=TRUE,legend.mode='style1',GLratio=2.5,layoutScale=1,legend.cex=0.8,
                  nodeNames=label_whole_pub[['nodeNames']])
dev.off()


### Centrality analysis

png("Figures_Quest/Centrality_Whole_imputed.png", width=2000, height=1400)
centrality_imputed<-centralityPlot(graph_imputed,include="All",orderBy = "Betweenness")
dev.off()

png("Figures_Quest/Centrality_Whole_raw.png", width=2000, height=1400)
centrality_raw<-centralityPlot(graph_raw,include="All",orderBy = "Betweenness")
dev.off()


### Robustesse 





# SEX comparaison  -------------------

############# division analyse par sex 

Measure_Fem_age<- subset(Measure_final_age[Measure_final_age$Sex == 1, ], select = - c(Sex,Cisgender))
Measure_Mal_age<- subset(Measure_final_age[Measure_final_age$Sex == 2, ], select = - c(Sex,Cisgender))

Measure_Fem_pub<- subset(Measure_final_pub[Measure_final_pub$Sex == 1, ], select = - c(Sex,Cisgender))
Measure_Mal_pub<- subset(Measure_final_pub[Measure_final_pub$Sex == 2, ], select = - c(Sex,Cisgender))


#Measure_final_age_col<- colnames(Measure_final_age)
#Measure_Fem_age <- Measure_Fem[, intersect(Measure_final_age_col, colnames(Measure_Fem))]
#Measure_Mal_age <- Measure_Mal[, intersect(Measure_final_age_col, colnames(Measure_Mal))]

# * Sex Check de l'overlap en boucle  --------------

Measure_Mal_age_net<-Measure_Mal_age

gb_dataset_mal <- list('','')
gb_dataset_mal['suggested_reductions'] <- list('')

 
while (gb_dataset_mal$suggested_reductions[1] != "No suggested reductions") {
  gb_dataset_mal <- goldbricker(
    Measure_Mal_age_net,
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
  
  Measure_Mal_age_net <- net_reduce(data = Measure_Mal_age_net, badpairs = gb_dataset_mal, method = c("best_goldbricker"))

}

supprimer_overlap_mal<-setdiff(names(Measure_Mal_age), names(Measure_Mal_age_net))

Measure_Network_sex_col<- colnames(Measure_Mal_age_net)
Measure_Fem_age_net <- Measure_Fem_age[, intersect(Measure_Network_sex_col, colnames(Measure_Fem_age))]


# * NCT  Sex--------------


# *** none gam50 nonabs----------------------------

NCTFemvsMal_nocorr_50_nonabs<-NCT(Measure_Fem_age_net, Measure_Mal_age_net,
                               gamma=0.50,
                               it = 1000, # The number of iterations (permutations).
                               binary.data=FALSE, 
                               paired=FALSE, 
                               weighted=TRUE, 
                               abs=FALSE,
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

png("Figures_Quest/NCTFemvsMal_nocorr_50_nonabs.png", width=2000, height=1400)
plot(NCTFemvsMal_nocorr_50_nonabs)
dev.off()

### test diff global strength.
p_diffstrenght_nocorr_50_nonabs <- NCTFemvsMal_nocorr_50_nonabs$glstrinv.pval # 	 The p value resulting from the permutation test concerning difference in global strength.

###test diff maximum difference in edge weights.
p_globaledgeweight_nocorr_50_nonabs <- NCTFemvsMal_nocorr_50_nonabs$nwinv.pval  # The p value resulting from the permutation test concerning the maximum difference in edge weights.

###test & trouver diff all edge weights.
p_alledgeweight_nocorr_50_nonabs <- NCTFemvsMal_nocorr_50_nonabs$einv.pvals # p-values (corrected for multiple testing or not according to 'p.adjust.methods') per edge from the permutation test concerning differences in edges weights
p_alledgeweight_nocorr_50_nonabs <- as.data.frame(p_alledgeweight_nocorr_50_nonabs)
names(p_alledgeweight_nocorr_50_nonabs)[3]<- 'pval'
edgeweight_signi_nocorr_50_nonabs <- p_alledgeweight_nocorr_50_nonabs %>% dplyr::filter(pval < 0.050) ##############"!!!!!!!!!!!!

which(names(Measure_Fem_age_net)=='Body_Objectification')
NCTFemvsMal_nocorr_50_nonabs$einv.real[11,23]

###trouver diffférence de centralité
p_allcent_nocorr_50_nonabs <- NCTFemvsMal_nocorr_50_nonabs$diffcen.pval #	p-values(corrected for multiple testing or not according to 'p.adjust.methods') per node from the permutation test concerning differences in centralities. Only if test.centrality = TRUE.
p_allcent_nocorr_50_nonabs<- as.data.frame(p_allcent_nocorr_50_nonabs)
diffbetwee_signi_nocorr_50_nonabs_p<-rownames(p_allcent_nocorr_50_nonabs%>% dplyr::filter(betweenness  < 0.050 ))
diffstren_cent_signi_nocorr_50_nonabs_p<-rownames(p_allcent_nocorr_50_nonabs%>% dplyr::filter(strength < 0.050))

diffbetwee_signi_nocorr_50_nonabs <- subset(NCTFemvsMal_nocorr_50_nonabs$diffcen.real, row.names(NCTFemvsMal_nocorr_50_nonabs$diffcen.real) %in% diffbetwee_signi_nocorr_50_nonabs_p)
diffstren_cent_signi_nocorr_50_nonabs <- subset(NCTFemvsMal_nocorr_50_nonabs$diffcen.real, row.names(NCTFemvsMal_nocorr_50_nonabs$diffcen.real) %in% diffstren_cent_signi_nocorr_50_nonabs_p)




# *** FDR gam50 nonabs----------------------------

NCTFemvsMal_fdr_50_nonabs<-NCT(Measure_Fem_age_net, Measure_Mal_age_net,
                               gamma=0.50,
                               it = 10000, # The number of iterations (permutations).
                               binary.data=FALSE, 
                               paired=FALSE, 
                               weighted=TRUE, 
                               abs=FALSE,
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


### test diff global strength.
p_diffstrenght_fdr_50_nonabs <- NCTFemvsMal_fdr_50_nonabs$glstrinv.pval # 	 The p value resulting from the permutation test concerning difference in global strength.

###test diff maximum difference in edge weights.
p_globaledgeweight_fdr_50_nonabs <- NCTFemvsMal_fdr_50_nonabs$nwinv.pval  # The p value resulting from the permutation test concerning the maximum difference in edge weights.

###test & trouver diff all edge weights.
p_alledgeweight_fdr_50_nonabs <- NCTFemvsMal_fdr_50_nonabs$einv.pvals # p-values (corrected for multiple testing or not according to 'p.adjust.methods') per edge from the permutation test concerning differences in edges weights
p_alledgeweight_fdr_50_nonabs <- as.data.frame(p_alledgeweight_fdr_50_nonabs)
names(p_alledgeweight_fdr_50_nonabs)[3]<- 'pval'
edgeweight_signi_fdr_50_nonabs <- p_alledgeweight_fdr_50_nonabs %>% dplyr::filter(pval < 0.050) ##############"!!!!!!!!!!!!

which(names(Measure_Fem_age_net)=='Body_Objectification')
NCTFemvsMal_fdr_50_nonabs$einv.real[11,23]

###trouver diffférence de centralité
p_allcent_fdr_50_nonabs <- NCTFemvsMal_fdr_50_nonabs$diffcen.pval #	p-values(corrected for multiple testing or not according to 'p.adjust.methods') per node from the permutation test concerning differences in centralities. Only if test.centrality = TRUE.
p_allcent_fdr_50_nonabs<- as.data.frame(p_allcent_fdr_50_nonabs)
diffbetwee_signi_fdr_50_nonabs_p<-rownames(p_allcent_fdr_50_nonabs%>% dplyr::filter(betweenness  < 0.050 ))
diffstren_cent_signi_fdr_50_nonabs_p<-rownames(p_allcent_fdr_50_nonabs%>% dplyr::filter(strength < 0.050))

diffbetwee_signi_fdr_50_nonabs <- subset(NCTFemvsMal_fdr_50_nonabs$diffcen.real, row.names(NCTFemvsMal_fdr_50_nonabs$diffcen.real) %in% diffbetwee_signi_fdr_50_nonabs_p)
diffstren_cent_signi_fdr_50_nonabs <- subset(NCTFemvsMal_fdr_50_nonabs$diffcen.real, row.names(NCTFemvsMal_fdr_50_nonabs$diffcen.real) %in% diffstren_cent_signi_fdr_50_nonabs_p)


# *** BH gam50 nonabs----------------------------

NCTFemvsMal_bh_50_nonabs<-NCT(Measure_Fem_age_net, Measure_Mal_age_net,
                               gamma=0.50,
                               it = 10000, # The number of iterations (permutations).
                               binary.data=FALSE, 
                               paired=FALSE, 
                               weighted=TRUE, 
                               abs=FALSE,
                               test.edges=TRUE, 
                               edges="all", 
                               progressbar=TRUE, 
                               make.positive.definite=TRUE,
                               p.adjust.methods= c("BH"), #,"holm","hochberg","hommel", "bonferroni","BH","BY","fdr"), 
                               test.centrality=TRUE, 
                               centrality=c("betweenness","strength"), #'betweenness', 'strength', 'expectedInfluence', 'bridgeStrength', 'bridgeCloseness', 'bridgeBetweenness', 'bridgeExpectedInfluence'
                               nodes="all",
                               communities=gr_sexe,
                               useCommunities="all",
                               #estimator,
                               #estimatorArgs = list(), 
                               verbose = TRUE)


### test diff global strength.
p_diffstrenght_bh_50_nonabs <- NCTFemvsMal_bh_50_nonabs$glstrinv.pval # 	 The p value resulting from the permutation test concerning difference in global strength.

###test diff maximum difference in edge weights.
p_globaledgeweight_bh_50_nonabs <- NCTFemvsMal_bh_50_nonabs$nwinv.pval  # The p value resulting from the permutation test concerning the maximum difference in edge weights.

###test & trouver diff all edge weights.
p_alledgeweight_bh_50_nonabs <- NCTFemvsMal_bh_50_nonabs$einv.pvals # p-values (corrected for multiple testing or not according to 'p.adjust.methods') per edge from the permutation test concerning differences in edges weights
p_alledgeweight_bh_50_nonabs <- as.data.frame(p_alledgeweight_bh_50_nonabs)
names(p_alledgeweight_bh_50_nonabs)[3]<- 'pval'
edgeweight_signi_bh_50_nonabs <- p_alledgeweight_bh_50_nonabs %>% dplyr::filter(pval < 0.050) ##############"!!!!!!!!!!!!

which(names(Measure_Fem_age_net)=='Body_Objectification')
NCTFemvsMal_bh_50_nonabs$einv.real[11,23]

###trouver diffférence de centralité
p_allcent_bh_50_nonabs <- NCTFemvsMal_bh_50_nonabs$diffcen.pval #	p-values(corrected for multiple testing or not according to 'p.adjust.methods') per node from the permutation test concerning differences in centralities. Only if test.centrality = TRUE.
p_allcent_bh_50_nonabs<- as.data.frame(p_allcent_bh_50_nonabs)
diffbetwee_signi_bh_50_nonabs_p<-rownames(p_allcent_bh_50_nonabs%>% dplyr::filter(betweenness  < 0.050 ))
diffstren_cent_signi_bh_50_nonabs_p<-rownames(p_allcent_bh_50_nonabs%>% dplyr::filter(strength < 0.050))

diffbetwee_signi_bh_50_nonabs <- subset(NCTFemvsMal_bh_50_nonabs$diffcen.real, row.names(NCTFemvsMal_bh_50_nonabs$diffcen.real) %in% diffbetwee_signi_bh_50_nonabs_p)
diffstren_cent_signi_bh_50_nonabs <- subset(NCTFemvsMal_bh_50_nonabs$diffcen.real, row.names(NCTFemvsMal_bh_50_nonabs$diffcen.real) %in% diffstren_cent_signi_bh_50_nonabs_p)





# *** none gam25 nonabs----------------------------

NCTFemvsMal_nocorr_25_nonabs<-NCT(Measure_Fem_age_net, Measure_Mal_age_net,
                                  gamma=0.25,
                                  it = 1000, # The number of iterations (permutations).
                                  binary.data=FALSE, 
                                  paired=FALSE, 
                                  weighted=TRUE, 
                                  abs=FALSE,
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
p_diffstrenght_nocorr_25_nonabs <- NCTFemvsMal_nocorr_25_nonabs$glstrinv.pval # 	 The p value resulting from the permutation test concerning difference in global strength.

###test diff maximum difference in edge weights.
p_globaledgeweight_nocorr_25_nonabs <- NCTFemvsMal_nocorr_25_nonabs$nwinv.pval  # The p value resulting from the permutation test concerning the maximum difference in edge weights.

###test & trouver diff all edge weights.
p_alledgeweight_nocorr_25_nonabs <- NCTFemvsMal_nocorr_25_nonabs$einv.pvals # p-values (corrected for multiple testing or not according to 'p.adjust.methods') per edge from the permutation test concerning differences in edges weights
p_alledgeweight_nocorr_25_nonabs <- as.data.frame(p_alledgeweight_nocorr_25_nonabs)
names(p_alledgeweight_nocorr_25_nonabs)[3]<- 'pval'
edgeweight_signi_nocorr_25_nonabs <- p_alledgeweight_nocorr_25_nonabs %>% dplyr::filter(pval < 0.05) ##############"!!!!!!!!!!!!



###trouver diffférence de centralité
p_allcent_nocorr_25_nonabs <- NCTFemvsMal_nocorr_25_nonabs$diffcen.pval #	p-values(corrected for multiple testing or not according to 'p.adjust.methods') per node from the permutation test concerning differences in centralities. Only if test.centrality = TRUE.
p_allcent_nocorr_25_nonabs<- as.data.frame(p_allcent_nocorr_25_nonabs)
diffbetwee_signi_nocorr_25_nonabs_p<-rownames(p_allcent_nocorr_25_nonabs%>% dplyr::filter(betweenness  < 0.05 ))
diffstren_cent_signi_nocorr_25_nonabs_p<-rownames(p_allcent_nocorr_25_nonabs%>% dplyr::filter(strength < 0.050))

diffbetwee_signi_nocorr_25_nonabs <- subset(NCTFemvsMal_nocorr_25_nonabs$diffcen.real, row.names(NCTFemvsMal_nocorr_25_nonabs$diffcen.real) %in% diffbetwee_signi_nocorr_25_nonabs_p)
diffstren_cent_signi_nocorr_25_nonabs <- subset(NCTFemvsMal_nocorr_25_nonabs$diffcen.real, row.names(NCTFemvsMal_nocorr_25_nonabs$diffcen.real) %in% diffstren_cent_signi_bh_25_nonabs_p)





# *** FDR gam25 nonabs ----------------------------


NCTFemvsMal_fdr_25_nonabs<-NCT(Measure_Fem_age_net, Measure_Mal_age_net,
                               gamma=0.25,
                               it = 1000, # The number of iterations (permutations).
                               binary.data=FALSE, 
                               paired=FALSE, 
                               weighted=TRUE, 
                               abs=FALSE,
                               test.edges=TRUE, 
                               edges="all", 
                               progressbar=TRUE, 
                               make.positive.definite=TRUE,
                               p.adjust.methods= c("BH"), #,"holm","hochberg","hommel", "bonferroni","BH","BY","fdr"), 
                               test.centrality=TRUE, 
                               centrality=c("betweenness","strength"), #'betweenness', 'strength', 'expectedInfluence', 'bridgeStrength', 'bridgeCloseness', 'bridgeBetweenness', 'bridgeExpectedInfluence'
                               nodes="all",
                               communities=gr_sexe,
                               useCommunities="all",
                               #estimator,
                               #estimatorArgs = list(), 
                               verbose = TRUE)


### test diff global strength.
p_diffstrenght_fdr_25_nonabs <- NCTFemvsMal_fdr_25_nonabs$glstrinv.pval # 	 The p value resulting from the permutation test concerning difference in global strength.

###test diff maximum difference in edge weights.
p_globaledgeweight_fdr_25_nonabs <- NCTFemvsMal_fdr_25_nonabs$nwinv.pval  # The p value resulting from the permutation test concerning the maximum difference in edge weights.

###test & trouver diff all edge weights.
p_alledgeweight_fdr_25_nonabs <- NCTFemvsMal_fdr_25_nonabs$einv.pvals # p-values (corrected for multiple testing or not according to 'p.adjust.methods') per edge from the permutation test concerning differences in edges weights
p_alledgeweight_fdr_25_nonabs <- as.data.frame(p_alledgeweight_fdr_25_nonabs)
names(p_alledgeweight_fdr_25_nonabs)[3]<- 'pval'
edgeweight_signi_fdr_25_nonabs <- p_alledgeweight_fdr_25_nonabs %>% dplyr::filter(pval < 0.050)

###trouver diffférence de centralité
p_allcent_fdr_25_nonabs <- NCTFemvsMal_fdr_25_nonabs$diffcen.pval #	p-values(corrected for multiple testing or not according to 'p.adjust.methods') per node from the permutation test concerning differences in centralities. Only if test.centrality = TRUE.
p_allcent_fdr_25_nonabs<- as.data.frame(p_allcent_fdr_25_nonabs)
diffbetwee_signi_fdr_25_nonabs_p<-rownames(p_allcent_fdr_25_nonabs%>% dplyr::filter(betweenness  < 0.050 ))
diffstren_cent_signi_fdr_25_nonabs_p<-rownames(p_allcent_fdr_25_nonabs%>% dplyr::filter(strength < 0.050))

diffbetwee_signi_fdr_25_nonabs <- subset(NCTFemvsMal_fdr_25_nonabs$diffcen.real, row.names(NCTFemvsMal_fdr_25_nonabs$diffcen.real) %in% diffbetwee_signi_fdr_25_nonabs_p)
diffstren_cent_signi_fdr_25_nonabs <- subset(NCTFemvsMal_fdr_25_nonabs$diffcen.real, row.names(NCTFemvsMal_fdr_25_nonabs$diffcen.real) %in% diffstren_cent_signi_fdr_25_nonabs_p)












# *** FDR gam10 nonabs----------------------------

NCTFemvsMal_fdr_10_nonabs<-NCT(Measure_Fem_age_net, Measure_Mal_age_net,
                               gamma=0.10,
                               it = 1000, # The number of iterations (permutations).
                               binary.data=FALSE, 
                               paired=FALSE, 
                               weighted=TRUE, 
                               abs=FALSE,
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


### test diff global strength.
p_diffstrenght_fdr_10_nonabs <- NCTFemvsMal_fdr_10_nonabs$glstrinv.pval # 	 The p value resulting from the permutation test concerning difference in global strength.

###test diff maximum difference in edge weights.
p_globaledgeweight_fdr_10_nonabs <- NCTFemvsMal_fdr_10_nonabs$nwinv.pval  # The p value resulting from the permutation test concerning the maximum difference in edge weights.

###test & trouver diff all edge weights.
p_alledgeweight_fdr_10_nonabs <- NCTFemvsMal_fdr_10_nonabs$einv.pvals # p-values (corrected for multiple testing or not according to 'p.adjust.methods') per edge from the permutation test concerning differences in edges weights
p_alledgeweight_fdr_10_nonabs <- as.data.frame(p_alledgeweight_fdr_10_nonabs)
names(p_alledgeweight_fdr_10_nonabs)[3]<- 'pval'
edgeweight_signi_fdr_10_nonabs <- p_alledgeweight_fdr_10_nonabs %>% dplyr::filter(pval < 0.050)

###trouver diffférence de centralité
p_allcent_fdr_10_nonabs <- NCTFemvsMal_fdr_10_nonabs$diffcen.pval #	p-values(corrected for multiple testing or not according to 'p.adjust.methods') per node from the permutation test concerning differences in centralities. Only if test.centrality = TRUE.
p_allcent_fdr_10_nonabs<- as.data.frame(p_allcent_fdr_10_nonabs)
diffbetwee_signi_fdr_10_nonabs_p<-rownames(p_allcent_fdr_10_nonabs%>% dplyr::filter(betweenness  < 0.050 ))
diffstren_cent_signi_fdr_10_nonabs_p<-rownames(p_allcent_fdr_10_nonabs%>% dplyr::filter(strength < 0.050))

diffbetwee_signi_fdr_10_nonabs <- subset(NCTFemvsMal_fdr_10_nonabs$diffcen.real, row.names(NCTFemvsMal_fdr_10_nonabs$diffcen.real) %in% diffbetwee_signi_fdr_10_nonabs_p)
diffstren_cent_signi_fdr_10_nonabs <- subset(NCTFemvsMal_fdr_10_nonabs$diffcen.real, row.names(NCTFemvsMal_fdr_10_nonabs$diffcen.real) %in% diffstren_cent_signi_fdr_10_nonabs_p)





# *** FDR gam10  abs----------------------------


NCTFemvsMal_fdr_10_abs<-NCT(Measure_Fem_age_net, Measure_Mal_age_net,
                            gamma=0.10,
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


### test diff global strength.
p_diffstrenght_fdr_10_abs <- NCTFemvsMal_fdr_10_abs$glstrinv.pval # 	 The p value resulting from the permutation test concerning difference in global strength.

###test diff maximum difference in edge weights.
p_globaledgeweight_fdr_10_abs <- NCTFemvsMal_fdr_10_abs$nwinv.pval  # The p value resulting from the permutation test concerning the maximum difference in edge weights.

###test & trouver diff all edge weights.
p_alledgeweight_fdr_10_abs <- NCTFemvsMal_fdr_10_abs$einv.pvals # p-values (corrected for multiple testing or not according to 'p.adjust.methods') per edge from the permutation test concerning differences in edges weights
p_alledgeweight_fdr_10_abs <- as.data.frame(p_alledgeweight_fdr_10_abs)
names(p_alledgeweight_fdr_10_abs)[3]<- 'pval'
edgeweight_signi_fdr_10_abs <- p_alledgeweight_fdr_10_abs %>% dplyr::filter(pval < 0.050)

###trouver diffférence de centralité
p_allcent_fdr_10_abs <- NCTFemvsMal_fdr_10_abs$diffcen.pval #	p-values(corrected for multiple testing or not according to 'p.adjust.methods') per node from the permutation test concerning differences in centralities. Only if test.centrality = TRUE.
p_allcent_fdr_10_abs<- as.data.frame(p_allcent_fdr_10_abs)
diffbetwee_signi_fdr_10_abs_p<-rownames(p_allcent_fdr_10_abs%>% dplyr::filter(betweenness  < 0.050 ))
diffstren_cent_signi_fdr_10_abs_p<-rownames(p_allcent_fdr_10_abs%>% dplyr::filter(strength < 0.050))

diffbetwee_signi_fdr_10_abs <- subset(NCTFemvsMal_fdr_10_abs$diffcen.real, row.names(NCTFemvsMal_fdr_10_abs$diffcen.real) %in% diffbetwee_signi_fdr_10_abs_p)
diffstren_cent_signi_fdr_10_abs <- subset(NCTFemvsMal_fdr_10_abs$diffcen.real, row.names(NCTFemvsMal_fdr_10_abs$diffcen.real) %in% diffstren_cent_signi_fdr_10_abs_p)



# *** FDR gam25 abs----------------------------

NCTFemvsMal_fdr_25_abs<-NCT(Measure_Fem_age_net, Measure_Mal_age_net,
                     gamma=0.25,
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


### test diff global strength.
p_diffstrenght_fdr_25_abs <- NCTFemvsMal_fdr_25_abs$glstrinv.pval # 	 The p value resulting from the permutation test concerning difference in global strength.

###test diff maximum difference in edge weights.
p_globaledgeweight_fdr_25_abs <- NCTFemvsMal_fdr_25_abs$nwinv.pval  # The p value resulting from the permutation test concerning the maximum difference in edge weights.

###test & trouver diff all edge weights.
p_alledgeweight_fdr_25_abs <- NCTFemvsMal_fdr_25_abs$einv.pvals # p-values (corrected for multiple testing or not according to 'p.adjust.methods') per edge from the permutation test concerning differences in edges weights
p_alledgeweight_fdr_25_abs <- as.data.frame(p_alledgeweight_fdr_25_abs)
names(p_alledgeweight_fdr_25_abs)[3]<- 'pval'
edgeweight_signi_fdr_25_abs <- p_alledgeweight_fdr_25_abs %>% dplyr::filter(pval < 0.050)

###trouver diffférence de centralité
p_allcent_fdr_25_abs <- NCTFemvsMal_fdr_25_abs$diffcen.pval #	p-values(corrected for multiple testing or not according to 'p.adjust.methods') per node from the permutation test concerning differences in centralities. Only if test.centrality = TRUE.
p_allcent_fdr_25_abs<- as.data.frame(p_allcent_fdr_25_abs)
diffbetwee_signi_fdr_25_abs_p<-rownames(p_allcent_fdr_25_abs%>% dplyr::filter(betweenness  < 0.050 ))
diffstren_cent_signi_fdr_25_abs_p<-rownames(p_allcent_fdr_25_abs%>% dplyr::filter(strength < 0.050))

diffbetwee_signi_fdr_25_abs <- subset(NCTFemvsMal_fdr_25_abs$diffcen.real, row.names(NCTFemvsMal_fdr_25_abs$diffcen.real) %in% diffbetwee_signi_fdr_25_abs_p)
diffstren_cent_signi_fdr_25_abs <- subset(NCTFemvsMal_fdr_25_abs$diffcen.real, row.names(NCTFemvsMal_fdr_25_abs$diffcen.real) %in% diffstren_cent_signi_fdr_25_abs_p)

# *** FDR gam50 abs----------------------------


NCTFemvsMal_fdr_50_abs<-NCT(Measure_Fem_age_net, Measure_Mal_age_net,
                            gamma=0.50,
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


### test diff global strength.
p_diffstrenght_fdr_50_abs <- NCTFemvsMal_fdr_50_abs$glstrinv.pval # 	 The p value resulting from the permutation test concerning difference in global strength.

###test diff maximum difference in edge weights.
p_globaledgeweight_fdr_50_abs <- NCTFemvsMal_fdr_50_abs$nwinv.pval  # The p value resulting from the permutation test concerning the maximum difference in edge weights.

###test & trouver diff all edge weights.
p_alledgeweight_fdr_50_abs <- NCTFemvsMal_fdr_50_abs$einv.pvals # p-values (corrected for multiple testing or not according to 'p.adjust.methods') per edge from the permutation test concerning differences in edges weights
p_alledgeweight_fdr_50_abs <- as.data.frame(p_alledgeweight_fdr_50_abs)
names(p_alledgeweight_fdr_50_abs)[3]<- 'pval'
edgeweight_signi_fdr_50_abs <- p_alledgeweight_fdr_50_abs %>% dplyr::filter(pval < 0.050) ##############"!!!!!!!!!!!!

which(names(Measure_Fem_age_net)=='Body_Objectification')
NCTFemvsMal_fdr_50_abs$einv.real[11,23]

###trouver diffférence de centralité
p_allcent_fdr_50_abs <- NCTFemvsMal_fdr_50_abs$diffcen.pval #	p-values(corrected for multiple testing or not according to 'p.adjust.methods') per node from the permutation test concerning differences in centralities. Only if test.centrality = TRUE.
p_allcent_fdr_50_abs<- as.data.frame(p_allcent_fdr_50_abs)
diffbetwee_signi_fdr_50_abs_p<-rownames(p_allcent_fdr_50_abs%>% dplyr::filter(betweenness  < 0.050 ))
diffstren_cent_signi_fdr_50_abs_p<-rownames(p_allcent_fdr_50_abs%>% dplyr::filter(strength < 0.050))

diffbetwee_signi_fdr_50_abs <- subset(NCTFemvsMal_fdr_50_abs$diffcen.real, row.names(NCTFemvsMal_fdr_50_abs$diffcen.real) %in% diffbetwee_signi_fdr_50_abs_p)
diffstren_cent_signi_fdr_50_abs <- subset(NCTFemvsMal_fdr_50_abs$diffcen.real, row.names(NCTFemvsMal_fdr_50_abs$diffcen.real) %in% diffstren_cent_signi_fdr_50_abs_p)

















# * Sex Plot Network --------------


###label sex

label_whole_age<-extractlabelnet(Measure_Fem_age_net)

Measure_Fem_age_net.cor<-cor_auto(Measure_Fem_age_net) #compute the correlation Matrix with qgraph Packages 
Measure_Mal_age_net.cor<-cor_auto(Measure_Mal_age_net) #compute the correlation Matrix with qgraph Packages 

max_fem<-max(Measure_Fem_age_net.cor[upper.tri(Measure_Fem_age_net.cor,diag=FALSE)]) # 0.6357468
max_hom<-max(Measure_Mal_age_net.cor[upper.tri(Measure_Mal_age_net.cor,diag=FALSE)]) # 0.5863839


png("Figures_Quest/Network_fem.png", width=2000, height=1400)
graphFem<-qgraph(Measure_Fem_age_net.cor, graph="glasso", layout="spring",labels=label_whole_age[['labels']],
                 tuning=0.50,
                  maximum=.75,minimum=.0,
                  vsize=7, cut=0,  sampleSize = nrow(Measure_Fem_age_net),
                  border.width=0.1, border.color="#a8a8a8",  
                  groups=label_whole_age[['groups']], color=c("#377eb8", "#fb9a99", "#4daf4a", "#ffffbf", "#ff7f00", "#ffff33", "#bcf60c", "#c77cff", "#a65628", 
                                           "#66c2a5", "#fc8d62", "#46f0f0", "#e78ac3"),
                  legend=TRUE,legend.mode='style1',GLratio=2.5,layoutScale=1,legend.cex=0.8,
                  nodeNames=label_whole_age[['nodeNames']])
dev.off()

layout_fem<-averageLayout(graphFem)

png("Figures_Quest/Network_mal.png", width=2000, height=1400)
graphMal<-qgraph(Measure_Mal_age_net.cor, graph="glasso", layout=layout_fem,labels=label_whole_age[['labels']],
                 tuning=0.50,
                  maximum=.75,minimum=.0,
                  vsize=7, cut=0,sampleSize = nrow(Measure_Mal_age_net),
                  border.width=0.1, border.color="#a8a8a8", 
                  groups=label_whole_age[['groups']], color=c("#377eb8", "#fb9a99", "#4daf4a", "#ffffbf", "#ff7f00", "#ffff33", "#bcf60c", "#c77cff", "#a65628", 
                                           "#66c2a5", "#fc8d62", "#46f0f0", "#e78ac3"),
                  legend=TRUE,legend.mode='style1',GLratio=2.5,layoutScale=1,legend.cex=0.8,
                  nodeNames=label_whole_age[['nodeNames']])
dev.off()


sum(graphMal$Edgelist$weight)  ## 2.167981
sum(graphFem$Edgelist$weight)  ## 3.417337


### **** mal and femal : check the correlation between the 2 matrix ------------------------------
mat_fem_ggm<- getWmat(Measure_Fem_age_net.cor) 
cor_fem_ggm<-mat_fem_ggm[upper.tri(mat_fem_ggm,diag=FALSE)]; 


mat_mal_ggm<- getWmat(Measure_Mal_age_net.cor)
cor_mal_ggm<-mat_mal_ggm[upper.tri(mat_mal_ggm,diag=FALSE)]; 


cor_pears_mal_fem <- cor(cor_fem_ggm,cor_mal_ggm,  method = "pearson"); # 0.8745354
cor_spear_mal_fem <- cor(cor_fem_ggm,cor_mal_ggm,  method = "spearman"); # 0.8439564




# * psychonetrics (Epskamp et al., 2021)Multigroup network modeling Sex   -------------------------------------------------
library(psychonetrics)

#First, one estimates a model in each separate dataset and removes non-significant partial correlations. S

graphFem2<-ggm(Measure_Fem_age_net, estimator = "FIML") %>% 
  runmodel %>% 
  prune %>% 
  modelsearch


graphMal2<-ggm(Measure_Mal_age_net, estimator = "FIML") %>% 
  runmodel %>% 
  prune %>% 
  modelsearch




compare(graphFem2,graphMal2)





econd, a pooled model is estimated in which
each parameter is included that was included in at least one
of the individual models from the first step. Third, equality
constraints are freed in a step-wise fashion until the BIC of
the overall model does not improve any further. The model
at the end of this procedure is selected as the final model and
specifies the estimated group differences. This method is
available in the R-package psychonetrics (Epskamp, 2020).




# * Williams et al. (2019) proposed two Bayesian methods -----------------------------------------
to test for differences in parameters across groups. The
first method uses a Bayes factor to compare the hypothesis
that a given partial correlation is the same in all groups
vs. not. The second method computes the posterior of the
difference between partial correlations in two groups, and
uses a threshold α on the posterior to decide whether the
difference is reliably different from zero. Both methods are
implemented in the R-package BGGM (Williams & Mulder,
                                   2019).Recently these methods have been extended to data
consisting of continuous, ordinal, and binary variables using
a semi-parametric copula model.







# *  MGM moderated network models in  (Halsbecq) -------------------------------------------

head(Measure_final_age_MGM_sex)
Measure_final_age_MGM_sex<-subset(Measure_final_age, select = - c(Cisgender))


type_final_age_MGM_sex<- c("c", "g", "g",
                  "g", "g", "g", "g",
                  "g", "g", "g","g",
                  "g", "g", "g","g",
                  "g", "g", "g","g",
                  "g", "g", "g","g",
                  "g", "g", "g","g",
                  "p", "g", "g")

cat_final_age_MGM_sex <-c(length(unique(Measure_final_age$Sex)), 1, 1,
                  1, 1, 1, 1,
                  1, 1, 1,1,
                  1, 1, 1,1,
                  1, 1, 1,1,
                  1, 1, 1,1,
                  1, 1, 1,1,
                  1, 1, 1)


mgm_obj <- mgm(data = Measure_final_age_MGM_sex,
               type=type_final_age_MGM_sex,level=cat_final_age_MGM_sex,
               moderators = c(1),
               lambdaSel = "EBIC",
               lambdaGam = 0.50,
               ruleReg = "AND")



l_mgm_cond <- list()
for(g in 1:2) l_mgm_cond[[g]] <- condition(mgm_obj, values = list("1" = g))

l_mgm_cond[[1]] <- condition(mgm_obj, values = list("1" = 1))
l_mgm_cond[[2]] <- condition(mgm_obj, values = list("1" = 2))

if (identical(l_mgm_cond[[1]]$pairwise$wadj, l_mgm_cond[[2]]$pairwise$wadj)) {
  # Les objets sont identiques
  print("Les objets sont identiques.")
} else {
  # Les objets sont différents
  print("Les objets sont différents.")
}


v_max <- rep(NA, 2)
for(g in 1:2) v_max[g] <- max(l_mgm_cond[[g]]$pairwise$wadj)

png("Figures_Quest/MGM_sexcomaprisonFem.png")
graphFem3<- qgraph(input=l_mgm_cond[[1]]$pairwise$wadj,
         edge.color = l_mgm_cond[[1]] $pairwise$edgecolor,
         layout = "spring",
         maximum = max(v_max),edge.labels = TRUE ) 
dev.off()

LayeoutFem3<-averageLayout(graphFem3)
       
png("Figures_Quest/MGM_sexcomaprisonMal.png")
graphMal3<- qgraph(input=l_mgm_cond[[2]]$pairwise$wadj,
                  edge.color = l_mgm_cond[[2]]
                  $pairwise$edgecolor,
                  layout = LayeoutFem3, 
                  maximum = max(v_max), 
                  edge.labels = TRUE) 
dev.off()




png("Figures_Quest/MGM_sexcomaprison.png")
par(mfrow=c(1, 2))
for(g in 1:2) {
  qgaprhMGMcomp<-qgraph(input=l_mgm_cond[[g]]$pairwise$wadj,
                     edge.color = l_mgm_cond[[g]]
                     $pairwise$edgecolor,
                     layout = LayeoutFem3,
                     maximum = max(v_max), 
                     edge.labels = TRUE) 
mtext(text= paste0("Group ", g), line= 2.5)}
dev.off()










# création fonction comparaison NCT  --------------------


CompNet<- function (data, critere,methode , nbsep='default' ,cutlist='default', gam=0.25,abso,corr,nbit=100)
{
  if (methode== "limite")  {
    nbsep <-length(cutlist)-1
    cut_points <- cutlist }
  if (methode== "proportion"){ 
    sep=1/nbsep
    cut_points <- quantile(data[[critere]], probs = seq(0, 1, sep))
  } 
  
  Subset <- list() 
  Subset[[1]]<- subset(data[data[[critere]] <= cut_points[[2]],],select = -which(names(data) == critere)) 
  print(nrow(Subset[[1]]))
  
  for (i in 2:nbsep){
    Subset[[i]] <- subset(data[data[[critere]] > cut_points[[i]] & data[[critere]] <= cut_points[[i+1]], ],select = -which(names(data) == critere))    
    print(nrow(Subset[[i]]))
  }
  
  pairs <-combn(1:nbsep, 2)
  pairs_list <- split(pairs, rep(1:ncol(pairs), each = nrow(pairs)))
  
  NCT_list<- list()
  NCT_list[['parameters']] <- paste0(critere, nbsep,cutlist, gam,abso,corr,nbit, sep = "_")
  NCT_list[['pval_invariance']]<- list()
  NCT_list[['pval_globalstenght']]<- list()
  NCT_list[['pval_cent_betwee']]<- list()
  NCT_list[['pval_cent_stren']]<- list()
  
  
  k=0
  
  for (pair in pairs_list) {
    k=k+1
    nbA<-pair[[1]][1]
    nbB<-pair[[2]][1]
    
    NCTpair<-NCT(Subset[[nbA]], Subset[[nbB]], 
                 gamma=gam,
                 it = nbit, # The number of iterations (permutations).
                 binary.data=FALSE, 
                 paired=FALSE, 
                 weighted=TRUE, 
                 abs=abso,
                 test.edges=TRUE, 
                 edges="all", 
                 progressbar=TRUE, 
                 make.positive.definite=TRUE,
                 p.adjust.methods= c(corr), #,"holm","hochberg","hommel", "bonferroni","BH","BY","fdr"), 
                 test.centrality=TRUE, 
                 centrality=c("betweenness","strength"), #'betweenness', 'strength', 'expectedInfluence', 'bridgeStrength', 'bridgeCloseness', 'bridgeBetweenness', 'bridgeExpectedInfluence'
                 nodes="all",
                 #communities=gr3,
                 useCommunities="all",
                 #estimator,
                 #estimatorArgs = list(), 
                 verbose = TRUE)
    
    
    NCT_list$pval_invariance[[k]]<-NCTpair$nwinv.pval
    NCT_list$pval_globalstenght[[k]]<-NCTpair$glstrinv.pval
    

    p_alledge<- NCTpair$einv.pvals # p-values (corrected for multiple testing or not according to 'p.adjust.methods') per edge from the permutation test concerning differences in edges weights
    p_alledge <- as.data.frame(p_alledge)
    names(p_alledge)[3]<- 'pval'
    edgeweight_signi<- p_alledge %>% dplyr::filter(pval < 0.050) ##############"!!!!!!!!!!!!
    
    
    
    
    
    p_allcent <- NCTpair$diffcen.pval #	p-values(corrected for multiple testing or not according to 'p.adjust.methods') per node from the permutation test concerning differences in centralities. Only if test.centrality = TRUE.
    p_allcent <- as.data.frame(p_allcent )
    diffbetwee_p<-rownames(p_allcent%>% dplyr::filter(betweenness  < 0.050 ))
    diffstren_p<-rownames(p_allcent%>% dplyr::filter(strength < 0.050))
    
    diffbetwee <- subset(NCTpair$diffcen.real, row.names(NCTpair$diffcen.real) %in% diffbetwee_p)
    diffstren<- subset(NCTpair$diffcen.real, row.names(NCTpair$diffcen.real) %in% diffstren_p)
    
    NCT_list$pval_cent_betwee[[k]]<-diffbetwee
    NCT_list$pval_cent_stren[[k]]<-diffstren_p
    
    NCT_list[[ paste0(cut_points[nbA+1],"vs",cut_points[nbB+1])]]<-NCTpair
    
    print(NCTpair$nwinv.pval)
    print(NCTpair$glstrinv.pval)
    print(edgeweight_signi)
    print(diffbetwee)
    print(diffstren)
    
  }
  
  return(NCT_list)
  
}





# AGE HF 2 groups  -----------------------
set.seed(10)
#proportion 
#FemetMal_Age_2_gam50_true_none_100<-CompNet(data=Measure_final_age,critere="Age",nbsep=2,methode= "proportion",abso = TRUE,corr="none",nbit = 100,gam=0.50)
FemetMal_Age_2_gam50_true_fdr_100<-CompNet(data=Measure_final_age,critere="Age",nbsep=2,methode= "proportion",abso = TRUE,corr="fdr",nbit = 100,gam=0.50)
FemetMal_Age_2_gam50_true_bh_100<-CompNet(data=Measure_final_age,critere="Age",nbsep=2,methode= "proportion",abso = TRUE,corr="BH",nbit = 100,gam=0.50)


set.seed(11)
cutlist1<-c(90,168,350)
FemetMal_age_cut168_gam50_true_none_100<-CompNet(data=Measure_final_age,critere="Age",methode= "limite",cutlist = cutlist1,abso = FALSE,corr="none",nbit = 100,gam=0.50) 
FemetMal_age_cut168_gam50_fdr_none_100<-CompNet(data=Measure_final_age,critere="Age",methode= "limite",cutlist = cutlist1,abso = FALSE,corr="fdr",nbit = 100,gam=0.50) 
FemetMal_age_cut168_gam50_bh_none_100<-CompNet(data=Measure_final_age,critere="Age",methode= "limite",cutlist = cutlist1,abso = FALSE,corr="BH",nbit = 100,gam=0.50) 


set.seed(4)
cutlist1<-c(90,180,350)
FemetMal_age_cut180_gam50_true_none_100<-CompNet(data=Measure_final_age,critere="Age",methode= "limite",cutlist = cutlist1,abso = FALSE,corr="none",nbit = 100,gam=0.50) 
## diff edge et betweeness           




# AGE HF >2 groups --------------

FemetMal_Age_3_gam50_true_none_100<-CompNet(data=Measure_final_age,critere="Age",nbsep=3,methode= "proportion",abso = TRUE,corr="none",nbit = 100,gam=0.50)
FemetMal_Age_4_gam50_true_none_100<-CompNet(data=Measure_final_age,critere='Age',nbsep=4,methode= "proportion",abso = TRUE,corr="none",nbit = 100,gam=0.50)
#########CELUI LA SUPER !!!!!!!!!!!!!!!!!!!!!!!

FemetMal_Age_3_gam50_true_fdr_100<-CompNet(data=Measure_final_age,critere="Age",nbsep=3,methode= "proportion",abso = TRUE,corr="fdr",nbit = 100,gam=0.50)

set.seed(13)
FemetMal_Age_4_gam50_true_fdr_100<-CompNet(data=Measure_final_age,critere='Age',nbsep=4,methode= "proportion",abso = TRUE,corr="fdr",nbit = 100,gam=0.50)

FemetMal_Age_4_gam50_true_fdr_1000<-CompNet(data=Measure_final_age,critere='Age',nbsep=4,methode= "proportion",abso = TRUE,corr="fdr",nbit = 1000,gam=0.50)



# AGE Sex séparé 2*2 groups   -----------------------















cutlist1<-c(1,2.8,4)
Fem_age_cut2.8_gam40_true_none_100<-CompNet(data=Measure_Fem_age,critere="Age",methode= "limite",cutlist = cutlist1,abso = FALSE,corr="none",nbit = 100,gam=0.50) 

cutlist1<-c(1,3,4)
Fem_age_cut3.0_gam40_true_none_100<-CompNet(data=Measure_Fem_age,critere="Age",methode= "limite",cutlist = cutlist1,abso = FALSE,corr="none",nbit = 100,gam=0.50) 

cutlist1<-c(1,3.2,4)
Fem_age_cut3.2_gam40_true_none_100<-CompNet(data=Measure_Fem_age,critere="Age",methode= "limite",cutlist = cutlist1,abso = FALSE,corr="none",nbit = 100,gam=0.50) 

cutlist1<-c(1,3.4,4)
Fem_age_cut3.4_gam40_true_none_100<-CompNet(data=Measure_Fem_age,critere="Age",methode= "limite",cutlist = cutlist1,abso = FALSE,corr="none",nbit = 100,gam=0.50) 

cutlist1<-c(1,3.6,4)
Fem_age_cut3.6_gam40_true_none_100<-CompNet(data=Measure_Fem_age,critere="Age",methode= "limite",cutlist = cutlist1,abso = FALSE,corr="none",nbit = 100,gam=0.50) 










###!!!!!!!!!!!!!!!! significatif
Fem_Age_4_gam50_false_none_100<-CompNet(data=Measure_Fem_age,critere='Age',nbsep=4,methode= "proportion",abso = FALSE,corr="none",nbit = 100,gam=0.50)
Fem_Age_4_gam50_false_none_1000<-CompNet(data=Measure_Fem_age,critere='Age',nbsep=4,methode= "proportion",abso = FALSE,corr="none",nbit = 1000,gam=0.50)
Fem_Age_4_gam50_false_fdr_1000<-CompNet(data=Measure_Fem_age,critere='Age',nbsep=4,methode= "proportion",abso = FALSE,corr="fdr",nbit = 1000,gam=0.50)



Fem_Age_cut3_gam50_false_none_1000<-CompNet(data=Measure_Fem_age,critere="Age",methode= "limite",cutlist = cutlist3,abso = FALSE,corr="none",nbit = 1000,gam=0.50) 
Fem_Age_cut3_gam50_false_none_1000<-CompNet(data=Measure_Fem_age,critere="Age",methode= "limite",cutlist = cutlist3,abso = FALSE,corr="fdr",nbit = 1000,gam=0.50) 







Fem_Age_3_gam50_false_none_100<-CompNet(data=Measure_Fem_age,critere="Age",nbsep=3,methode= "proportion",abso = FALSE,corr="none",nbit = 100,gam=0.50)
##diffff 


Fem_Age_2_gam25_true_none_100<-CompNet(data=Measure_Fem_age,critere="Age",nbsep=2,methode= "proportion",abso = TRUE,corr="none",nbit = 100,gam=0.25)
Fem_Age_3_gam25_true_none_100<-CompNet(data=Measure_Fem_age,critere="Age",nbsep=3,methode= "proportion",abso = TRUE,corr="none",nbit = 100,gam=0.25)
Fem_Age_4_gam25_true_none_100<-CompNet(data=Measure_Fem_age,critere='Age',nbsep=4,methode= "proportion",abso = TRUE,corr="none",nbit = 100,gam=0.25)
Fem_Age_5_gam25_true_none_100<-CompNet(data=Measure_Fem_age,critere='Age',nbsep=5,methode= "proportion",abso = TRUE,corr="none",nbit = 100,gam=0.25)



Fem_Age_2_gam50_false_none_100<-CompNet(data=Measure_Fem_age,critere="Age",nbsep=2,methode= "proportion",abso = FALSE,corr="none",nbit = 100,gam=0.50)
Fem_Age_5_gam50_false_none_100<-CompNet(data=Measure_Fem_age,critere='Age',nbsep=5,methode= "proportion",abso = FALSE,corr="none",nbit = 100,gam=0.50)

Fem_Pub_2_gam25_true_none<-CompNet(data=Measure_final_pub,critere='Puberty',nbsep=2,methode= "proportion",abso = TRUE,corr="none",nbit = 100,gam=0.25)
Fem_Pub_3_gam25_true_none<-CompNet(data=Measure_final_pub,critere='Puberty',nbsep=3,methode= "proportion",abso = TRUE,corr="none",nbit = 100,gam=0.25)
Fem_Pub_4_gam25_true_none<-CompNet(data=Measure_final_pub,critere='Puberty',nbsep=4,methode= "proportion",abso = TRUE,corr="none",nbit = 100,gam=0.25)
Fem_Pub_5_gam25_true_none<-CompNet(data=Measure_final_pub,critere='Puberty',nbsep=5,methode= "proportion",abso = TRUE,corr="none",nbit = 100,gam=0.25)

Fem_Age_2_gam50_true_none_100<-CompNet(data=Measure_Fem,critere="Age",nbsep=2,methode= "proportion",abso = TRUE,corr="none",nbit = 100,gam=0.50)
Fem_Age_3_gam50_true_none_100<-CompNet(data=Measure_Fem,critere="Age",nbsep=3,methode= "proportion",abso = TRUE,corr="none",nbit = 100,gam=0.50)
Fem_Age_4_gam50_true_none_100<-CompNet(data=Measure_Fem,critere='Age',nbsep=4,methode= "proportion",abso = TRUE,corr="none",nbit = 100,gam=0.50)
Fem_Age_5_gam50_true_none_100<-CompNet(data=Measure_Fem,critere='Age',nbsep=5,methode= "proportion",abso = TRUE,corr="none",nbit = 100,gam=0.50)

Fem_Pub_2_gam50_true_none<-CompNet(data=Measure_final_pub,critere='Puberty',nbsep=2,methode= "proportion",abso = TRUE,corr="none",nbit = 100,gam=0.50)
Fem_Pub_3_gam50_true_none<-CompNet(data=Measure_final_pub,critere='Puberty',nbsep=3,methode= "proportion",abso = TRUE,corr="none",nbit = 100,gam=0.50)
Fem_Pub_4_gam50_true_none<-CompNet(data=Measure_final_pub,critere='Puberty',nbsep=4,methode= "proportion",abso = TRUE,corr="none",nbit = 100,gam=0.50)
Fem_Pub_5_gam50_true_none<-CompNet(data=Measure_final_pub,critere='Puberty',nbsep=5,methode= "proportion",abso = TRUE,corr="none",nbit = 100,gam=0.50)


cutlist1<-c(108,162,204,246,300)
cutlist2<-c(108,144,168,192,216,240)

cutlist3<-c(100,175,210,237,237,316)
Fem_Age_cut3_gam25_true_none_100<-CompNet(data=Measure_Fem_age,critere="Age",methode= "limite",cutlist = cutlist3,abso = TRUE,corr="none",nbit = 100,gam=0.25) 

# Age cutlist
Fem_Age_cut1_gam25_true_none_100<-CompNet(data=Measure_Fem_age,critere="Age",methode= "limite",cutlist = cutlist1,abso = TRUE,corr="none",nbit = 100,gam=0.25) 
Fem_Age_cut2_gam25_true_none_100<-CompNet(data=Measure_Fem_age,critere="Age",methode= "limite",cutlist = cutlist2,abso = TRUE,corr="none",nbit = 100,gam=0.25)


Fem_Age_cut1_gam25_true_none_100_toutemesure<-CompNet(data=Measure_Fem_age,critere="Age",cutlist = cutlist1,abso = TRUE,corr="none",nbit = 100,gam=0.25) #!!!!!!!!!! ici diffff global strength
Fem_Age_cut2_gam25_true_none_100_toutemesure<-CompNet(data=Measure_Fem_age,critere="Age",cutlist = cutlist2,abso = TRUE,corr="none",nbit = 100,gam=0.25)

Fem_Pub_cut1_gam25_true_none<-CompNet(data=Measure_Fem,critere='Puberty',cutlist = cutlist1,abso = TRUE,corr="none",nbit = 100,gam=0.25)
Fem_Pub_cut2_gam25_true_none<-CompNet(data=Measure_Fem,critere='Puberty',cutlist = cutlist2,abso = TRUE,corr="none",nbit = 100,gam=0.25)

Fem_Age_cut1_gam50_true_none_100<-CompNet(data=Measure_Fem,critere="Age",cutlist = cutlist1,abso = TRUE,corr="none",nbit = 100,gam=0.50)
Fem_Age_cut2_gam50_true_none_100<-CompNet(data=Measure_Fem,critere="Age",cutlist = cutlist2,abso = TRUE,corr="none",nbit = 100,gam=0.50)

Fem_Pub_cut1_gam50_true_none<-CompNet(data=Measure_Fem,critere='Puberty',cutlist = cutlist1,abso = TRUE,corr="none",nbit = 100,gam=0.50)
Fem_Pub_cut2_gam50_true_none<-CompNet(data=Measure_Fem,critere='Puberty',cutlist = cutlist2,abso = TRUE,corr="none",nbit = 100,gam=0.50)













# AGE Mal 2 niveau   -----------------------

cutlist1<-c(1,2.8,4)
Mal_age_cut2.8_gam40_true_none_100<-CompNet(data=Measure_Mal_age,critere="Age",methode= "limite",cutlist = cutlist1,abso = FALSE,corr="none",nbit = 100,gam=0.50) 

cutlist1<-c(1,3,4)
Mal_age_cut3.0_gam40_true_none_100<-CompNet(data=Measure_Mal_age,critere="Age",methode= "limite",cutlist = cutlist1,abso = FALSE,corr="none",nbit = 100,gam=0.50) 

cutlist1<-c(1,3.2,4)
Mal_age_cut3.2_gam40_true_none_100<-CompNet(data=Measure_Mal_age,critere="Age",methode= "limite",cutlist = cutlist1,abso = FALSE,corr="none",nbit = 100,gam=0.50) 

cutlist1<-c(1,3.4,4)
Mal_age_cut3.4_gam40_true_none_100<-CompNet(data=Measure_Mal_age,critere="Age",methode= "limite",cutlist = cutlist1,abso = FALSE,corr="none",nbit = 100,gam=0.50) 

cutlist1<-c(1,3.6,4)
Mal_age_cut3.6_gam40_true_none_100<-CompNet(data=Measure_Mal_age,critere="Age",methode= "limite",cutlist = cutlist1,abso = FALSE,corr="none",nbit = 100,gam=0.50) 








# PUB 2 niveau HF  -----------------------

Pub1 <- Measure_final_pub[Measure_final_pub$Puberty <= cutlist1[2], ]
nrow(Pub1)
Pub2 <- Measure_final_pub[Measure_final_pub$Puberty > cutlist1[2],]
nrow(Pub2)

cutlist1<-c(1,2.8,4)
FemetMal_Pub_cut2.8_gam40_true_none_100<-CompNet(data=Measure_final_pub,critere="Puberty",methode= "limite",cutlist = cutlist1,abso = FALSE,corr="none",nbit = 100,gam=0.50) 
#[1] "Cisgender"

cutlist1<-c(1,3,4)
FemetMal_Pub_cut3.0_gam40_true_none_100<-CompNet(data=Measure_final_pub,critere="Puberty",methode= "limite",cutlist = cutlist1,abso = FALSE,corr="none",nbit = 100,gam=0.50) 
# Cisgender                       -28 -0.0205602
#Body_Esteem_Attribution         -54 -0.3265546
#Body_Listening                  -47 -0.3162633
#[1] "Sex"                  "Physical_Activity"    "Social_Anxiety"       "Body_Objectification" "Body_Trusting" 


cutlist1<-c(1,3.2,4)
FemetMal_Pub_cut3.2_gam40_true_none_100<-CompNet(data=Measure_final_pub,critere="Puberty",methode= "limite",cutlist = cutlist1,abso = FALSE,corr="none",nbit = 100,gam=0.50) 
##Cisgender                        -5 -0.04248587
#Body_Esteem_Attribution         -29 -0.11396233
#Body_Listening                  -65 -0.13770557
#Chronic_Pain                    -22 -0.09708018
#[1] "Sex"               "Cisgender"         "Physical_Activity" "Body_Trusting" 


cutlist1<-c(1,3.4,4)
FemetMal_Pub_cut3.4_gam40_true_none_100<-CompNet(data=Measure_final_pub,critere="Puberty",methode= "limite",cutlist = cutlist1,abso = FALSE,corr="none",nbit = 100,gam=0.50) 
##  betweenness   strength
#Body_Listening         -52 -0.1298416
#[1] "Body_Objectification" "Body_Trusting" 

cutlist1<-c(1,3.6,4)
FemetMal_Pub_cut3.6_gam40_true_none_100<-CompNet(data=Measure_final_pub,critere="Puberty",methode= "limite",cutlist = cutlist1,abso = FALSE,corr="none",nbit = 100,gam=0.50) 


# PUB 2 niveau Fem  -----------------------

cutlist1<-c(1,2.8,4)
Fem_Pub_cut2.8_gam40_true_none_100<-CompNet(data=Measure_Fem_pub,critere="Puberty",methode= "limite",cutlist = cutlist1,abso = FALSE,corr="none",nbit = 100,gam=0.50) 

cutlist1<-c(1,3,4)
Fem_Pub_cut3.0_gam40_true_none_100<-CompNet(data=Measure_Fem_pub,critere="Puberty",methode= "limite",cutlist = cutlist1,abso = FALSE,corr="none",nbit = 100,gam=0.50) 

cutlist1<-c(1,3.2,4)
Fem_Pub_cut3.2_gam40_true_none_100<-CompNet(data=Measure_Fem_pub,critere="Puberty",methode= "limite",cutlist = cutlist1,abso = FALSE,corr="none",nbit = 100,gam=0.50) 

cutlist1<-c(1,3.4,4)
Fem_Pub_cut3.4_gam40_true_none_100<-CompNet(data=Measure_Fem_pub,critere="Puberty",methode= "limite",cutlist = cutlist1,abso = FALSE,corr="none",nbit = 100,gam=0.50) 

cutlist1<-c(1,3.6,4)
Fem_Pub_cut3.6_gam40_true_none_100<-CompNet(data=Measure_Fem_pub,critere="Puberty",methode= "limite",cutlist = cutlist1,abso = FALSE,corr="none",nbit = 100,gam=0.50) 


# PUB 2 niveau Mal  -----------------------

cutlist1<-c(1,2.8,4)
Mal_Pub_cut2.8_gam40_true_none_100<-CompNet(data=Measure_Mal_pub,critere="Puberty",methode= "limite",cutlist = cutlist1,abso = FALSE,corr="none",nbit = 100,gam=0.50) 

cutlist1<-c(1,3,4)
Mal_Pub_cut3.0_gam40_true_none_100<-CompNet(data=Measure_Mal_pub,critere="Puberty",methode= "limite",cutlist = cutlist1,abso = FALSE,corr="none",nbit = 100,gam=0.50) 

cutlist1<-c(1,3.2,4)
Mal_Pub_cut3.2_gam40_true_none_100<-CompNet(data=Measure_Mal_pub,critere="Puberty",methode= "limite",cutlist = cutlist1,abso = FALSE,corr="none",nbit = 100,gam=0.50) 

cutlist1<-c(1,3.4,4)
Mal_Pub_cut3.4_gam40_true_none_100<-CompNet(data=Measure_Mal_pub,critere="Puberty",methode= "limite",cutlist = cutlist1,abso = FALSE,corr="none",nbit = 100,gam=0.50) 

cutlist1<-c(1,3.6,4)
Mal_Pub_cut3.6_gam40_true_none_100<-CompNet(data=Measure_Mal_pub,critere="Puberty",methode= "limite",cutlist = cutlist1,abso = FALSE,corr="none",nbit = 100,gam=0.50) 










# PUB 4 niveau -------------------

Measure_final_pub<-subset(Measure_Network, select = - c(Age))

cut_points <- quantile(Measure_final_pub$Puberty, probs = seq(0, 1, 0.25))


Pub1 <- Measure_final_pub[Measure_final_pub$Puberty <= cut_points[[2]], ]
Pub2 <- Measure_final_pub[Measure_final_pub$Puberty > cut_points[[2]] & Measure_final_pub$Puberty <= cut_points[[3]],]
Pub3 <- Measure_final_pub[Measure_final_pub$Puberty > cut_points[[3]] & Measure_final_pub$Puberty <= cut_points[[4]],]
Pub4 <- Measure_final_pub[Measure_final_pub$Puberty  > cut_points[[4]],]


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


Pub1.cor<-cor_auto(Pub1) #compute the correlation Matrix with qgraph Packages 
Pub4.cor<-cor_auto(Pub4) #compute the correlation Matrix with qgraph Packages 






qgraph(Pub1.cor, graph="glasso", layout="spring",labels=labels_sex,
       maximum=.45,minimum=.03,tuning=0.25,
       vsize=7, cut=0,  sampleSize = nrow(Pub1),
       border.width=0.1, border.color="#a8a8a8",  
       groups=gr_sexe, color=c("#377eb8", "#fb9a99", "#4daf4a", "#ffffbf", "#ff7f00", "#ffff33", "#bcf60c", "#c77cff", "#a65628", 
                               "#66c2a5", "#fc8d62", "#46f0f0", "#e78ac3"),title= "Whole sample Network",
       legend=TRUE,legend.mode='style1',GLratio=2.5,layoutScale=1,legend.cex=0.8,
       nodeNames=nodenames_sex)





# **  Pub1vs2  -------------

NCTPub1vs2<-NCT(Pub1, Pub2, 
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


### test diff global strength.
p_diffstrenght_Pub1vs2 <- NCTPub1vs2$glstrinv.pval # 	 The p value resulting from the permutation test concerning difference in global strength.

### test diff global edge weight
p_globaledgeweight_Pub1vs2  <- NCTPub1vs2$nwinv.pval  # The p value resulting from the permutation test concerning the maximum difference in edge weights.


###test & trouver diff all edge weights.
p_alledgeweight_Pub1vs2  <- NCTPub1vs2$einv.pvals # p-values (corrected for multiple testing or not according to 'p.adjust.methods') per edge from the permutation test concerning differences in edges weights
p_alledgeweight_Pub1vs2  <- as.data.frame(p_alledgeweight_Pub1vs2 )
names(p_alledgeweight_Pub1vs2 )[3]<- 'pval'
edgeweight_signi_Pub1vs2  <- p_alledgeweight_Pub1vs2  %>% dplyr::filter(pval < 0.050)

###trouver diffférence de centralité
p_allcent_Pub1vs2  <- NCTPub1vs2$diffcen.pval #	p-values(corrected for multiple testing or not according to 'p.adjust.methods') per node from the permutation test concerning differences in centralities. Only if test.centrality = TRUE.
p_allcent_Pub1vs2 <- as.data.frame(p_allcent_Pub1vs2 )
diffbetwee_signi_Pub1vs2_p<-rownames(p_allcent_Pub1vs2%>% dplyr::filter(betweenness  < 0.050 ))
diffstren_cent_signi_Pub1vs2_p<-rownames(p_allcent_Pub1vs2%>% dplyr::filter(strength < 0.050))

diffbetwee_signi_Pub1vs2 <- subset(NCTPub1vs2$diffcen.real, row.names(NCTPub1vs2$diffcen.real) %in% diffbetwee_signi_Pub1vs2_p)
diffstren_cent_signi_Pub1vs2 <- subset(NCTPub1vs2$diffcen.real, row.names(NCTPub1vs2$diffcen.real) %in% diffstren_cent_signi_Pub1vs2_p)



# **  Pub2vs3  -------------

NCTPub2vs3<-NCT(Pub2, Pub3, 
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


### test diff global strength.
p_diffstrenght_Pub2vs3 <- NCTPub2vs3$glstrinv.pval # 	 The p value resulting from the permutation test concerning difference in global strength.

### test diff global edge weight
p_globaledgeweight_Pub2vs3  <- NCTPub2vs3$nwinv.pval  # The p value resulting from the permutation test concerning the maximum difference in edge weights.


###test & trouver diff all edge weights.
p_alledgeweight_Pub2vs3  <- NCTPub2vs3$einv.pvals # p-values (corrected for multiple testing or not according to 'p.adjust.methods') per edge from the permutation test concerning differences in edges weights
p_alledgeweight_Pub2vs3  <- as.data.frame(p_alledgeweight_Pub2vs3 )
names(p_alledgeweight_Pub2vs3 )[3]<- 'pval'
edgeweight_signi_Pub2vs3  <- p_alledgeweight_Pub2vs3  %>% dplyr::filter(pval < 0.050)

###trouver diffférence de centralité
p_allcent_Pub2vs3  <- NCTPub2vs3$diffcen.pval #	p-values(corrected for multiple testing or not according to 'p.adjust.methods') per node from the permutation test concerning differences in centralities. Only if test.centrality = TRUE.
p_allcent_Pub2vs3 <- as.data.frame(p_allcent_Pub2vs3 )
diffbetwee_signi_Pub2vs3_p<-rownames(p_allcent_Pub2vs3%>% dplyr::filter(betweenness  < 0.050 ))
diffstren_cent_signi_Pub2vs3_p<-rownames(p_allcent_Pub2vs3%>% dplyr::filter(strength < 0.050))

diffbetwee_signi_Pub2vs3 <- subset(NCTPub2vs3$diffcen.real, row.names(NCTPub2vs3$diffcen.real) %in% diffbetwee_signi_Pub2vs3_p)
diffstren_cent_signi_Pub2vs3 <- subset(NCTPub2vs3$diffcen.real, row.names(NCTPub2vs3$diffcen.real) %in% diffstren_cent_signi_Pub2vs3_p)



# **  Pub3vs4  -------------


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
summary(NCTPub3vsPub4)



# **  Pub1vs3  -------------



NCTPub1vs3<-NCT(Pub1, Pub3, 
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


### test diff global strength.
p_diffstrenght_Pub1vs3 <- NCTPub1vs3$glstrinv.pval # 	 The p value resulting from the permutation test concerning difference in global strength.

### test diff global edge weight
p_globaledgeweight_Pub1vs3  <- NCTPub1vs3$nwinv.pval  # The p value resulting from the permutation test concerning the maximum difference in edge weights.


###test & trouver diff all edge weights.
p_alledgeweight_Pub1vs3  <- NCTPub1vs3$einv.pvals # p-values (corrected for multiple testing or not according to 'p.adjust.methods') per edge from the permutation test concerning differences in edges weights
p_alledgeweight_Pub1vs3  <- as.data.frame(p_alledgeweight_Pub1vs3 )
names(p_alledgeweight_Pub1vs3 )[3]<- 'pval'
edgeweight_signi_Pub1vs3  <- p_alledgeweight_Pub1vs3  %>% dplyr::filter(pval < 0.050)

###trouver diffférence de centralité
p_allcent_Pub1vs3  <- NCTPub1vs3$diffcen.pval #	p-values(corrected for multiple testing or not according to 'p.adjust.methods') per node from the permutation test concerning differences in centralities. Only if test.centrality = TRUE.
p_allcent_Pub1vs3 <- as.data.frame(p_allcent_Pub1vs3 )
diffbetwee_signi_Pub1vs3_p<-rownames(p_allcent_Pub1vs3%>% dplyr::filter(betweenness  < 0.050 ))
diffstren_cent_signi_Pub1vs3_p<-rownames(p_allcent_Pub1vs3%>% dplyr::filter(strength < 0.050))

diffbetwee_signi_Pub1vs3 <- subset(NCTPub1vs3$diffcen.real, row.names(NCTPub1vs3$diffcen.real) %in% diffbetwee_signi_Pub1vs3_p)
diffstren_cent_signi_Pub1vs3 <- subset(NCTPub1vs3$diffcen.real, row.names(NCTPub1vs3$diffcen.real) %in% diffstren_cent_signi_Pub1vs3_p)



# **  Pub1vs4  -------------



NCTPub1vs4<-NCT(Pub1, Pub4, 
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


### test diff global strength.
p_diffstrenght_Pub1vs4 <- NCTPub1vs4$glstrinv.pval # 	 The p value resulting from the permutation test concerning difference in global strength.

### test diff global edge weight
p_globaledgeweight_Pub1vs4  <- NCTPub1vs4$nwinv.pval  # The p value resulting from the permutation test concerning the maximum difference in edge weights.


###test & trouver diff all edge weights.
p_alledgeweight_Pub1vs4  <- NCTPub1vs4$einv.pvals # p-values (corrected for multiple testing or not according to 'p.adjust.methods') per edge from the permutation test concerning differences in edges weights
p_alledgeweight_Pub1vs4  <- as.data.frame(p_alledgeweight_Pub1vs4 )
names(p_alledgeweight_Pub1vs4 )[3]<- 'pval'
edgeweight_signi_Pub1vs4  <- p_alledgeweight_Pub1vs4  %>% dplyr::filter(pval < 0.050)

###trouver diffférence de centralité
p_allcent_Pub1vs4  <- NCTPub1vs4$diffcen.pval #	p-values(corrected for multiple testing or not according to 'p.adjust.methods') per node from the permutation test concerning differences in centralities. Only if test.centrality = TRUE.
p_allcent_Pub1vs4 <- as.data.frame(p_allcent_Pub1vs4 )
diffbetwee_signi_Pub1vs4_p<-rownames(p_allcent_Pub1vs4%>% dplyr::filter(betweenness  < 0.050 ))
diffstren_cent_signi_Pub1vs4_p<-rownames(p_allcent_Pub1vs4%>% dplyr::filter(strength < 0.050))

diffbetwee_signi_Pub1vs4 <- subset(NCTPub1vs4$diffcen.real, row.names(NCTPub1vs4$diffcen.real) %in% diffbetwee_signi_Pub1vs4_p)
diffstren_cent_signi_Pub1vs4 <- subset(NCTPub1vs4$diffcen.real, row.names(NCTPub1vs4$diffcen.real) %in% diffstren_cent_signi_Pub1vs4_p)




































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



































load("../R_Env_Quest/Fem_Age_cut1_gam25_true_none_100_RESULTSBIEN.RData")

save.image("../R_Env_Quest/NetworkAnalyseTOUT.RData")




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
