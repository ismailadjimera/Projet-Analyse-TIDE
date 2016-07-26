# Création du répertoire de travail deouis votre PC pour réduire le temps de calcul
setwd("//AD/Liste/Projets/SGLS/TRANSVERSE/EMMS/Customer Base Management/Reporting LMS/Reporting LMS décembre/Extract_KPIs/conversion_KPIs_csv")



#inserer les fichiers d'intersec après les avoir enregistrer sous format CSV
SN_08 <- read.csv("Senegal-2016-01-05 - Raw_KPIs.csv", sep=";")
CF_08 <- read.csv("RCA-2016-01-01 - Raw_KPIs.csv", sep=";")
NE_08 <- read.csv("Niger-2016-01-01 - Raw_KPIs.csv", sep=";")
MU_08 <- read.csv("Mauritius-2016-01-01 - Raw_KPIs.csv", sep=";")
ML_08 <- read.csv("Mali-2016-01-05 - Raw_KPIs.csv", sep=";")
MG_08 <- read.csv("Madagascar-2016-01-05 - Raw_KPIs.csv", sep=";")
JO_08 <- read.csv("Jordan-2016-01-05 - Raw_KPIs.csv", sep=";")
GN_08 <- read.csv("Conakry-2016-01-01 - Raw_KPIs.csv", sep=";")
CM_08 <- read.csv("Cameroun-2016-01-01 - Raw_KPIs.csv", sep=";")
#MA_08 <- read.csv("ReportingMorocco-2015-09-01 - Raw_KPIs.csv", sep=";")
CD_08 <- read.csv("Congo-2016-01-05 - Raw_KPIs.csv", sep=";")
GW_08 <- read.csv("GuineeBissau-2016-01-05 - Raw_KPIs.csv", sep=";")
BW_08 <- read.csv("Botswana-2016-01-01 - Raw_KPIs.csv", sep=";")
KE_08 <- read.csv("Kenya-2016-01-01 - Raw_KPIs.csv", sep=";")

#############################################################################################
# Fonction                                                                                  #
#   pays(SN_05,code)                                                                        #
#                                                                                           #
# Description                                                                               #
#	 Récupère et crée les indicateurs du reporting du reporting et rajoute un code pays       #
#  et un identifiant pour chaque fichier mensuel reçu                                       #
#                                                                                           #
# Arguments                                                                                 #
#	SN_O5 - Nom de la table d'entré                                                           #
#	code -  Code ISO du pays                                                                  #
#                                                                                           #
# Sortie de la fonction                                                                     #
#	La fonction retourne une table contenant tous les indicateurs utilisés pour le reporting  #  
#                                                                                           #
#############################################################################################


pays=function(SN_05,code){
  SN_5<-SN_05
  # Création de l'identifiant
  SN_5$country<-code
  SN_5$ID <- do.call(paste, c(SN_5[c("country", "Id")], sep = ""))
  # Selection des variables d'intéret: Ajouter nom des variables si d'autres indicateurs sont inclus
  SN_5<-as.data.frame(SN_5[,c("ID", "country","Name","State",  "TARGET.SIZE","CONTROL.GROUP.SIZE",
                                "CAMPAIGN.START","CAMPAIGN.STOP","CONTROL.GROUP.MODE",
                                "BONUS.MODULE","TAKERS","NATURAL.TAKERS","Blacklist.size","Whitelist.size",
                                "Seedlist.size","subsAwarded","subsFailed","bonusStart",
                                "bonusStop")])
  #Conversion en format date des variables de date
  SN_5$CAMPAIGN.START<-as.Date(SN_5$CAMPAIGN.START,"%Y-%m-%d")
  SN_5$CAMPAIGN.STOP<-as.Date(SN_5$CAMPAIGN.STOP, "%Y-%m-%d")
  #Selection et filtre des données de l'année en cours
  SN_5<-SN_5[ which(format(SN_5$CAMPAIGN.STOP, "%Y")=="2015"),]
  #Création de la variable de mois
  SN_5$Mois<-format(SN_5$CAMPAIGN.STOP, "%b")
  SN_5$duree=SN_5$CAMPAIGN.STOP-SN_5$CAMPAIGN.START
  
  attach(SN_5)
  SN_5$type_duree[duree < 8] <- "Less than 8 days"
  SN_5$type_duree[duree >= 8] <- "More than 8 days"
  detach(SN_5)
  
  SN_5$GT<-ifelse(SN_5$CONTROL.GROUP.MODE=="NONE","WITHOUT CG","WITH CG")
  SN_5$Nature_takers<-ifelse(SN_5$TAKERS==0,"No Taker","At least 1 taker")
  # Take_up_rate
  SN_5$Take_up_rate<-SN_5$TAKERS/SN_5$TARGET.SIZE
  
  attach(SN_5)
  SN_5$c_take_up_rate[Take_up_rate > 0.3] <- "> 30%"
  SN_5$c_take_up_rate[Take_up_rate <= 0.3] <- "21<= TR <=30%"
  SN_5$c_take_up_rate[Take_up_rate <= 0.2] <- "11<= TR <=20%"
  SN_5$c_take_up_rate[Take_up_rate <= 0.1] <- "6<= TR <=10%"
  SN_5$c_take_up_rate[Take_up_rate <= 0.05] <- "1<= TR <=5%"
  SN_5$c_take_up_rate[Take_up_rate < 0.01] <- "< 1%"
  detach(SN_5)
  
  SN_5$Natural_Take_up_rate<-SN_5$NATURAL.TAKERS/SN_5$CONTROL.GROUP.SIZE
  SN_5$Delta_take_up_rate<-SN_5$Take_up_rate-SN_5$Natural_Take_up_rate
  SN_5$Delta<-100*SN_5$Delta_take_up_rate
  
  SN_5$Blacklist<-ifelse(SN_5$Blacklist.size>0,"With Blacklist","Without Blacklist")
  SN_5$Whitelist<-ifelse(SN_5$Whitelist.size>0,"With Whitelist","Without Whitelist")
  SN_5$Seedlist<-ifelse(SN_5$Seedlist.size>0,"With Seedlist","Without Seedlist")
  SN_5$failedAward<-ifelse(SN_5$subsFailed>0,"With failed awarding","Without failed awarding")
  SN_5$failed_rate=100*SN_5$subsFailed/SN_5$TARGET.SIZE
  SN=data.frame(SN_5)
  return(SN)
}


SN_08 <- pays(SN_08,"SN")
CF_08 <- pays(CF_08,"CF")
NE_08 <- pays(NE_08,"NE")
MU_08 <- pays(MU_08,"MU")
ML_08 <- pays(ML_08,"ML")
JO_08 <- pays(JO_08,"JO")
GN_08 <- pays(GN_08,"GN")
CM_08 <- pays(CM_08,"CM")
#MA_08 <- pays(MA_08,"MA")
CD_08 <- pays(CD_08,"CD")
GW_08 <- pays(GW_08,"GW")
MG_08 <- pays(MG_08,"MG")
BW_08 <- pays(BW_08,"BW")
KE_08 <- pays(KE_08,"KE")



# Superposition des tables et suppression des doublons
reporting_aout<-unique(do.call(rbind, list(SN_08,
                                           CF_08,
                                           NE_08,
                                           MU_08,
                                           ML_08,
                                           JO_08,
                                           GN_08,
                                           CM_08,
                                           #MA_08,
                                           CD_08,
                                           GW_08,
                                           MG_08,
                                           BW_08,
                                           KE_08)))

table(reporting_aout$Mois)
#Supression des données du mois de septembre qui est non concerné
reporting_aout<-reporting_aout[ which(reporting_aout$Mois!="janv.") | (SN_5$CAMPAIGN.START, "%Y")=="2016"),]

# Flag des campagnes de réattribution de bonus : on flag les campagnes contenant les mots clés suivant
reporting_aout$reattrib<-grepl("BonusFailed|bonusfailed|Bonusfailed|bonusFailed|Bonus failed|bonus failed|Bonus Failed|Remboursement|remboursement|Incident|incident|INCIDENT|echec|Echec|échec|ECHEC|reattribution|réattribution|Reattribution|REATTRIBUTION|Réattribution|reclamations|réclamations|reclamation|réclamation|Reclamations|Réclamations|Reclamation|Réclamation|reassignement|Reassignement|Injection|injection|Injections|injections|bonus not credited|restitution de bonus",
                               reporting_aout$Name)
#Flag des campagnes test
reporting_aout$Test<-grepl("Test|test|TEST",reporting_aout$Name)

table(reporting_aout$country)
rownames(reporting_aout) <- NULL


write.table(reporting_aout, "reporting aout.txt", sep="\t") 
write.csv(reporting_aout, file = "reporting_lms_test.csv")
write.xlsx(reporting_aout, "reporting aout.xlsx", sheetName="Données brutes",
           col.names=TRUE, row.names=FALSE, append=FALSE)
