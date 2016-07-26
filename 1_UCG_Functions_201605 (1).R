
#*********************************************
# This macro allows to compute a             *
# value base segmentation based on           *
# AMEA definition rates    the thresholds    *
# here are different your value segments     *
#The argument is the global CBM base         *
#*********************************************


segval<-function(cbm_base,base_gtu){
  
  
  names(cbm_base)[c(1,2,3,4)]<-c("SUBS_ID","ARPU_M1","ARPU_M2","ARPU_M3") 
  n1=nrow(cbm_base)

  cbm_base<-cbm_base[!duplicated(cbm_base[,1]),]
  n2=nrow(cbm_base)
  
  doublons_cbm=n1-n2
  
  n3=nrow(base_gtu)
  base_gtu<-base_gtu[!duplicated(base_gtu[,1]),]
  base_gtu<-as.data.frame(base_gtu)
  n4=nrow(base_gtu)
  
  doublons_gtu=n3-n4
  

  
  cbm_base[is.na(cbm_base)] <- 0
  
  cbm_base$CONSO_3M=(cbm_base$ARPU_M1+cbm_base$ARPU_M2+cbm_base$ARPU_M3)/3
  
  cbm_base<-cbm_base[cbm_base$CONSO_3M>=0,]
  
  vbs<-subset(cbm_base, select=c(SUBS_ID, CONSO_3M))
  
  vbso<-vbs[order(vbs[,2],decreasing=FALSE),]
  
  a<-data.frame(table(vbso$CONSO_3M))
  a$ARPU<-as.numeric(as.character(a$Var1))
  a <- subset(a, select=c(ARPU,Freq))
  
  
  a$frequence<-a$Freq/sum(a$Freq)
  a$cumul<-cumsum(a$frequence)*100
  
  
  attach(a)
  #detach(a)
  a$seg[cumul <= 57 ] <- "LV"
  a$seg[cumul <= 87 & cumul > 57 ] <- "MV"
  a$seg[cumul <= 97 & cumul > 87] <- "HV"
  a$seg[cumul > 97] <- "SHV"
  a$seg[ARPU == 0] <- "ZERO"
  detach(a)
  
  
  
  aa<-as.data.frame(quantile(cbm_base[cbm_base$CONSO_3M!=0 & cbm_base$CONSO_3M <= max(a$ARPU[a$seg=="HV"]),]$CONSO_3M, probs=0:4/4))
  
  
  attach(cbm_base)
  cbm_base$segm[cbm_base$CONSO_3M == 0] <- "ZEROS"
  cbm_base$segm[cbm_base$CONSO_3M <= aa[2,] & cbm_base$CONSO_3M!=0 ] <- "Q1"
  cbm_base$segm[cbm_base$CONSO_3M <= aa[3,] & cbm_base$CONSO_3M >aa[2,] ] <- "Q2"
  cbm_base$segm[cbm_base$CONSO_3M <= aa[4,] & cbm_base$CONSO_3M >aa[3,] ] <- "Q3"
  cbm_base$segm[cbm_base$CONSO_3M <= aa[5,] & cbm_base$CONSO_3M >aa[4,] ] <- "Q4"
  cbm_base$segm[cbm_base$CONSO_3M > max(a$ARPU[a$seg=="HV"])] <- "SHV"
  detach(cbm_base)
  
  #Dropping outliers
  
  lh <- quantile(cbm_base[cbm_base$segm =="SHV",]$CONSO_3M,probs=0.25)	#Lower hinge (first quartile)
  uh <- quantile(cbm_base[cbm_base$segm =="SHV",]$CONSO_3M,probs=0.75)	#Upper hinge (third quartile)
  step<- 1.5 * (uh-lh)	#Define the step as 1.5×IQR
  
  cbm_base<-cbm_base[cbm_base$CONSO_3M<=(uh+step),]
  
  #cbm_base<-cbm_base[!cbm_base$CONSO_3M%in%boxplot.stats(cbm_base$CONSO_3M[cbm_base$segm=="SHV"])$out,]
  
  
  seuil<-sqldf("SELECT segm,count(segm) as volume, max(CONSO_3M) as thresholds
               FROM cbm_base
               group by segm")
  

  seuil1<-subset(cbm_base,!(cbm_base$SUBS_ID%in%base_gtu$SUBS_ID))
  
  seuil2<-sqldf("SELECT segm,count(segm) as volume
               FROM seuil1 group by segm")
  

  
  seuil$percentage=100*seuil$volume/sum(seuil$volume)
  
  seuil2$percentage=100*seuil2$volume/sum(seuil2$volume)
  
  
  cat(sprintf("\"%s\"\"%f\" \"%s\"\n",'There is ', doublons_cbm,
              'duplicated rows deleted from the cbm base'))
  
  cat(sprintf("\"%s\"\"%f\" \"%s\"\n",'There is ', doublons_gtu,
              'duplicated rows deleted from the ucg base')) 
  
  print("Segmentation distribution in the CBM base including the former UCG")
  print(seuil)

  print("----------------------------------------------------------------")
  cat(sprintf("\"%s\"\"%f\" \"%s\"\n",'Q1 are customers who make less than', seuil[1,3],
              'revenues during the period'))
  
  cat(sprintf("\"%s\"\"%f\" \"%s\"\n",'They represent', seuil[1,4],
              'percent of the customer base'))
  
  cat(sprintf("\"%s\"\"%f\" \"%s\"\n",'Zeros are customers who make', seuil[6,3],
              'revenues during the period'))
  
  cat(sprintf("\"%s\"\"%f\" \"%s\"\n",'They represent', seuil[6,4],
              'percent of the customer base'))
  
  print("----------------------------------------------------------------")
  
  print("Segmentation distribution in the CBM base excluding the former UCG")
  print(seuil2)
  
  
  return(cbm_base)
  
}


#****************************************************************
#This macro allows to sample the representative UCG             *
# The sampling is based on strata of value segmentation         *
# computed using the macro segval the segment different         *
#from Zeros and SHV are split into 4 parts based on quartiles   *
# The first argument is your all cbm base, the 2nd argument is  *
#                       your UCG size                           *
#****************************************************************

random.sample <- function(cbm_base,base_gtu,taille) {
  # Exclusion of the previous UCG
  
  cbm_base<-cbm_base[!duplicated(cbm_base[,1]),]
  base_gtu<-base_gtu[!duplicated(base_gtu[,1]),]
  base_gtu<-as.data.frame(base_gtu)
  names(base_gtu)<-'SUBS_ID'
  GTU_strata<-NULL
  base_animable<-NULL
  
  base_cbm_hors_gtu<-subset(cbm_base,!(cbm_base$SUBS_ID%in%base_gtu$SUBS_ID))
  
  

  

  
 #base_cbm_hors_gtu<-sqldf("SELECT DISTINCT *
 #                   FROM cbm_base
  #                    WHERE SUBS_ID NOT IN(
  #                    SELECT DISTINCT SUBS_ID
   #                    FROM base_gtu)")
  
  
  
  #base_cbm_hors_gtu<-base_cbm_hors_gtu[!base_cbm_hors_gtu$CONSO_3M%in%boxplot.stats(base_cbm_hors_gtu$CONSO_3M[base_cbm_hors_gtu$segm=="SHV"])$out,]
  
  #Splitting the other segments base into quartiles
  base_cbm_hors_gtu2 <- within(base_cbm_hors_gtu[base_cbm_hors_gtu$segm !="ZEROS" & 
                                                   base_cbm_hors_gtu$segm !="SHV",], 
                               quartile <- as.integer(cut(CONSO_3M, 
                                                          quantile(CONSO_3M, probs=0:4/4), 
                                                          include.lowest=TRUE)))
  
  
  base_cbm_hors_gtu2$segm<-ifelse(base_cbm_hors_gtu2$quartile==1,"Q1",
                                  ifelse(base_cbm_hors_gtu2$quartile==2,"Q2",
                                         ifelse(base_cbm_hors_gtu2$quartile==3,"Q3",
                                                "Q4"))) 
  
  base_cbm_hors_gtu2$quartile<-NULL
  
  #Sélection de la base des SHV
  base_cbm_hors_gtu3<-base_cbm_hors_gtu[base_cbm_hors_gtu$segm =="SHV",]
  #Sélection de la base des ZEROS
  base_cbm_hors_gtu4<-base_cbm_hors_gtu[base_cbm_hors_gtu$segm =="ZEROS",]
  
  
  i=0
  repeat {
    
    
    
    GTU_DEC <- stratified(base_cbm_hors_gtu2, "segm",round(taille*nrow(base_cbm_hors_gtu2)/(4*nrow(base_cbm_hors_gtu))))
    ecart_dec<-100*((mean(GTU_DEC$CONSO_3M)-mean(base_cbm_hors_gtu2$CONSO_3M))/mean(base_cbm_hors_gtu2$CONSO_3M))
    ecart_type_dec<-100*abs((sd(GTU_DEC$CONSO_3M)-sd(base_cbm_hors_gtu2$CONSO_3M))/sd(base_cbm_hors_gtu2$CONSO_3M))
    
    #Slecting the SHV strata
    GTU_SHV<-stratified(base_cbm_hors_gtu3,"segm", 
                        round(taille*nrow(base_cbm_hors_gtu3)/nrow(base_cbm_hors_gtu)))
    
    #Compute of the ARPU gap in SHV strata
    ecart_shv<-100*((mean(GTU_SHV$CONSO_3M)-mean(base_cbm_hors_gtu3$CONSO_3M))/mean(base_cbm_hors_gtu3$CONSO_3M))
    
    ecart_type_shv<-100*abs((sd(GTU_SHV$CONSO_3M)-sd(base_cbm_hors_gtu3$CONSO_3M))/sd(base_cbm_hors_gtu3$CONSO_3M))
    
    #Selecting the sample of the ZERO strata 
    GTU_ZERO<-stratified(base_cbm_hors_gtu4,"segm", 
                         round(taille*nrow(base_cbm_hors_gtu4)/nrow(base_cbm_hors_gtu)))
    
    

    GTU_strata<-do.call("rbind", list(GTU_ZERO,GTU_DEC,GTU_SHV))
    #base_cbm_hors_gtu<-do.call("rbind", list(base_cbm_hors_gtu4,base_cbm_hors_gtu3,base_cbm_hors_gtu2))
    
   # base_gtu<-base_gtu[!duplicated(base_gtu[,1]),]
    
    #base_cbm_hors_gtu <-subset(base_cbm_hors_gtu,!(base_cbm_hors_gtu$SUBS_ID%in%GTU_strata$SUBS_ID))
    
    ecart<-100*((mean(GTU_strata$CONSO_3M)-mean(base_cbm_hors_gtu$CONSO_3M))/mean(base_cbm_hors_gtu$CONSO_3M))
    ecart_type<-100*abs((sd(GTU_strata$CONSO_3M)-sd(base_cbm_hors_gtu$CONSO_3M))/sd(base_cbm_hors_gtu$CONSO_3M))
    
    i=i+1
    
    if ( abs(ecart_dec)< 0.1 & abs(ecart_type_dec)<5 &
         abs(ecart_shv)< 0.1 & abs(ecart_type_shv)<5 & 
         abs(ecart)< 0.1 & abs(ecart_type)<5
      
    ) break
  }
  
  
  base_gtu<-base_gtu[!duplicated(base_gtu[,1]),]
  
  base_cbm_hors_gtu <-subset(base_cbm_hors_gtu,!(base_cbm_hors_gtu$SUBS_ID%in%GTU_strata$SUBS_ID))
  
  base_animable <-subset(cbm_base,!(cbm_base$SUBS_ID%in%GTU_strata$SUBS_ID))
  
  
  
  ecart<-100*((mean(GTU_strata$CONSO_3M)-mean(base_cbm_hors_gtu$CONSO_3M))/mean(base_cbm_hors_gtu$CONSO_3M))
  ecart_type<-100*abs((sd(GTU_strata$CONSO_3M)-sd(base_cbm_hors_gtu$CONSO_3M))/sd(base_cbm_hors_gtu$CONSO_3M))
  
  ecart2<-100*((mean(GTU_strata$CONSO_3M)-mean(base_animable$CONSO_3M))/mean(base_animable$CONSO_3M))
  ecart_type2<-100*abs((sd(GTU_strata$CONSO_3M)-sd(base_animable$CONSO_3M))/sd(base_animable$CONSO_3M))
  
  
  GTU_NEW<-GTU_strata
  
  
  
  #CBM_base segmentation and agregated ARPU by month for template filling
  cbm_seg<-sqldf("SELECT segm,count(segm) as volume, sum(ARPU_M1) as sum_ARPU_M1,
                 sum(ARPU_M2) as sum_ARPU_M2,sum(ARPU_M3) as sum_ARPU_M3
                 FROM base_animable
                 group by segm")
  cbm_seg$percentage=100*cbm_seg$volume/sum(cbm_seg$volume)
  
  cbm_seg_rev<-sqldf("SELECT sum(ARPU_M1) as sum_ARPU_M1,
                     sum(ARPU_M2) as sum_ARPU_M2, sum(ARPU_M3) as sum_ARPU_M3
                     FROM base_animable")
  
  
  
  #UCG_base segmentation and agregated ARPU by month for template filling
  gtu_seg<-sqldf("SELECT segm,count(segm) as volume, sum(ARPU_M1) as sum_ARPU_M1,
                 sum(ARPU_M2) as sum_ARPU_M2,sum(ARPU_M3) as sum_ARPU_M3
                 FROM GTU_strata
                 group by segm")
  gtu_seg$percentage=100*gtu_seg$volume/sum(gtu_seg$volume)
  
  gtu_seg_rev<-sqldf("SELECT sum(ARPU_M1) as sum_ARPU_M1,
                     sum(ARPU_M2) as sum_ARPU_M2,sum(ARPU_M3) as sum_ARPU_M3
                     FROM GTU_strata")
  
  
  
  print("The CBM targetable base contains including the former UCG:")
  cat(sprintf("\"%i\" \"%s\"\n", nrow(base_animable),'observations'))
  
  print("The CBM targetable base contains excluding the former UCG:")
  cat(sprintf("\"%i\" \"%s\"\n", nrow(base_cbm_hors_gtu),'observations'))
  
  print("The UCG base contains :")
  cat(sprintf("\"%i\" \"%s\"\n", nrow(GTU_strata),'observations'))
  
  
  print("----------------------------------------------------------------")
  
  print("The quaterly ARPU of the CBM base including the former UCG is :")
  print(mean(base_animable$CONSO_3M))
  
  print("The quaterly ARPU of the CBM base excluding the former UCG is :")
  print(mean(base_cbm_hors_gtu$CONSO_3M))
  
  print("The quaterly ARPU of the UCG base is :")
  print(mean(GTU_NEW$CONSO_3M))
  
  print("----------------------------------------------------------------")
  
  print("The quartely ARPU gap with the overall cbm base including former UCG is :")
  cat(sprintf("\"%f\" \"%s\"\n", abs(ecart2),'%'))
  
  print("The quartely ARPU gap with the overall cbm base excluding former UCG is :")
  cat(sprintf("\"%f\" \"%s\"\n", abs(ecart),'%'))
  
  print("----------------------------------------------------------------")
  print("The quartely standard deviation ARPU of the CBM base including former UCG is :")
  print(sd(base_animable$CONSO_3M))
  
  print("The quartely standard deviation ARPU of the CBM base excluding former UCG is :")
  print(sd(base_cbm_hors_gtu$CONSO_3M))
  
  print("The quartely standard deviation ARPU of the UCG base is :")
  print(sd(GTU_NEW$CONSO_3M))
  
  print("----------------------------------------------------------------")
  
  print("The quaterly ARPU standard deviation gap with the CBM base including former UCG is : ")
  cat(sprintf("\"%f\" \"%s\"\n", abs(ecart_type2),'%'))
  
  print("The quaterly ARPU standard deviation gap with the CBM base excluding former UCG is : ")
  cat(sprintf("\"%f\" \"%s\"\n", abs(ecart_type),'%'))
  print("----------------------------------------------------------------")
  print("The number of iterations is :")
  print(i)
  
  
  
  print("----------------------------------------------------------------")
  print("        Output to paste in the on the Excel template            ")
  print("----------------------------------------------------------------")
  
  # Printing results
  print("Value base segmentation distribution in the CBM base")
  print(cbm_seg[,c(1,2,6)])
  print("Value base segmentation distribution in the UCG base")
  print(gtu_seg[,c(1,2,6)])
  
  
  print("Outgoing revenu by month in the CBM base")
  print(cbm_seg_rev)
  print("Outgoing revenu by month in the UCG base")
  print(gtu_seg_rev)
  
  print("---------------------------END------------------------------")
  
  assign("GTU_strata",GTU_NEW,envir = .GlobalEnv)
  assign("ecart",ecart,envir = .GlobalEnv)
  assign("base_animable",base_animable,envir = .GlobalEnv)

  # Saving datasets into csv in your directory
  write.csv(base_animable,"base_animable.csv",row.names = FALSE)
  write.csv(GTU_NEW,"GTU_strata.csv",row.names = FALSE)

}