
# Set your working directory: copy and paste here the directory of your datasets between the ""
# If you are working on windows replace "\" by "/" below
setwd("C:/Users/Data 1/Desktop/0-Mission IMT CBM")

#Install and activate packages
setInternet2(TRUE)
install.packages("splitstackshape")
install.packages("sqldf")
library(splitstackshape)
library(sqldf)


#Data importation

#****************************************************
#  To run this program you need 2 datasets:         *
#  Your cbm base called here cbm_base and your      *
#  previous UCG datasets called here base_gtu       *
#****************************************************

#Ensure that your seperator is ";".
# Wether replace sep=";" by:
# "\t" for tab
# "," for comma
cbm_base<- read.table("base_cbm.csv", sep=",", header=T)
base_gtu<- read.table("base_gtu.csv", sep=";", header=T)
names(base_gtu)[1]<-"SUBS_ID"


  
#***************************************************
# In the base_cbm you must have these variables    *
#      -SUBS_ID which is the concatenation of the  *
#       MSISDN and the activation date             *     
#      -ARPU_M1 is the APRU of month M-1           *
#      -ARPU_M2 is the APRU of month M-2           *
#      -ARPU_M3 is the APRU of month M-3           *
# In the base_gtu you only need the SUBS_ID        *
#***************************************************
  
  
  
  #******************************************************************
  #  Execution steps:                                               *
  #    - First run the functions in the file 1_UCG_Functions.R      *
  #    - Run the function segval below by putting in argument       *
  #             your cbm base and the UCG base                      *
  #        The function give you a value based segmentation         *
  #       with a ZEROS segment, a SHV segment and 4 others segments *
  #        called Q1, Q2, Q3, Q4                                    *
  #    - Run the function random.sample by puting in arguments      *
  #     your cbm basesegmented by the previous step, the former UCG *
  #         base and  the size of the new UCG you want              *
  #******************************************************************



#Segmentation
cbm_base<-segval(cbm_base,base_gtu)

#Sampling process
random.sample(cbm_base,base_gtu,144500)

