
library(tidyr)
library(dplyr)
library(ggplot2)
library(keyringr)
library(redcapAPI)
library(REDCapR)
library(lubridate)

# **************************************************************************** #
# ***************  Pull data from redcap with api                                              
# **************************************************************************** # 

# Get Redcap API Token
# # https://cran.r-project.org/web/packages/keyringr/vignettes/Avoiding_plain_text_passwords_in_R_with_keyringr.html
credential_label <- "beach_api"
credential_path <- paste(Sys.getenv("USERPROFILE"), '\\DPAPI\\passwords\\', Sys.info()["nodename"], '\\', credential_label, '.txt', sep="")
uri <- "https://redcap.ctsi.ufl.edu/redcap/api/"
beach_token<-decrypt_dpapi_pw(credential_path)
print(beach_token)

# Create connections
rcon <- redcapConnection(url=uri, token=beach_token)

# crc variables
fields <- exportFieldNames(rcon)
desired_fields_triHU=c("mom3t_delivery_location",
                       "mom3t_breast_surg",
                       "mom3t_baby_sex")  
desired_fields_2wkHU=c("mom2wk_delivery_date",
                       "inf2wk_delivery_location",
                       "inf2wk_sex")
desired_fields_2moHU=c("mom2mo_abx_name"
                       ,"mom2mo_abx_length"
                       ,"mom2mo_height_foot"
                       ,"mom2mo_height_in"
                       ,"mom2mo_wt_lbs"
                       ,"mom2mo_wtchange_postbirth"
                       ,"mom2mo_wtgain_postbirth"
                       ,"mom2mo_wtloss_postbirth"
                       ,"mom2mo_drinks_wk"
                       ,"mom2mo_drinks_4more"
                       ,"mom2mo_cigs"
                       ,"mom2mo_cigs_day"
                       ,"mom2mo_2nd_smoke"
                       ,"mom2mo_2nd_smoke_hrs"
                       ,"mom2mo_birth_ctrl"
                       ,"mom2mo_birth_ctrl_type"
                     )
desired_fields_all=c(desired_fields_triHU,desired_fields_2wkHU,"test_id","redcap_event_name")
# events to retain
exportEvents(rcon)
events_to_retain  <- c("third_trimester_arm_1", "two_week_arm_1", "two_month_arm_1")

# list of instruments
exportInstruments(rcon)
Instruments_to_retain <- c("health_update_3rd_trimester_v5082518","clinical_health_update_2wk_v4_042218",
                           "clinical_health_update_2mo_to_12mo_v3_022118","infant_feeding_questionnaire_2_weeks_v3_042218",
                           "infant_feeding_questionnaire_2mo_to_12mo_v3_022118")

# list of events
exportEvents(rcon)

# list records
exportRecords(rcon)

# export field names
exportFieldNames(rcon)

# consented records
consent.records.v1=c("BLS001A","BLS002A","BLS003A")

# pull data
ds_some_rows_v1 <- redcap_read(
  batch_size=300,
  records= consent.records.v1,
  redcap_uri = uri, 
  token      = beach_token, 
  fields     = desired_fields_all,
  events     = events_to_retain,
  
)$data

# look at data
dat=ds_some_rows_v1
head(dat); str(dat); names(dat)

#chart of Delivery location question at 2 week
DeliveryLocation_wk <-dat%>%
  select(test_id,redcap_event_name,inf2wk_delivery_location___1:inf2wk_delivery_location___5)%>%
  arrange(test_id,redcap_event_name)%>%
  filter(redcap_event_name == "two_week_arm_1")

#chart of Delivery location question at 3rd trimester   
DeliveryLocation_tri <-dat%>%
  select(test_id,redcap_event_name,mom3t_delivery_location)%>%
  arrange(test_id,redcap_event_name)%>%
  filter(redcap_event_name == "third_trimester_arm_1")

infantSex <- dat%>%
  select(test_id,redcap_event_name,mom3t_baby_sex,inf2wk_sex)%>%
  arrange(test_id,redcap_event_name)


#chart of Delivery location question at 3rd trimester   
DeliveryLocation_tri <-dat%>%
  select(test_id,redcap_event_name,mom3t_delivery_location)%>%
  arrange(test_id,redcap_event_name)%>%
  filter(redcap_event_name == "third_trimester_arm_1")
kable(DeliveryLocation_tri)
#Chart of Infant sex responses


#chart of Delivery location question at 2 week

DeliveryLocation_wk <-dat%>%
  select(test_id,redcap_event_name,inf2wk_delivery_location___1:inf2wk_delivery_location___5)%>%
  arrange(test_id,redcap_event_name)%>%
  rename("UFHealth"=inf2wk_delivery_location___1,
         "North Florida Regional"=inf2wk_delivery_location___2,
         "Birth & wellness Center of Gainesville"=inf2wk_delivery_location___3,
         "other"=inf2wk_delivery_location___4,
         "IDK"=inf2wk_delivery_location___5)%>%
  filter(redcap_event_name == "two_week_arm_1")
head(DeliveryLocation_wk)

uf<-DeliveryLocation_wk%>%
  select(test_id,UFHealth)%>%
  filter(UFHealth>0)%>%
  select(test_id)
head(uf)

NF<-DeliveryLocation_wk%>%
  select(test_id,`North Florida Regional`)%>%
  filter(`North Florida Regional`>0)%>%
  select(test_id)
head(NF)

BWC<-DeliveryLocation_wk%>%
  select(test_id,`Birth & wellness Center of Gainesville`)%>%
  filter(`Birth & wellness Center of Gainesville`>0)%>%
  select(test_id)
head(BWC)

Other<-DeliveryLocation_wk%>%
  select(test_id,`other`)%>%
  filter(other>0)%>%
  select(test_id)
head(Other)

idk<-DeliveryLocation_wk%>%
  select(test_id,`IDK`)%>%
  filter(IDK>0)%>%
  select(test_id)
head(idk)
tm1<-dplyr::full_join(uf,NF,BWC,by="test_id")
tm2<-dplyr::full_join(idk,Other,by="test_id")
dplyr::full_join(tm1,tm2,by="test_id")



