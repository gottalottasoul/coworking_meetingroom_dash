library(tidyverse)
library(RiHana)

load("~/OneDrive - CBRE, Inc/data/raw_data/ELT_raw_data.RData")


meet_bookings_edited<-nexudus_meetings %>% 
  mutate(pos=as.double(pos)) %>% 
  mutate_if(is.character, list(~na_if(.,""))) %>% 
  mutate_at(vars(discount,gross_price), list(~ifelse(is.na(bookingid), 0, .))) %>% 
  filter(is_deleted=='NO',
         grepl('PD Prod|PPI',resourceresourcetypename),
         coworkerid!='1122454558',
         !grepl('Catering|Day Pass',resourceresourcetypename,ignore.case = TRUE)) %>% 
  separate(email,c(NA,"email_domain"),sep="@") %>% 
  arrange(createdon) %>% 
  group_by(coworkerid) %>% 
  mutate(name=resourcename,
         time_between_bookings=coalesce(difftime(createdon,lag(createdon,1),units = 'mins'),99),
         resource_type=str_replace_all(resourceresourcetypename, ' \\(PD Prod\\)| \\(PPI\\)', ''),
         room_size=str_replace_all(room_size, ' \\(PD Prod\\)| \\(PPI\\)', ''),
         unique_booking=ifelse(time_between_bookings>5,1,0),
         booking_number=row_number(),
 #        invoiced=as.numeric(invoiced),
         internal_booking=ifelse(email_domain=='yourhana.com',1,0), 
         employee_related=ifelse(grepl('cbre|hana',email_domain,ignore.case = TRUE),1,0),
         estimated_price=coworkerextraserviceprice,
         zero_price_res=ifelse(gross_price==0,1,0),
         net_price=gross_price+discount,
#         fully_discounted=ifelse(zero_price_res==0,ifelse(gross_price+discount==0,1,0),0),
          discount_level= case_when(
            zero_price_res==1~'Full',
            gross_price+discount==0~'Full',
            discount==0~'None',
            TRUE~'Partial'
          ),
         hana_location=ifelse(grepl('PD Prod',resourceresourcetypename),'Park District','Park Place'),
         company=coalesce(companyname,teamsatthetimeofbooking,email_domain),
         fromtime_local=as.POSIXct(ifelse(hana_location=='Park District',fromtime-lubridate::hours(6),fromtime-lubridate::hours(8)),origin="1970-01-01",tz='UTC'),
         totime_local=as.POSIXct(ifelse(hana_location=='Park District',totime-lubridate::hours(6),totime-lubridate::hours(8)),origin="1970-01-01",tz='UTC'),
         meeting_date=as.Date(totime_local),
         meet_start=ifelse(lubridate::minute(fromtime_local)>0,lubridate::hour(fromtime_local)+.5,lubridate::hour(fromtime_local)),
         meet_end=ifelse(lubridate::minute(totime_local)>0,lubridate::hour(totime_local)+.5,lubridate::hour(totime_local)),
         meet_day=lubridate::wday(fromtime_local,label=TRUE,week_start=1),
         meeting_length=difftime(totime,fromtime,units="mins"),
         booking_lead_time=difftime(totime,createdon,units="hours")) %>% 
  select(-companyname,-pos,-teamsatthetimeofbooking,-is_deleted,-tariffatthetimeofbooking,-resourceresourcetypename,-resourcename) %>% 
  ungroup(.)


zoom_meetings_edited<-zoom_meetings %>% 
  inner_join(RiHana::get_hana_meeting_rooms(),by=c("host"="zoom_room_id"))

#skim(meet_bookings)

save(meet_bookings_edited,file="inc/Meet_data.RData")
