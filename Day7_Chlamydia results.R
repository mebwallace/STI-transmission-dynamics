#Organizing chlamydia data by individual ID 

#Loading in chlamydia data 
chlam_df<- read_excel("Desktop/Summer Research/Phase2_Chlamydia_output.xlsx", 
                      sheet="Removed")
print(chlam_df)


n_distinct(chlam_df$Study_Participant_ID)
#smoothing out the result value nulls
#replacing all the nulls values 
chlam_df$Result_Value[chlam_df$Result_Value=="NT"]<-"Null"
chlam_df$Result_Value[chlam_df$Result_Value=="TNP"]<-"Null"
chlam_df$Result_Value[chlam_df$Result_Value=="SEE BELOW"]<-"Null"
chlam_df$Result_Value[chlam_df$Result_Value=="TNP1601"]<-"Null"
chlam_df$Result_Value[chlam_df$Result_Value=="TNP124"]<-"Null"
chlam_df$Result_Value[chlam_df$Result_Value=="TNP317"]<-"Null"
chlam_df$Result_Value[chlam_df$Result_Value=="TNP-NO SUITABLE SPECIMEN RECEIVED."]<-"Null"
chlam_df$Result_Value[chlam_df$Result_Value=="NOT TESTED"]<-"Null"

summary(as.factor(chlam_df$Result_Name))

#Recoding result names to group by location

recode_list <- list(
  "urogenital" = c("CHLAMYDIA AND GC SCREEN",
                   "CHLAMYDIA TRACHOMATIS RNA, TMA",
                   "CHLAMYDIA TRACHOMATIS RNA,TMA",
                   "CHLAMYDIA TRACHOMATIS/NEISSERIA GONORRHOEAE TMA", 
                   "CHLAMYDIA TRACHOMATIS/N. GONORRHOEAE RNA, TMA",
                   "CHLAMYDIA/N GONORRHOEAE,DNA,SDA, PAP VIAL",
                   "CHLAMYDIA TRACHOMATIS DNA, SDA",
                   "CHLAMYDIA TRACHOMATIS, DFA",
                   "CHLAMYDIA/N. GONORRHOEAE DNA, SDA"),
  "rectal" = c("CHLAMYDIA-GC RNA, TMA, RECTAL"),
  "throat" = c("CHLAMYDIA-GC, RNA, TMA, THROAT")
)


reassign_values_by_list <- function(vector, list, new_label) {
  vector[which(vector %in% list[[new_label]])] <- new_label
  return(vector)
}

updated_chlam_df <- chlam_df %>%
  filter(Result_Value == "DETECTED") %>%
  mutate(Result_Name = reassign_values_by_list(vector = Result_Name, 
                                               list = recode_list, 
                                               new_label = "urogenital"),
         Result_Name = reassign_values_by_list(vector = Result_Name, 
                                               list = recode_list, 
                                               new_label = "throat"),
         Result_Name = reassign_values_by_list(vector = Result_Name, 
                                               list = recode_list, 
                                               new_label = "rectal"))


table(updated_chlam_df$Result_Name)
ggplot(data = updated_chlam_df, aes(Result_Value, fill = Result_Name)) +
  geom_bar(position = "dodge2")

positiveschlam <- function(dataframe) {
  participantschlam <- unique(dataframe[["Study_Participant_ID"]])
  
  outcomes2 <- sapply(participantschlam, function(id) {
    reduced2 <- dataframe %>%
      filter(Study_Participant_ID == id) 
    
    if ("DETECTED" %in% reduced2[["Result_Value"]]) {
      positive2 <- reduced2 %>%
        filter(Result_Value == "DETECTED")
      
      if (("urogenital" %in% positive2[["Result_Name"]] & 
           "throat" %in% positive2[["Result_Name"]] &
           "rectal" %in% positive2[["Result_Name"]])) {
        "The Big Three"
      } else if (("urogenital" %in% positive2[["Result_Name"]] & 
                  "throat" %in% positive2[["Result_Name"]])) {
        "Urogenital and Throat"
      } else if (("urogenital" %in% positive2[["Result_Name"]] &
                  "rectal" %in% positive2[["Result_Name"]])) {
        "Urogenital and Rectal"
      } else if (("throat" %in% positive2[["Result_Name"]] &
                  "rectal" %in% positive2[["Result_Name"]])) {
        "Throat and Rectal"
      } else if ("urogenital" %in% positive2[["Result_Name"]]) {
        "Urogential Only"
      } else if ("throat" %in% positive2[["Result_Name"]]) {
        "Throat Only"
      } else if ("rectal" %in% positive2[["Result_Name"]]) {
        "Rectal Only"
      } else {
        "ASM Fucked Up"
      }
    } else {
      "No positives"
    }
  })
  
  list("participant" = participantschlam,
       "outcomes" = outcomes2) %>%
    as.data.frame() %>%
    as_tibble()
}

positiveschlam(dataframe = megi_late_night2) %>%
  filter(outcomes != "No positives") %>%
  ggplot(data = , aes(outcomes)) +
  geom_bar(position = "dodge2", fill="dark blue")

chlam<- as.data.frame(positiveschlam(dataframe = megi_late_night2))
summary(as.factor(chlam$outcomes))
table(as.factor(chlam$outcomes))

# Break
megi_late_night2 <- chlam_df %>%
  select(Study_Participant_ID, Result_Name, Result_Value) %>%
  mutate(Result_Name = reassign_values_by_list(vector = Result_Name, 
                                               list = recode_list, 
                                               new_label = "urogenital"),
         Result_Name = reassign_values_by_list(vector = Result_Name, 
                                               list = recode_list, 
                                               new_label = "throat"),
         Result_Name = reassign_values_by_list(vector = Result_Name, 
                                               list = recode_list, 
                                               new_label = "rectal")) %>%
  filter(Result_Name %in% c("urogenital", "throat", "rectal")) %>%
  group_by(Result_Name)


#coutning how many individuals had an infection at which location

recode_list <- list(
  "urogenital" = c("CHLAMYDIA AND GC SCREEN",
                   "CHLAMYDIA TRACHOMATIS RNA, TMA",
                   "CHLAMYDIA TRACHOMATIS RNA,TMA",
                   "CHLAMYDIA TRACHOMATIS/NEISSERIA GONORRHOEAE TMA", 
                   "CHLAMYDIA TRACHOMATIS/N. GONORRHOEAE RNA, TMA",
                   "CHLAMYDIA/N GONORRHOEAE,DNA,SDA, PAP VIAL",
                   "CHLAMYDIA TRACHOMATIS DNA, SDA",
                   "CHLAMYDIA TRACHOMATIS, DFA",
                   "CHLAMYDIA/N. GONORRHOEAE DNA, SDA"),
  "rectal" = c("CHLAMYDIA-GC RNA, TMA, RECTAL"),
  "throat" = c("CHLAMYDIA-GC, RNA, TMA, THROAT")
)


reassign_values_by_list <- function(vector, list, new_label) {
  vector[which(vector %in% list[[new_label]])] <- new_label
  return(vector)
}


# Break
megi_late_night3 <- chlam_df %>%
  select(Study_Participant_ID, Result_Name, Result_Value) %>%
  mutate(Result_Name = reassign_values_by_list(vector = Result_Name, 
                                               list = recode_list, 
                                               new_label = "urogenital"),
         Result_Name = reassign_values_by_list(vector = Result_Name, 
                                               list = recode_list, 
                                               new_label = "throat"),
         Result_Name = reassign_values_by_list(vector = Result_Name, 
                                               list = recode_list, 
                                               new_label = "rectal")) %>%
  filter(Result_Name %in% c("urogenital", "throat", "rectal")) %>%
  #group_by(Result_Name)
  group_by(chlam_df$Study_Participant_ID) %>%
  summarise(total_tests = n(),
            detection_rate = 100 * length(which(Result_Value == "DETECTED")) / n()) %>%
  arrange(desc(detection_rate))

print(megi_late_night3)

write_xlsx(megi_late_night3, "Desktop/Summer Research/Phase2_chlam_detection_rate.xlsx")

#Indirect way 
chlam_df2<- megi_late_night3%>%
  filter(Result_Value == "NOT DETECTED") 

chlam_df3<- chlam_df2 %>%
  filter(Result_Name == "throat")
  mutate("count_throat" = unique(chlam_df3$Study_Participant_ID))
  

#plots by location of infection
ggplot(new_df_try, aes(x=LocationofInfection))+ geom_bar()


positiveschlam2 <- function(dataframe) {
  participantschlam2 <- unique(dataframe[["Study_Participant_ID"]])
  
  outcomes3 <- sapply(participantschlam2, function(id) {
    reduced3 <- dataframe %>%
      filter(Study_Participant_ID == id) 
    
    if ("DETECTED" %in% reduced3[["Result_Value"]]) {
      positive3 <- reduced3 %>%
        filter(Result_Value == "DETECTED")
      
      if (("urogenital" %in% positive3[["Result_Name"]])) {
        "Urogenital"
      } else if (("throat" %in% positive3[["Result_Name"]])) {
        "Throat"
      } else if (("rectal" %in% positive3[["Result_Name"]])) {
        "Rectal"
      } 
       else {
        "ERROR"
      }
    } else {
      "No positives"
    }
  })
  
  list("participant" = participantschlam2,
       "outcomes" = outcomes3) %>%
    as.data.frame() %>%
    as_tibble()
}

positiveschlam2(dataframe = megi_late_night3) %>%
  filter(outcomes != "No positives") %>%
  ggplot(data = , aes(outcomes)) +
  geom_bar(position = "dodge2", fill="dark blue")
