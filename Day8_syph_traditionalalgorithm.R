#Syphilis stuff 
#Need to split Phase2 into syphilis  
Phase3_df<- read_excel("Desktop/Summer Research/Phase2_playingaround_630.xlsx", sheet = "syph")
head(Phase3_df)

unique_identifierss<- subset(Phase3_df, select = c("Study_Participant_ID", "Study_Participant_Gender"))
dfs<- unique(unique_identifierss)
n_distinct(unique_identifierss)

colSums(dfs== "M")
colSums(dfs=="F")

Phase3_df$Result_Value[Phase3_df$Result_Value=="NONREACTIV"]<-"NONREACTIVE"
Phase3_df$Result_Value[Phase3_df$Result_Value=="REACTIVEMINIMAL"]<-"EQUIVOCAL"
Phase3_df$Result_Value[Phase3_df$Result_Value=="TNP114"]<-"TNP"
Phase3_df$Result_Value[Phase3_df$Result_Value=="TNP124"]<-"TNP"
Phase3_df$Result_Value[Phase3_df$Result_Value=="TNP317"]<-"TNP"
Phase3_df$Result_Value[Phase3_df$Result_Value=="TNP603"]<-"TNP"
Phase3_df$Result_Value[Phase3_df$Result_Value=="NT"]<-"TNP"
Phase3_df$Result_Value[Phase3_df$Result_Value=="NOT INDICATED"]<-"TNP"
Phase3_df$Result_Value[Phase3_df$Result_Value=="TNP-NO SUITABLE SPECIMEN RECEIVED."]<-"TNP"
Phase3_df$Result_Value[Phase3_df$Result_Value=="TNP-REFLEX TESTING NOT REQUIRED"]<-"TNP"
Phase3_df$Result_Value[Phase3_df$Result_Value=="NA's"]<-"NULL"
Phase3_df$Result_Value[Phase3_df$Result_Value=="MINIMAL"]<-"EQUIVOCAL"
Phase3_df$Result_Value[Phase3_df$Result_Value=="NOT DETECTED"]<-"NONREACTIVE"
Phase3_df$Result_Value[Phase3_df$Result_Value=="NONREACTIVE"]<-"NONREACTIVE"
Phase3_df$Result_Value[Phase3_df$Result_Value=="RPR NONREACTIV"]<-"NONREACTIVE"
Phase3_df$Result_Value[Phase3_df$Result_Value=="NEGATIVE"]<-"NONREACTIVE"
Phase3_df$Result_Value[Phase3_df$Result_Value=="REACTIVE"]<-"REACTIVE"
Phase3_df$Result_Value[Phase3_df$Result_Value=="POSITIVE"]<-"REACTIVE"
Phase3_df$Result_Value[Phase3_df$Result_Value=="RPR TITER * REACTIVE 1:2"]<-"REACTIVE"
Phase3_df$Result_Value[Phase3_df$Result_Value=="REACTIVE 1:2"]<-"REACTIVE"
Phase3_df$Result_Value[Phase3_df$Result_Value=="REACTIVE 1:1"]<-"REACTIVE"
Phase3_df$Result_Value[Phase3_df$Result_Value=="REACTIVE 1:4"]<-"REACTIVE"
Phase3_df$Result_Value[Phase3_df$Result_Value=="REACTIVE 1:1,024"]<-"REACTIVE"
Phase3_df$Result_Value[Phase3_df$Result_Value=="REACTIVE 1:1,048,576"]<-"REACTIVE"
Phase3_df$Result_Value[Phase3_df$Result_Value=="REACTIVE 1:1024"]<-"REACTIVE"
Phase3_df$Result_Value[Phase3_df$Result_Value=="REACTIVE 1:256"]<-"REACTIVE"
Phase3_df$Result_Value[Phase3_df$Result_Value=="REACTIVE 1:1"]<-"REACTIVE"
Phase3_df$Result_Value[Phase3_df$Result_Value=="REACTIVE 1:8"]<-"REACTIVE"
Phase3_df$Result_Value[Phase3_df$Result_Value=="REACTIVE 1:128"]<-"REACTIVE"
Phase3_df$Result_Value[Phase3_df$Result_Value=="REACTIVE 1:16"]<-"REACTIVE"
Phase3_df$Result_Value[Phase3_df$Result_Value=="REACTIVE 1:32"]<-"REACTIVE"
Phase3_df$Result_Value[Phase3_df$Result_Value=="REACTIVE 1:512"]<-"REACTIVE"
Phase3_df$Result_Value[Phase3_df$Result_Value=="REACTIVE 1:4096"]<-"REACTIVE"
Phase3_df$Result_Value[Phase3_df$Result_Value=="REACTIVE 1:4,096"]<-"REACTIVE"
Phase3_df$Result_Value[Phase3_df$Result_Value=="REACTIVE 1:8,192"]<-"REACTIVE"
Phase3_df$Result_Value[Phase3_df$Result_Value=="RPR TITER * REACTIVE 1:16"]<-"REACTIVE"
Phase3_df$Result_Value[Phase3_df$Result_Value=="A"]<-"NULL"
Phase3_df$Result_Value[Phase3_df$Result_Value=="SERUM"]<-"NULL"
Phase3_df$Result_Value[Phase3_df$Result_Value=="FTA ABS"]<-"NULL"
Phase3_df$Result_Value[Phase3_df$Result_Value=="SEE BELOW"]<-"NULL"
Phase3_df$Result_Value[Phase3_df$Result_Value=="REACTIVE 1:64"]<-"REACTIVE"
Phase3_df$Result_Value[Phase3_df$Result_Value=="REACTIVE 1:2048"]<-"REACTIVE"
Phase3_df$Result_Value[Phase3_df$Result_Value=="REACTIVE 1:2,048"]<-"REACTIVE"
Phase3_df$Result_Value[Phase3_df$Result_Value=="RPR REACTIVE 1"]<-"REACTIVE"
Phase3_df$Result_Value[Phase3_df$Result_Value=="RPR TITER * REACTIVE 1:4"]<-"REACTIVE"
Phase3_df$Result_Value[Phase3_df$Result_Value=="RPR TITER * REACTIVE 1:8"]<-"REACTIVE"
Phase3_df$Result_Value[Phase3_df$Result_Value=="REACTIVE 1"]<-"REACTIVE"
Phase3_df$Result_Value[Phase3_df$Result_Value=="REACTIVE MINIMAL"]<-"EQUIVOCAL"
Phase3_df$Result_Value[Phase3_df$Result_Value=="Equivocal"]<-"EQUIVOCAL"
Phase3_df$Result_Value[Phase3_df$Result_Value=="TNP"]<-"NULL"
Phase3_df$Result_Value[Phase3_df$Result_Value=="NA's"]<-"NULL"

summary(as.factor(Phase3_df$Result_Value))
summary(as.factor(Phase3_df$Result_Name))

recode_list1 <- list(
  "RPR" = c("RPR (MONITOR) WITH REFLEX TO TITER",
                   "RPR SCREEN",
                   "RPR SCREEN W/ RFLX",
                   "RPR"),
  "RPR Quant" = c("RPR QUANTITATIVE", "RPR TITER"),
  "Syph AB" = c("T. PALLIDUM AB", "T. PALLIDUM AB (TP-PA)",
                "T. PALLIDUM AB, EIA", "TREPONEMA PALLIDUM AB,PA"),
  "T. PALLIDUM IFA" = c("TREPONEMA PALLIDUM ANTIBODY, IFA (CSF)", "T. PALLIDUM AB, IFA"),
  "FTA-ABS" = c("FTA-ABS", "PROGRESSIVE FTA-ABS", "FTA-ABS, SERUM"),
  "T. PALLIDUM DNA" = c("TREPONEMA PALLIDUM DNA", "T. PALLIDUM DNA,QL RT-PCR",
                        "TREPONEMA PALLIDUM DNA, QUALITATIVE REAL-TIME PCR")
)

reassign_values_by_list <- function(vector, list, new_label) {
  vector[which(vector %in% list[[new_label]])] <- new_label
  return(vector)
}


updated_syph_df <- Phase3_df %>%
  filter(Result_Value == "REACTIVE") %>%
  mutate(Result_Name = reassign_values_by_list(vector = Result_Name, 
                                               list = recode_list1, 
                                               new_label = "RPR"),
         Result_Name = reassign_values_by_list(vector = Result_Name, 
                                               list = recode_list1, 
                                               new_label = "RPR QUANT"),
         Result_Name = reassign_values_by_list(vector = Result_Name, 
                                               list = recode_list1, 
                                               new_label = "Syph AB"),
         Result_Name = reassign_values_by_list(vector = Result_Name, 
                                               list = recode_list1, 
                                               new_label = "T. PALLIDUM IFA"),
         Result_Name = reassign_values_by_list(vector = Result_Name, 
                                               list = recode_list1, 
                                               new_label = "FTA-ABS"),
         Result_Name = reassign_values_by_list(vector = Result_Name, 
                                               list = recode_list1, 
                                               new_label = "T. PALLIDUM DNA")
                                               )
  filter(Result_Name %in% c("RPR", "RPR QUANT", "Syph AB", "T. PALLIDUM IFA", "FTA-ABS", "T. PALLIDUM DNA")) %>%
  #group_by(Result_Name)
    group_by(Phase3_df$Study_Participant_ID) %>%
    summarise(total_tests = n(),
            detection_rate = 100 * length(which(Result_Value == "DETECTED")) / n()) %>%
    arrange(desc(detection_rate))

print(updated_syph_df$Result_Name)



table(updated_syph_df$Result_Name)
ggplot(data = updated_syph_df, aes(Result_Value, fill = Result_Name)) +
  geom_bar(position = "dodge2")



summary(as.factor(Phase3_df$Result_Value))
table(as.factor(Phase3_df$Result_Value))
barplot(table(Phase3_df$Result_Value))
ggplot(Phase3_df, aes(x=Result_Value))+ geom_bar()

write_xlsx(Phase3_df, "Desktop/Summer Research/Phase2_syphcleaner_rate.xlsx")

#Using the traditional algorithm 

positivessyph <- function(dataframe) {
  participantssyph <- unique(dataframe[["Study_Participant_ID"]])
  
  outcomessyph <- sapply(participantssyph, function(id) {
    reducedsyph <- dataframe %>%
      filter(Study_Participant_ID == id) 
    
    if ("REACTIVE" %in% reducedsyph[["Result_Value"]]) {
      positivesyph <- reducedsyph %>%
        filter(Result_Value == "DETECTED")
      
      if (("RPR" %in% positivesyph[["Result_Name"]] & 
           "RPR QUANTITATIVE" %in% positivesyph[["Result_Name"]] &
           "FTA-ABS" %in% positivesyph[["Result_Name"]])) {
        "Confirmed Syph"
      } else if (("RPR" %in% positivesyph[["Result_Name"]] & 
                  "RPR QUANTITATIVE" %in% positivesyph[["Result_Name"]]&
                  "T. PALLIDUM IFA" %in% positivesyph[["Result_Name"]])) {
        "Confirmed Syph"
      } else if (("RPR" %in% positivesyph[["Result_Name"]] & 
                  "RPR QUANTITATIVE" %in% positivesyph[["Result_Name"]]&
                  "T. PALLIDUM AB, TP-PA" %in% positivesyph[["Result_Name"]])) {
        "Confirmed Syph"
      } else if (("RPR" %in% positivesyph[["Result_Name"]] & 
                  #How to say that RPR QUANTITATIVE IS NOT POSITIVE
                  "RPR QUANTITATIVE" %notin% positivesyph[["Result_Name"]]&
                  "T. PALLIDUM IFA" %in% positivesyph[["Result_Name"]])) {
        "Confirmed Syph"
      } else if (("RPR" %notin% positivesyph[["Result_Name"]])) {
        "NEGATIVE Syph"
      } else {
        "ERROR"
      }
    } else {
      "No positives"
    }
  })
  
  list("participant" = participantssyph,
       "outcomes" = outcomessyph) %>%
    as.data.frame() %>%
    as_tibble()
}


positivessyph(dataframe = updated_syph_df) %>%
  filter(outcomessyph != "No positives") %>%
  ggplot(data = , aes(outcomessyph)) +
  geom_bar(position = "dodge2", fill="dark blue")


group_by(Phase3_df$Study_Participant_ID) %>%
  summarise(total_tests = n(),
            detection_rate = 100 * length(which(Result_Value == "DETECTED")) / n()) %>%
  arrange(desc(detection_rate))


