library("tidyverse")
library("writexl")
library("xlsx")
library("")
recode_list1 <- list(
  "RPR" = c("RPR (MONITOR) WITH REFLEX TO TITER",
            "RPR SCREEN",
            "RPR SCREEN W/ RFLX",
            "RPR"),
  "RPR QUANT" = c("RPR QUANTITATIVE", "RPR TITER"),
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

cleaned_phase_three <- Phase3_df %>%
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
                                               new_label = "T. PALLIDUM DNA")) #%>%
# filter(Result_Name %in% c("RPR", "RPR QUANT", "Syph AB", "T. PALLIDUM IFA", "FTA-ABS", "T. PALLIDUM DNA")) %>%
# group_by(Study_Participant_ID) %>%
# summarise(total_tests = n(),
#           detection_rate = 100 * length(which(Result_Value == "REACTIVE")) / n()) %>%
# arrange(desc(detection_rate))

print(cleaned_phase_three)
write_xlsx(cleaned_phase_three, "Desktop/Summer Research/Phase3_syphm_detection_rate.xlsx")

table(updated_syph_df1$Result_Name)
ggplot(data = cleaned_phase_three, aes(Result_Value, fill = Result_Name)) +
  geom_bar(position = "dodge2")

#Trying to implement algorithm 
#Using the traditional algorithm 

positivessyph <- function(dataframe) {
  participantssyph <- unique(dataframe[["Study_Participant_ID"]])
  
  outcomessyph <- sapply(participantssyph, function(id) {
    reducedsyph <- dataframe %>%
      filter(Study_Participant_ID == id) 
    
    if (c("REACTIVE", "EQUIVOCAL") %in% reducedsyph[["Result_Value"]]) {
      positivesyph <- reducedsyph %>%
        filter(Result_Value == "REACTIVE" | Result_Value == "EQUIVOCAL")
      
      if (("Syph AB" %in% positivesyph[["Result_Name"]] & 
           "RPR" %in% positivesyph[["Result_Name"]] &
           "RPR QUANT" %in% positivesyph[["Result_Name"]])) {
        "Confirmed Syph IR"
      } else if (("Syph AB" %in% positivesyph[["Result_Name"]] & 
                  !("RPR" %in% positivesyph[["Result_Name"]])&
                  "RPR QUANT" %in% positivesyph[["Result_Name"]])) {
        "Confirmed Syph IIR"
      } else if (("Syph AB" %in% positivesyph[["Result_Name"]] & 
                  !("RPR" %in% positivesyph[["Result_Name"]])&
                  "FTA-ABs" %in% positivesyph[["Result_Name"]])) {
        "Confirmed Syph IIIR"
      } else if (("Syph AB" %in% positivesyph[["Result_Name"]] & 
                  !("RPR" %in% positivesyph[["Result_Name"]]) &
                  !("FTA-ABS" %in% positivesyph[["Result_Name"]]))) {
        "Negative Syph IVR"
      } else if ((!("Syph AB" %in% positivesyph[["Result_Name"]]))){
        "Negative Syph VR"
      } else {
        "Not Picked Up by logic"
      }
    } else {
      "No positives"
    }
  })
  
  list("participant" = participantssyph,
       "detailed" = outcomessyph) %>%
    as.data.frame() %>%
    as_tibble() %>%
    mutate(outcomes = ifelse(test = grepl("Confirmed Syph", detailed), 
                             yes = "Confirmed Syph", no = detailed))
}

updated_clean_phase_three <- positivessyph(dataframe = cleaned_phase_three) %>%
  mutate(detailed = as.factor(detailed),
         outcomes = as.factor(outcomes))

summary(updated_clean_phase_three)

not_caught <- updated_clean_phase_three %>%
  filter(detailed == "Not Picked Up by logic") 

not_caught_df <- cleaned_phase_three %>%
  filter(Study_Participant_ID %in% not_caught[["participant"]]) %>%
  write.csv(x = ., 
            file = "/Users/meganwallace/Desktop/Summer Research/not_caught3.csv", 
            row.names = FALSE)