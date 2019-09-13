#This script reads in a cleaned version of the annotator data with no overlap in parallels
#Tags are converted to numeric values
#The resulting "data" object is used by, e.g. test_linear_model.R

JCTL <- read.csv('../data/uniquedata.csv')

#isolate the rows which contain each broad topic

JCTL$SOCIAL_CONTENT <- 0
for (i in c("social-intercourse_hospitality", "social-intercourse_hospitality_guest-arrival-andor-reception", "social-intercourse_hospitality_meals", "social-intercourse_hospitality_entertainment", "social-intercourse_hospitality_retiring-at-night", "social-intercourse_hospitality_departure-andor-gifts", "social-intercourse_messengers", "social-intercourse_reminiscence", "social-intercourse_dreams", "social-intercourse_disguise", "social-intercourse_recognition", "social-intercourse_reputation-or-fame", "social-intercourse_divine-visit", "social-intercourse_conversation", "social-intercourse_greeting", "social-intercourse_assembly-andor-dismissal", "social-intercourse_ransom", "social-intercourse_supplication", "social-intercourse_dressing-andor-adornment", "social-intercourse_benediction", "social-intercourse_allurement-andor-seduction")) {
  
  JCTL$SOCIAL_CONTENT[grep(i, JCTL$TOPIC_CONTENT, ignore.case = TRUE)] <- JCTL$SOCIAL_CONTENT[grep(i, JCTL$TOPIC_CONTENT, ignore.case = TRUE)] + 1
  
}

JCTL$BATTLE_CONTENT <- 0
for (i in c("battle", "battle_deathscene", "battle_trope", "battle_aresteai-andor-duels", "battle_arming", "battle_catalogues-andor-obituaries", "battle_speeches")) {
  
  JCTL$BATTLE_CONTENT[grep(i, JCTL$TOPIC_CONTENT, ignore.case = TRUE)] <- JCTL$BATTLE_CONTENT[grep(i, JCTL$TOPIC_CONTENT, ignore.case = TRUE)] + 1
  
}

JCTL$TRAVEL_CONTENT <- 0
for (i in c("travel", "travel_by", "travel_arrival", "travel_in", "travel_depar")) {
  
  JCTL$TRAVEL_CONTENT[grep(i, JCTL$TOPIC_CONTENT, ignore.case = TRUE)] <- JCTL$TRAVEL_CONTENT[grep(i, JCTL$TOPIC_CONTENT, ignore.case = TRUE)] + 1  
  
}

JCTL$RITUAL_CONTENT <- 0
for (i in c("ritual","ritual_v","ritual_pr","ritual_sac","ritual_fun","ritual_om","ritual_lib","ritual_oath","ritual_pur", "ritual_prayer_fulfilled", "ritual_prayer_un")) {
  
  JCTL$RITUAL_CONTENT[grep(i, JCTL$TOPIC_CONTENT, ignore.case = TRUE)] <- JCTL$RITUAL_CONTENT[grep(i, JCTL$TOPIC_CONTENT, ignore.case = TRUE)] + 1
  
}


JCTL$SPEECH_CONTENT <- 0
for (i in c("speeches-and-deliberation", "speeches-and-deliberation_del", "speeches-and-deliberation_test", "speeches-and-deliberation_lam", "speeches-and-deliberation_pers", "speeches-and-deliberation_cons", "speeches-and-deliberation_prop", "speeches-and-deliberation_disa")) {
  
  JCTL$SPEECH_CONTENT[grep(i, JCTL$TOPIC_CONTENT, ignore.case = TRUE)] <- JCTL$SPEECH_CONTENT[grep(i, JCTL$TOPIC_CONTENT, ignore.case = TRUE)] + 1
  
}

JCTL$FAMILY_CONTENT <- 0
for (i in c("family", "family_par", "family_bir", "family_desc", "family_sib", "family_spo")) {
  
  JCTL$FAMILY_CONTENT[grep(i, JCTL$TOPIC_CONTENT, ignore.case = TRUE)] <- JCTL$FAMILY_CONTENT[grep(i, JCTL$TOPIC_CONTENT, ignore.case = TRUE)] + 1
  
}

JCTL$DESCRIPTION_CONTENT <- 0

for (i in c("description", "description_time", "description_char", "description_place", "description_obj", "description_cos", "description_sim", "description_weat", "description_simile_animal")) {
  
  JCTL$DESCRIPTION_CONTENT[grep(i, JCTL$TOPIC_CONTENT, ignore.case = TRUE)] <- JCTL$DESCRIPTION_CONTENT[grep(i, JCTL$TOPIC_CONTENT, ignore.case = TRUE)] + 1
  
}


JCTL$FATE_CONTENT <- 0
for (i in c("fate", "fate_death", "fate_divine", "fate_apo")) {
  
  JCTL$FATE_CONTENT[grep(i, JCTL$TOPIC_CONTENT, ignore.case = TRUE)] <- JCTL$FATE_CONTENT[grep(i, JCTL$TOPIC_CONTENT, ignore.case = TRUE)] + 1
  
}


JCTL$SENTIMENT_CONTENT <- 0
for (i in c("sentiment", "sentiment_ang", "sentiment_frus", "sentiment_happ", "sentiment_sad", "sentiment_fea", "sentiment_pri", "sentiment_lov", "sentiment_rev")) {
  
  JCTL$SENTIMENT_CONTENT[grep(i, JCTL$TOPIC_CONTENT, ignore.case = TRUE)] <- JCTL$SENTIMENT_CONTENT[grep(i, JCTL$TOPIC_CONTENT, ignore.case = TRUE)] + 1
  
}

JCTL$CHARACTER_CONTENT <- 0
for (i in c("charactertype", "charactertype_hero", "charactertype_hero_specificindividual", "charactertype_divine", "charactertype_divine_specificindividual","charactertype_same-trade","charactertype_same-trade_specificindividual")){
  
  JCTL$CHARACTER_CONTENT[grep(i, JCTL$TOPIC_CONTENT, ignore.case = TRUE)] <- JCTL$CHARACTER_CONTENT[grep(i, JCTL$TOPIC_CONTENT, ignore.case = TRUE)] + 1
  
}


JCTL$SOCIAL_CONTEXT <- 0
for (i in c("social-intercourse_hospitality", "social-intercourse_hospitality_guest-arrival-andor-reception", "social-intercourse_hospitality_meals", "social-intercourse_hospitality_entertainment", "social-intercourse_hospitality_retiring-at-night", "social-intercourse_hospitality_departure-andor-gifts", "social-intercourse_messengers", "social-intercourse_reminiscence", "social-intercourse_dreams", "social-intercourse_disguise", "social-intercourse_recognition", "social-intercourse_reputation-or-fame", "social-intercourse_divine-visit", "social-intercourse_conversation", "social-intercourse_greeting", "social-intercourse_assembly-andor-dismissal", "social-intercourse_ransom", "social-intercourse_supplication", "social-intercourse_dressing-andor-adornment", "social-intercourse_benediction", "social-intercourse_allurement-andor-seduction")) {
  
  JCTL$SOCIAL_CONTEXT[grep(i, JCTL$TOPIC_CONTEXT, ignore.case = TRUE)] <- JCTL$SOCIAL_CONTEXT[grep(i, JCTL$TOPIC_CONTEXT, ignore.case = TRUE)] + 1
  
}

JCTL$BATTLE_CONTEXT <- 0
for (i in c("battle", "battle_deathscene", "battle_trope", "battle_aresteai-andor-duels", "battle_arming", "battle_catalogues-andor-obituaries", "battle_speeches")) {
  
  JCTL$BATTLE_CONTEXT[grep(i, JCTL$TOPIC_CONTEXT, ignore.case = TRUE)] <- JCTL$BATTLE_CONTEXT[grep(i, JCTL$TOPIC_CONTEXT, ignore.case = TRUE)] + 1
  
}

JCTL$TRAVEL_CONTEXT <- 0
for (i in c("travel", "travel_by", "travel_arrival", "travel_in", "travel_depar")) {
  
  JCTL$TRAVEL_CONTEXT[grep(i, JCTL$TOPIC_CONTEXT, ignore.case = TRUE)] <- JCTL$TRAVEL_CONTEXT[grep(i, JCTL$TOPIC_CONTEXT, ignore.case = TRUE)] + 1  
  
}

JCTL$RITUAL_CONTEXT <- 0
for (i in c("ritual","ritual_v","ritual_pr","ritual_sac","ritual_fun","ritual_om","ritual_lib","ritual_oath","ritual_pur", "ritual_prayer_fulfilled", "ritual_prayer_un")) {
  
  JCTL$RITUAL_CONTEXT[grep(i, JCTL$TOPIC_CONTEXT, ignore.case = TRUE)] <- JCTL$RITUAL_CONTEXT[grep(i, JCTL$TOPIC_CONTEXT, ignore.case = TRUE)] + 1
  
}


JCTL$SPEECH_CONTEXT <- 0
for (i in c("speeches-and-deliberation", "speeches-and-deliberation_del", "speeches-and-deliberation_test", "speeches-and-deliberation_lam", "speeches-and-deliberation_pers", "speeches-and-deliberation_cons", "speeches-and-deliberation_prop", "speeches-and-deliberation_disa")) {
  
  JCTL$SPEECH_CONTEXT[grep(i, JCTL$TOPIC_CONTEXT, ignore.case = TRUE)] <- JCTL$SPEECH_CONTEXT[grep(i, JCTL$TOPIC_CONTEXT, ignore.case = TRUE)] + 1
  
}

JCTL$FAMILY_CONTEXT <- 0
for (i in c("family", "family_par", "family_bir", "family_desc", "family_sib", "family_spo")) {
  
  JCTL$FAMILY_CONTEXT[grep(i, JCTL$TOPIC_CONTEXT, ignore.case = TRUE)] <- JCTL$FAMILY_CONTEXT[grep(i, JCTL$TOPIC_CONTEXT, ignore.case = TRUE)] + 1
  
}

JCTL$DESCRIPTION_CONTEXT <- 0

for (i in c("description", "description_time", "description_char", "description_place", "description_obj", "description_cos", "description_sim", "description_weat", "description_simile_animal")) {
  
  JCTL$DESCRIPTION_CONTEXT[grep(i, JCTL$TOPIC_CONTEXT, ignore.case = TRUE)] <- JCTL$DESCRIPTION_CONTEXT[grep(i, JCTL$TOPIC_CONTEXT, ignore.case = TRUE)] + 1
  
}


JCTL$FATE_CONTEXT <- 0
for (i in c("fate", "fate_death", "fate_divine", "fate_apo")) {
  
  JCTL$FATE_CONTEXT[grep(i, JCTL$TOPIC_CONTEXT, ignore.case = TRUE)] <- JCTL$FATE_CONTEXT[grep(i, JCTL$TOPIC_CONTEXT, ignore.case = TRUE)] + 1
  
}


JCTL$SENTIMENT_CONTEXT <- 0
for (i in c("sentiment", "sentiment_ang", "sentiment_frus", "sentiment_happ", "sentiment_sad", "sentiment_fea", "sentiment_pri", "sentiment_lov", "sentiment_rev")) {
  
  JCTL$SENTIMENT_CONTEXT[grep(i, JCTL$TOPIC_CONTEXT, ignore.case = TRUE)] <- JCTL$SENTIMENT_CONTEXT[grep(i, JCTL$TOPIC_CONTEXT, ignore.case = TRUE)] + 1
  
}

JCTL$CHARACTER_CONTEXT <- 0
for (i in c("charactertype", "charactertype_hero", "charactertype_hero_specificindividual", "charactertype_divine", "charactertype_divine_specificindividual","charactertype_same-trade","charactertype_same-trade_specificindividual")){
  
  JCTL$CHARACTER_CONTEXT[grep(i, JCTL$TOPIC_CONTEXT, ignore.case = TRUE)] <- JCTL$CHARACTER_CONTEXT[grep(i, JCTL$TOPIC_CONTEXT, ignore.case = TRUE)] + 1
  
}


#Remove all non-aeneid matches
JCTL <- JCTL[grep("vergil", JCTL$TARGET),]

#Fix all missing difficulties
JCTL$DIFFICULTY[which(is.na(JCTL$DIFFICULTY))] <- 1

# Should consider number of semantic matches per line of the Aeneid, as opposed to semantic matches total.
#ALSO USED IN CLEANING.

for (i in 1:length(JCTL$TARGET_STOP)) {
  
  JCTL$TARGET_LENGTH[i] <-as.numeric(gsub("1_|a|b", "", JCTL$TARGET_STOP[i])) - as.numeric(gsub("1_|a|b", "", JCTL$TARGET_START[i])) + 1
}
#JCTL <- JCTL[which(JCTL$TARGET_LENGTH < 8),]


JCTL <- JCTL[!is.na(JCTL$TARGET_LENGTH),]
# do it again for Homer

for (i in 1:length(JCTL$SOURCE_STOP)) {
  
  JCTL$SOURCE_LENGTH[i] <-as.numeric(gsub("\\d\\d?_|a|b", "", JCTL$SOURCE_STOP[i])) - as.numeric(gsub("\\d\\d?_|a|b", "", JCTL$SOURCE_START[i])) + 1
}
#JCTL <- JCTL[which(JCTL$SOURCE_LENGTH < 11),]


JCTL <- JCTL[!is.na(JCTL$SOURCE_LENGTH),]

JCTL$SEMANTIC[which(JCTL$SEMANTIC=="")] <- 0
JCTL$SEMANTIC <- as.numeric(JCTL$SEMANTIC)

# Replace all 'na' with zeroes for sound and semantic and syntax columns
JCTL$SEMANTIC[which(is.na(JCTL$SEMANTIC))] <- 0
JCTL$SOUNDS_NUMERIC <- 0
JCTL$SYNTACTIC_NUMERIC <- 0

#grep the number of word-characters
#take that number and 

library('stringr')

for (i in c(1:length(JCTL$SOUNDS))) {

  JCTL$SOUNDS_NUMERIC[i] <- str_count(JCTL$SOUNDS[i], "\\w")/6  
  
}

# ONE POINT FOR HAVING ANYTHING, ONE MORE FOR EACH COMMA
JCTL$SYNTACTIC_NUMERIC[grep("\\w", JCTL$SYNTACTIC)] <- 1

for (i in c(1:length(JCTL$SYNTACTIC))) {
  
  JCTL$SYNTACTIC_NUMERIC[i] <- JCTL$SYNTACTIC_NUMERIC[i] + str_count(JCTL$SYNTACTIC[i], ",")  
  
}

#add the count of all topics
JCTL$TOTAL_CONTENT <-0
for (i in 1:length(JCTL$TOTAL_CONTENT)) {
  
  JCTL$TOTAL_CONTENT[i] = sum(JCTL[i,c("SOCIAL_CONTENT", "BATTLE_CONTENT", "TRAVEL_CONTENT", "RITUAL_CONTENT", "SPEECH_CONTENT", "FAMILY_CONTENT", "DESCRIPTION_CONTENT", "FATE_CONTENT", "SENTIMENT_CONTENT", "CHARACTER_CONTENT")])
  
}

JCTL$TOTAL_CONTEXT <-0
for (i in 1:length(JCTL$TOTAL_CONTEXT)) {
  
  JCTL$TOTAL_CONTEXT[i] = sum(JCTL[i,c("SOCIAL_CONTEXT", "BATTLE_CONTEXT", "TRAVEL_CONTEXT", "RITUAL_CONTEXT", "SPEECH_CONTEXT", "FAMILY_CONTEXT", "DESCRIPTION_CONTEXT", "FATE_CONTEXT", "SENTIMENT_CONTEXT", "CHARACTER_CONTEXT")])
  
}

data <- JCTL[,c("TYPE", "DIFFICULTY", "SEMANTIC", "SOCIAL_CONTENT", "BATTLE_CONTENT", "TRAVEL_CONTENT", "RITUAL_CONTENT", "SPEECH_CONTENT", "FAMILY_CONTENT", "DESCRIPTION_CONTENT", "FATE_CONTENT", "SENTIMENT_CONTENT", "CHARACTER_CONTENT", "SOCIAL_CONTEXT", "BATTLE_CONTEXT", "TRAVEL_CONTEXT", "RITUAL_CONTEXT", "SPEECH_CONTEXT", "FAMILY_CONTEXT", "DESCRIPTION_CONTEXT", "FATE_CONTEXT", "SENTIMENT_CONTEXT", "CHARACTER_CONTEXT", "TARGET_LENGTH", "SOURCE_LENGTH", "SOUNDS_NUMERIC", "SYNTACTIC_NUMERIC", "TOTAL_CONTENT", "TOTAL_CONTEXT")]

##############################End cleaning of data. The lines below represent further experiments.

write.csv(JCTL, '../current_csv/simplified.csv')