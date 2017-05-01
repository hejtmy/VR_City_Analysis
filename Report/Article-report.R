source("R/LoadingScript.R")
library(ggthemes)
library(wesanderson)
data_dir = "U:/OneDrive/NUDZ/HCENAT/Data/"

subject_table = read.table(paste(data_dir, "ListOfSubjects.csv", sep = ""), sep = ",", 
                           header = T, stringsAsFactors = F, na.strings = c(""))

SESSION = 1

subject_table = subject_table[c(16, 18), ]
#loads from the subjectList table
Analyses = MultiParticipantUnityAnalysis$new(data_dir, subject_table, SESSION, override = F)

# -------- UNITY EYE ---------
Analyses$load_unity_eyetracker()
tabEyeQuests = Analyses$EyetrackerQuestsSummary()

path_plt <- Analyses$Data$HCE_E_4$UnityEyetracker$DrawQuestPath(9)
path_plt <- path_plt + theme_bw() + theme(legend.position='none')
path_plt + xlab("Postion X") + ylab("Position Z") + scale_color_brewer(palette="Set1")

# -------- UNITY MRI ---------
Analyses$load_unity_mri()
tabMRIQuests = Analyses$MRIQuestSummary()

# -------- EYETRACKER ---------
Analyses$load_eyetracker()
synced_fixations = Analyses$synchronise_eyetracker()

areas = create_areas()
synced_fixations = add_screen_area_fixations(synced_fixations, areas)
fix_summary = make_fixations_summary(synced_fixations)

# -------- POINTING ---------
correct_angles = read.csv(paste(data_dir, "correct-angles.csv", sep = ""),
                          header = T, stringsAsFactors = F, na.strings = c(""))
dt_pointing = Analyses$pointing_summary(override = T, correct_angles = correct_angles)
