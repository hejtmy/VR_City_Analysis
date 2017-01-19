source("Scripts/LoadingScript.R")


Analyses = MultiParticipantUnityAnalysis$new(data_dir, subject_table, SESSION, override = F)
Analyses$load_unity_eyetracker()
tabEyeQuests = Analyses$EyetrackerQuestsSummary()
all.equal(tabEyeQuests, test_tabEyeQuests)
# -------- UNITY MRI ---------
Analyses$load_unity_mri()
tabMRIQuests = Analyses$MRIQuestSummary()
tabMRIPulses = Analyses$synchropulses_table()
all.equal(test_tabMRIPulses, tabMRIPulses)
all.equal(test_tabMRIQuests, tabMRIQuests)
# -------- EYETRACKER ---------
Analyses$load_eyetracker()
synced_fixations = Analyses$synchronise_eyetracker()

areas = create_areas()
synced_fixations = add_screen_area_fixations(synced_fixations, areas)
all.equal(test_tabMRIPulses, tabMRIPulses)
all.equal(test_tabMRIQuests, tabMRIQuests)

# -------- POINTING ---------
correct_angles = read.csv(paste(data_dir, "correct-angles.csv", sep = ""),
                          header = T, stringsAsFactors = F, na.strings = c(""))
dt_pointing = Analyses$pointing_summary(override = T, correct_angles = correct_angles)
all.equal(test_dt_pointing, dt_pointing)
