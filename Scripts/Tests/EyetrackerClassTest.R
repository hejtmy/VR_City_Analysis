source("Scripts/LoadingScript.R")

data_dir = "U:/OneDrive/NUDZ/HCENAT/Data/"
participant_code = "HCE_E_10"
edf_code = "HCE_1E10"
override = F
save = T
eye = EyetrackerAnalysis$new(data_dir, participant_code, edf_code, override, save)

unity_class = UnityEyetrackerAnalysis$new(data_dir, id = participant_code, session = 1)

tm = eye$summary(unity_class)

events = eye$events
fixations = eye$fixations
quest_times = unity_class$quests_timewindows(T)

