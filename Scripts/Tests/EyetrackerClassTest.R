data_dir = "U:/OneDrive/NUDZ/HCENAT/Data/"
participant_code = "HCE_E_2"
edf_code = "HCE_1E2"
override = F
save = T
eye = EyetrackerAnalysis$new(data_dir, participant_code, edf_code, override, save)

unity_class = UnityEyetrackerAnalysis$new(data_dir, id = participant_code, session = 1)

eye$summary(quest_times = unity_class$quests_timewindows(T))

events = eye$events
fixations = eye$fixations
quest_times = unity_class$quests_timewindows(T)
synchro_times = unity_class$event_times("EyetrackerSynchro")
