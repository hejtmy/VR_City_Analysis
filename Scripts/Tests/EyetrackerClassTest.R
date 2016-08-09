source("Scripts/LoadingScript.R")

data_dir = "U:/OneDrive/NUDZ/HCENAT/Data/"
participant_code = "HCE_E_15"
edf_code = "HCE_1E15"

unity = UnityEyetrackerAnalysis$new(data_dir, id = participant_code, session = 1)
eye = EyetrackerAnalysis$new(data_dir, participant_code, edf_code, unity_class = unity, override = F, save = T)

tm = eye$summary(unity)

tevents = eye$events
fixations = eye$fixations
quest_times = unity_class$quests_timewindows(T)

eyes = select_experiment(Analyses$Data, "Eyetracker")
sapply(eyes, function(x) x$events[type == "l", .N])
sapply(eyes, function(x) is.null(x$events))
