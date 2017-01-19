#instantiates VR_analysis class with the name and project directory 
#it loads appropriate log files and allows for immediate analysis
AnalysisEye = UnityEyetrackerAnalysis$new(data_dir, id = "HCE_E_10", session = 1)
AnalysisEye$QuestSummary(7)
AnalysisEye$QuestsSummary()
AnalysisEye$DrawQuestPath(2)
AnalysisEye$pointing_summary()