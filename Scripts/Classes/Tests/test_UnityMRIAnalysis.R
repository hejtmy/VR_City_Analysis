#MRI analysis
AnalysisMRI = UnityMRIAnalysis$new(data_dir, id = "HCE_E_10", session = 1)
AnalysisMRI$QuestSummary(2)
AnalysisMRI$QuestsSummary()
AnalysisMRI$DrawQuestPath(1)
AnalysisMRI$pointing_summary()
