#How to work with this
VR analysis is written largely with support of R6 reference class system for better handeling or interdependent data. This might be a bit counterintuitive at first if you are not used to using classes in R, but eventually allows for buffering, readibility etc.


# Basic uses
Open .Rproject
1. Open Main.R
2. Change data_dir appropriate to the path where the data files are located
3. Start class initialisation

```r
#loading dependencies
source("Scripts/Classes/EyetrackerUnityAnalysis.R")
source("Scripts/Classes/MultiParticipantUnityAnalysis.R")

#sets the working dirrectory where the logs are
data_dir <- "../Data"

#loads either one patient
Analysis = UnityEyetrackerAnalysis$new(data_dir,id="HCE_1_E_3",session=1)
Analysis$ReadData()

#or multiple
#choose participants
participants = c("HCE_1_E_1","HCE_1_E_2","HCE_1_E_3","HCE_1_E_4","HCE_1_E_5")
Analyses = MultiParticipantUnityAnalysis$new(data_dir,participants,1)
```

### Possible things to do
Quest info
```r
#single patient
quest_table = Analysis$QuestsSummary()
#or
quest_table = Analyses$Analyses$HCE_1_E_1$QuestsSummary()

#multiple patients
quests_table = Analyses$QuestsSummary()
```

Basic analysis

```°r
t.test(tab$time~tab$type)
#anova model
summary(aov(time~id, ab))
summary(aov(time~type*participant_id, ab))
```

##Classes
Classes you shoudl be familiar with are:
* EyetrackerUnityAnalysis
  * fields
  * methods

* MultiParticipantUnityAnalysis
  * fields
  * methods





This is [on GitHub](https://github.com/jbt/markdown-editor) so let me know if I've b0rked it somewhere.


Props to Mr. Doob and his [code editor](http://mrdoob.com/projects/code-editor/), from which
the inspiration to this, and some handy implementation hints, came.
