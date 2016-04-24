#How to work with this
VR analysis is written largely with support of R6 reference class system for better handeling or interdependent data. This might be a bit counterintuitive at first if you are not used to using classes in R, but eventually allows for buffering, readibility etc.


# Basic uses
Open .Rproject

1. Open Main.R

2. Change data_dir appropriate to the path where the data files are located

```r
#sets the working dirrectory where the logs are
data_dir <- "../Data"
```

3. Check you have correct List of subjects file loaded

```r
#loading subject
subject_table = read.table("../Data/ListOfSubjects.csv", sep = ";", header=T, stringsAsFactors = F, na.strings = c(""))
```
4. Start class initialisation

```r
#loading dependencies
source("Scripts/LoadingScript.R")
```

## Single participant analysis
### Eyetracker
```r
#loading subject
Analysis = UnityEyetrackerAnalysis$new(data_dir,id="HCE_1_E_3",session=1)
Analysis$MakePathImage(8)
Analysis$QuestSummary(7)
Analysis$QuestsSummary()
Analysis$DrawQuestPath(2)
```
###MRI
```r
#loading subject
Analysis =  UnityMRIAnalysis$new(data_dir,id="HCE_1_E_2")
Analysis$MakePathImage(8)
Analysis$QuestSummary(2)
Analysis$QuestsSummary()
Analysis$DrawQuestPath(1)
```

##Multi-participant analysis
Loads all appropriate MRI and Eyetracker Unity files 
```r
Analyses = MultiParticipantUnityAnalysis$new(data_dir,subject_table,1)
tab = Analyses$EyetrackerQuestsSummary()
tabMRI = Analyses$MRIQuestSummary()
```
##Basic analysis
```r
t.test(tab$time~tab$type)
#anova model
summary(aov(time~id, tab))
summary(aov(time~type*participant_id, tab))
```

## Saving data
```r
name = "saved_table.csv"
tab = Analysis$QuestsSummary()
write.csv(tab,name)

tab = Analyses$EyetrackerQuestsSummary()
tabMRI = Analyses$MRIQuestSummary()
write.csv(tab,name)
```


##Classes
Classes you shoudl be familiar with are:
* EyetrackerUnityAnalysis
  * fields
  * methods

* MultiParticipantUnityAnalysis
  * fields
  * methods
