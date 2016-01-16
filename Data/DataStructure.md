# Data structure in the data folder

This manual should provide insight into how the data is structured inside each folder. It's paramout that you follow these rules as the scripts provided in the Anlaysis folders dependend on the structure.

For example
> Read eyelink files depends on each of the files to be inside folder of the specific id in the eyelink folder. If it's positioned in a different one, the read script will asume no eyetracker was recorder during the session.

## How to structure it

Structure should be geenerally in the following format:

```
DataEADME.md
|---ID
    notes.txt
    |---VR
        |---Session1
            |---Task1
                |---ID_player_timetamp.txt
                |---ID_experiment_timetamp.txt
                |---ID_inventory_timetamp.txt
                |---ID_scenario--name_timetamp.txt
                |---ID_quest1-name_timetamp.txt
                |---ID_quest2-name_timetamp.txt
                |---ID_quest...-name_timetamp.txt
                |---ID_questN-name_timetamp.txt
            |---TaskN
                |---ID_player_timetamp.txt
                |---ID_experiment_timetamp.txt
                |---ID_inventory_timetamp.txt
                |---ID_scenario--name_timetamp.txt
                |---ID_quest1-name_timetamp.txt
                |---ID_quest2-name_timetamp.txt
                |---ID_quest...-name_timetamp.txt
                |---ID_questN-name_timetamp.txt
        |---Session2
        |--- ...
        |---Session7
    |---Eyelink
        |---Session1
            |---link_to_asc.html
            |---link_to_edf.html
        |---Session7
            |---eyelink_stupid_name.txt
            |---eyelink_stupid_name.asc
    |---MRI
        |---Structural
        |---Functional
            |---Session2
    |---VUZIX
```
