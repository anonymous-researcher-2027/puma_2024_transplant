# puma_2024_transplant

"1.download_motus_data.R" downloads motus data from the Motus website for our Motus project; 
You will need to use your own Motus account information to login for this script;
This script is used to create the file "1.filtered_motus_data_puma.csv"; 
This csv file was then manually filtered to remove additional false positives
"1.filtered_motus_data_puma.csv" is contained within this repository in the data folder.

"2.filtering_motus_data.R" uses "1.filtered_motus_data_puma.csv" created from the previous script as well as metadata from "PUMA_Nestling_Tag_SUMMARY_ALL_2024.csv"--both of which are contained within this repository. 
This script further filters motus data to create the file "csv4.csv" which is used for downstream analyses. 

"3.looking_at_detection_histories.R" further filters and formats Motus data downloaded in the previous script; 
This script uses "csv4.csv" described previously, and also creates a folder of .csv files (one file per individual bird tracked), which are all stored in the folder "Individual_Motus_Data". 
This script removes Motus data columns which were irrelevant to our movement analyses. 

"4.extracting_24hr_and_colony_departure_per_individual.R" extracts first 24 hr colony departure and final colony departure variables for each individual bird tracked (of individuals which lived long enough to obtain one or both variables). 
This script creates "motus_summary.csv" and "motus_24hr_gap_and_departure_by_tag.csv". 

"5.native_comparison.R" compares local (native, or Control/Sham individuals) between sites for all 3 timing metrics examined. 
This script uses "motus_summary.csv" created in the previous step. 
This code was used to generate the stats and plots used for the local comparisons shown in Figure 2, as well as Table S5, in the paper. 

"6.stats_and_reaction_norms_bm.R" contains statistical analyses for the timing metrics examined; this scrip was used to create Figure 3 and Figure 4 in the paper. 
This script uses "motus_summary_21jan2026.csv" contained within the repository. 

"7.model-tables-phenological-traits.R" uses "motus_summary_21jan2026.csv" to model dredge for all 3 timing metrics. This code was used in conjunction with the script "8.newest_model_dredging_and_averaging.R" to create Table 1. This code is actually the outdated version of "8.newest_model_dredging_and_averaging.R" since we ended up analyzing "site" differently in the subsequent script. This script was retained to show all steps but can be skipped when replicating as it does not contribute to the final product. 

"8.newest_model_dredging_and_averaging" uses "motus_summary_21jan2026.csv" to model dredge for all 3 timing metrics. This script was used to create Table 1 from the paper. 

"9.growth_and_hatch.R" uses "growth_with_rates.csv" to analyze growth and hatch rates data. This script is used to create Figure 5, Figure S2, and Table S5 from the paper. 


"10.photoperiod_differences.R" compares photoperiod differences between the two sites (MN and MB) during the nestling provisioning period and is used to create Figure S1 from the paper. 
