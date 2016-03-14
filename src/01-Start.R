## Classification of clinical trials using supervised machine learning
# --------------------------------------------------------------------

# Start of the project
# --------------------

# Script to load the data(XML file) from the clinical trials.gov website and
# parse it create the tables and load all the tables in serverless SQLite database.
# ---------------------------------------------------------------------------------


# Initially raw XML files are all saved in the clinical_trials_XML_files folder
# and will be read from it instead of being read directly from the website.
# -------------------------------------------------------------------------------

# All the supporting scripts are in the folder lib and will be loaded automatically
# when you run load.project() inside the project folder.
# ---------------------------------------------------------------------------------

cat("Setting the temporary variables...\n")
temp <- NULL

# Structures for the elements with multiple childrens
# ---------------------------------------------------

outcome_struct <- c("measure", "time_frame", "safety_issue", "description")
condition_struct <- c("condition")
arm_group_struct <- c("arm_group_label", "arm_group_type", "description")
intervention_struct <- c("intervention_type", "intervention_name", "description", "arm_group_label", "other_name")
eligibilit_struct <- c("study_pop/textblock", "sampling_method", "criteria/textblock", "gender", "minimum_age", "maximum_age")
investigator_struct <- c("first_name", "last_name", "middle_name", "degrees", "role", "affiliation")
address_struct <- c("city", "state", "zip", "country")
link_struct <- c("url", "description")
reference_struct <- c("citation", "PMID")
responsible_party_struct <- c("name_title", "organization", "responsible_party_type", "investigator_affiliation", "investigator_full_name", "investigator_title")

# Structures for results database
# -------------------------------

group_struct <- c("title", "description")
participant_struct <- c("participant_id")
participants_list_struct <- c("participants_list_id")
milestone_struct <- c("title")
measure_struct <- c("title", "description", "units", "param", "dispersion")
results_outcome_struct <- c("type", "title", "description", "time_frame", "safety_issue", "posting_date", "population")
results_outcome_struct_group <- c("NULL")
participant_flow_struct<-c("recruitment_details","pre_assignment_details")
baseline_struct<-c("population")
period_list_struct<-c("title")
participants_list_struct1<-c("NULL")

other_tables <<- list(primary_outcome = outcome_struct, secondary_outcome = outcome_struct, other_outcome = outcome_struct, condition = condition_struct, arm_group = arm_group_struct, 
                      intervention = intervention_struct, eligibility = eligibilit_struct, overall_official = investigator_struct, address = address_struct, link = link_struct, reference = reference_struct, 
                      results_reference = reference_struct, responsible_party = responsible_party_struct, group = group_struct, participants = participant_struct, participants_list = participants_list_struct, 
                      milestone = milestone_struct, `baseline/measure_list/measure` = measure_struct, `baseline/measure_list/measure/category_list` = measure_struct, `outcome_list/outcome` = results_outcome_struct, 
                      `outcome_list/outcome/group_list` = results_outcome_struct_group,participant_flow=participant_flow_struct,'participant_flow/group_list/group'=group_struct,'participant_flow/period_list/period'=period_list_struct,
                      'participant_flow/period_list/period/milestone_list/milestone'=milestone_struct,"participant_flow/period_list/period/milestone_list/milestone/participants_list"=participants_list_struct1,baseline=baseline_struct,'baseline/group_list/group'=group_struct,'baseline/measure_list/measure'=measure_struct)

# Assigning the null to the table names as they form the part of the logic to create the tables in other scripts

temporary_variables <<- paste(names(other_tables), "temp", sep = "_")
for (var in temporary_variables) {
  assign(var, NULL)
}

assign("measure_temp", NULL)
assign("outcome_temp", NULL)
assign("category_details_temp", NULL)
assign("outcome_group_temp", NULL)
assign("group_list_temp", NULL)
assign("period_list_temp",NULL)
assign("milestone_list_temp",NULL)
assign("participants_list_temp",NULL)
assign("group_list_baseline_temp",NULL)
assign("measure_list_temp",NULL)
xmlNodesResults <<- c("group", "participants", "participants_list", "milestone", "measure", "category_details", "outcome", "outcome_group","participant_flow","group_list","period_list","milestone_list","participants_list","baseline","group_list_baseline","measure_list")

files=dir("clinical_trials_XML_files/")
files<-paste("clinical_trials_XML_files",files,sep = "/")

create_observation <- function(file) {
  xmlDoc <- xmlTreeParse(file)
  xmltop <<- xmlRoot(xmlDoc)
  # get the node
  
  xmlNodes <- c("nct_id", "brief_title", "acronym", "official_title", "source", "brief_summary/textblock", "detailed_description/textblock", "overall_status", "start_date", "completion_date", 
                "primary_completion_date", "phase", "study_type", "study_design", "target_duration", "number_of_arms", "number_of_groups", "primary_outcome", "secondary_outcome", "other_outcome", 
                "condition", "arm_group", "intervention", "biospec_retention", "biospec_descr/textblock", "eligibility", "overall_official", "address", "link", "reference", "results_reference", 
                "verification_date", "lastchanged_date", "firstreceived_date", "firstreceived_results_date", "responsible_party")
  
  sapply(xmlNodes, function(node) getNodeAndCreateCell(node))
  
  observation <<- data.frame(nct_id, brief_title, acronym, official_title, source, brief_summary, detailed_description, overall_status, start_date, completion_date, primary_completion_date, 
                             phase, study_type, study_design, target_duration, number_of_arms, number_of_groups, biospec_retention, biospec_descr, verification_date, lastchanged_date, firstreceived_date, 
                             firstreceived_results_date, stringsAsFactors = FALSE)
  temp <<- rbind(temp, observation)
}

invisible(sapply(files,function(file) create_observation(file)))