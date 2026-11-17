# Template: ElementKlass v1.33

identifier: `b8fd050a-f1f9-495b-bcf7-c4bd8ebcefb4`


# Formulation of the Study


## General Information  (`general_information`)

| field | type | label | options / columns | shown if |
|---|---|---|---|---|
| `full_title` | text | Full Title of Study |  |  |
| `Duration` | text | Duration of the Study |  |  |
| `Date_Duration_Project` | datetime-range | Start date and expected duration |  |  |
| `Link_TVA` | upload | Link to Animal Experiment Application |  |  |
| `Abstract` | textarea | Abstract |  |  |

## Contributors and Communication  (`contributors`)

| field | type | label | options / columns | shown if |
|---|---|---|---|---|
| `Contributors` | table | Contributors | First Name (text); Last Name (text); Institute (text); Affiliation (text); Address (text); Email (text); Phone (text); ORCID (text); Role (select-multi); Regulatory Function (select-multi); Qualification (select-multi); Comment (text) |  |
| `communication_plan` | upload | Upload Communication Plan here |  |  |
| `Organigram` | upload | Upload Organigram here |  |  |

## Study Hypothesis  (`study_hypothesis`)

| field | type | label | options / columns | shown if |
|---|---|---|---|---|
| `Definition_of_a_Study_Hypothesis` | text | Definition of a Study Hypothesis |  |  |
| `Platform` | text | Population |  |  |
| `Intervention` | text | Intervention |  |  |
| `Control` | text | Comparator |  |  |
| `Outcome` | text | Outcome |  |  |
| `Time` | text | Time |  |  |
| `Effect` | text | Environment |  |  |
| `Study_hypothesis` | textarea | Study Hypothesis |  |  |
| `repetition_of_study` | checkbox | Repetition of a study? |  |  |
| `Knowledge_Claim` | checkbox | Knowledge Claim intended? |  |  |
| `reason_for_repetition` | textarea | Justification |  | repetition_of_study=true |
| `Repetition_Literature_Search` | table | Used Databases for Literature Search | Database [DB-ALM (Database on Alternative Methods) / NORINA / ICCVAM / NICEATM / PubMed / Embase / Web of Science / Scopus / Google Scholar / SMAFIRA / NAT Database / TSAR]; Search Terms (text); Limits (text); URL (text) |  |

## Study Overview  (`study_overview`)

| field | type | label | options / columns | shown if |
|---|---|---|---|---|
| `Link_study_designer` | text | Link to Experimental Design Assistant (NC3R) |  |  |
| `Study_plan` | upload | Study Plan Diagram |  |  |
| `Description_Justification_of_Stucy` | textarea | Description and Justification of the intended Study |  |  |

## Pre-Registration  (`preregistration`)

| field | type | label | options / columns | shown if |
|---|---|---|---|---|
| `Preregistration_Check` | checkbox | Did you consider Pre-Registration? |  |  |
| `Preregistration` | table |  | Platform [Preclinicaltrials.eu / www.animalstudyregistry.org / OSF.io / PROSPERO / aspredicted.org / Center for Open Science]; Preregistration Type [Study Protocol / Systematic Review / Report]; ID/DOI (text) | Preregistration_Check=true |

## Risk assessment  (`risk_assessment`)

| field | type | label | options / columns | shown if |
|---|---|---|---|---|
| `explanation` | text | Explanation |  |  |
| `no_one` | select | Is the study hypothesis defined? | yes / no |  |
| `88c03b0c-f0cd-48ad-ae8a-cd678cb10cc6` | text |  |  | no_one=yes & no_one= |
| `no_one_expl` | text | Indicate location of relevant information |  | no_one=no |
| `no_two` | select | Are the outcome parameters defined? | yes / no |  |
| `1273e78a-9f36-4e20-bd93-876fcaff68ff` | text |  |  | no_two=yes & no_two= |
| `no_two_expl` | text | Indicate location of relevant information |  | no_two=no |
| `no_three` | select | Is the sample size justified? | yes / no |  |
| `f0ecf5e5-446e-445a-b222-d14051ec527b` | text |  |  | no_three=yes & no_three= |
| `no_three_expl` | text | Indicate location of relevant information |  | no_three=no |
| `no_four` | select | Inclusion / exclusion criteria defined? | yes / no |  |
| `df5e8f91-73e9-4f79-8219-757d7b71bff8` | text |  |  | no_four=yes & no_four= |
| `no_four_expl` | text | Indicate location of relevant information |  | no_four=no |
| `no_five` | select | Data / statistical analysis plan defined? | yes / no |  |
| `841f8311-102d-4082-baea-4fde26fd4f63` | text |  |  | no_five=yes & no_five= |
| `no_five_expl` | text | Indicate location of relevant information |  | no_five=no |
| `no_six` | select | Group allocation randomized? | yes / no |  |
| `396aaecf-ec4b-4b19-98e2-1067907a0f91` | text |  |  | no_six=yes & no_six= |
| `no_six_expl` | text | Indicate location of relevant information |  | no_six=no |
| `no_seven` | select | Group allocation blinded? | yes / no |  |
| `5ef229ba-a5f0-41eb-b41d-694383f371da` | text |  |  | no_seven=yes & no_seven= |
| `no_seven_expl` | text | Indicate location of relevant information |  | no_seven=no |
| `no_eight` | select | Outcome assessment blinded? | yes / no |  |
| `4c1b833a-e7a6-43f5-89df-b32384f2a92c` | text |  |  | no_eight=yes & no_eight= |
| `no_eight_expl` | text | Indicate location of relevant information |  | no_eight=no |

# Quality Control of Study Components


## Replacement  (`replacement`)

| field | type | label | options / columns | shown if |
|---|---|---|---|---|
| `Alternative_Models` | checkbox | Alternatives to Animal Models |  |  |
| `Justification_Animals` | textarea | Justification for the use of Animal Models |  | Alternative_Models=false |
| `Justification_Animals_Alternatives` | textarea | Justification for the use of Animal Models despite available Alternative Methods |  | Alternative_Models=true |
| `Used_Database_Alternatives` | table | Used Database for Literature Search | Database [DB-ALM (Database on Alternative Methods) / NORINA / ICCVAM / NICEATM / PubMed / Embase / Web of Science / Scopus / Google Scholar / SMAFIRA / NAT Database / TSAR]; Search Terms (text); Limits (text); URL (text) |  |

## Planned Animal Resources  (`animal_resources`)

| field | type | label | options / columns | shown if |
|---|---|---|---|---|
| `Animal_Resources` | table | Planned Animal Characteristics | Species (ontology-select); Strain/Breeding Line (text); Genotype (text); Sex [Male / Female / Unknown/Undetermined / Hermaphrodite]; Age at Start (system-defined); Weight Range (system-defined); Burden Level [None / Mild / Moderate / Severe]; Supplier (text); Total Number (number) |  |

## Justifications for the Choice of intended Animal Models  (`justification_animal_model`)

| field | type | label | options / columns | shown if |
|---|---|---|---|---|
| `Justification_Species` | textarea | Species |  |  |
| `Justification_Age_Sex` | textarea | Age and Sex |  |  |
| `Justification_Lines_Genotypes` | textarea | Lines and Genotypes  |  |  |
| `Justification_Origin` | textarea | Origin |  |  |

## Planned Substances and Materials  (`planned_samples_substances`)

| field | type | label | options / columns | shown if |
|---|---|---|---|---|
| `Samples_Substances` | table | Samples and Substances | Sample (drag_element); Name (text); Substance Type [drug / Anaesthetic / Analgesic / Vehicle / Cell Line / Other]; Dosis (text); Volume (text); Routes of Administration [Oral / Intraveneous (i.v.) / Intraperitoneal (i.p.) / Subcutaneous (s.c.) / Intramuscular (i.m.) / Intradermal / Topical/dermal / Inhalation / Intranasal / Intrathecal / Rectal / Intraocular / Implantation / Other]; Pre-Treatment (textarea); Description (textarea) |  |

## Experimental Procedures  (`experimental_procedures`)

| field | type | label | options / columns | shown if |
|---|---|---|---|---|
| `Research_plan_Yes_No` | checkbox | Use Research Plan Element |  |  |
| `Research_plan_Explanation` | table | Drop Research Plan Element here | Research Plan (drag_element) | Research_plan_Yes_No=true |
| `Experimental_Procedures_Drag` | textarea | Experimental Procedures |  | Research_plan_Yes_No=false |

## Justifications for the intended Experimental Procedures  (`justifications_experimental`)

| field | type | label | options / columns | shown if |
|---|---|---|---|---|
| `Justification_Experimental_Design` | textarea | Justification for the Experimental Design |  |  |
| `Justification_Procedures_Burden` | textarea | Justification for the Procedures and Burden |  |  |

## Raw Data Definitions  (`data_analysis`)

| field | type | label | options / columns | shown if |
|---|---|---|---|---|
| `Raw_Data` | table | Data Definition | Identifier (text); Acquisition Method (text); Instrument (text); Data Format (text); Format Type [Open / Proprietary / Open standard / Partially open]; Storage Location (text); Timestamping [Automatic system timestamp / Manual timestamp entry / Instrument-generated timestamp / File metadata timestamp / Audit trail with versioning / No formal timestamping procedure]; Quality Assurance (select-multi); Verification (text) |  |

# Design of Experiment


## Predictors  (`predictor_variables`)

| field | type | label | options / columns | shown if |
|---|---|---|---|---|
| `Predictors` | table | Predictor Variables | Variable (text); Predictor Role [Primary Factor / Secondary Factor / Covariate / Confounder / Blocking variable]; Predictor Type [Categorial / Ordinal / Continuous]; Value / Range (text); Unit (text); Model Term [Fixed Effect / Random Effect / unknown / none]; Comment (textarea) |  |

## Outcome Measures  (`outcome_measures`)

| field | type | label | options / columns | shown if |
|---|---|---|---|---|
| `Outcome_Measures` | text | Outcome Measures |  |  |
| `Outcome_Measures_Table` | table | Outcome Measures | Outcome Measure (text); Outcome Role [Primary Outcome / Secondary Outcome / Exploratory Outcome]; Outcome Type [Categorial / Ordinal / Continuous]; Acquisition Method (text); Device (text); Unit (text); Timepoint(s) (text); Relevance for the study objectives (textarea) |  |

## Controls and Comparators  (`controls`)

| field | type | label | options / columns | shown if |
|---|---|---|---|---|
| `Controls_used` | checkbox | Controls or Comparators will be applied in the study |  |  |
| `Selection_Controls` | table | Define the Controls and Comparators | Name (text); Type [Positive control / Negativ control / Sham / Genetic Control]; Description (text); In case of Failure (text) | Controls_used=true |
| `Justification_no_Cntrls` | textarea | Rationale for the Absence of Controls or Comparators |  | Controls_used=false |
| `Used_Databases_Controls` | table | Used Databases for Literature Search | Database [DB-ALM (Database on Alternative Methods) / NORINA / ICCVAM / NICEATM / PubMed / Embase / Web of Science / Scopus / Google Scholar / SMAFIRA / NAT Database / TSAR]; Search Terms (text); Limits (text); URL (text) |  |

## Sample Size Calculation  (`sample_size`)

| field | type | label | options / columns | shown if |
|---|---|---|---|---|
| `Introduction_Sample_Size` | text | For more Information on Sample Size please refer to:  |  |  |
| `Selection_Mode` | select | How would you like to determine your sample size? | External tool / guidance / Select method directly |  |
| `Direct_Method_Selection` | select | Select method | One-sample t-test / One-sample Wilcoxon signed-rank test / Wilcoxon rank-sum test / t-test (Student) / Welch t-test / Kruskal-Wallis test / ANOVA (Student) / Welch-ANOVA / pairwise Wilcoxon test / pairwise t-test (Student) / pairwise Welch t-test / Paired t-test / Wilcoxon signed-rank test (paired) / Repeated-measures ANOVA (RM-ANOVA) / Friedman test | Selection_Mode=Select method directly |
| `Direct_Recommended_Test_OneSample_Normal` | text | One-sample t-test |  | Selection_Mode=Select method directly & Direct_Method_Selection=One-sample t-test |
| `Direct_Recommended_Test_OneSample_NonNormal` | text | One-sample Wilcoxon signed-rank test |  | Selection_Mode=Select method directly & Direct_Method_Selection=One-sample Wilcoxon signed-rank test |
| `Direct_CohensD_OneSample` | number | Effect size (Cohen's d) |  | Selection_Mode=Select method directly & Direct_Method_Selection=One-sample t-test |
| `Direct_SigLevel_OneSample_Normal` | number | Significance level |  | Selection_Mode=Select method directly & Direct_Method_Selection=One-sample t-test |
| `Direct_PowerTarget_OneSample_Normal` | number | Desired power |  | Selection_Mode=Select method directly & Direct_Method_Selection=One-sample t-test |
| `Direct_ReferenceValue_OneSample` | number | Reference value |  | Selection_Mode=Select method directly & Direct_Method_Selection=One-sample Wilcoxon signed-rank test |
| `Direct_ExpectedMean_OneSample` | number | Expected mean |  | Selection_Mode=Select method directly & Direct_Method_Selection=One-sample Wilcoxon signed-rank test |
| `Direct_ExpectedSD_OneSample` | number | Expected SD |  | Selection_Mode=Select method directly & Direct_Method_Selection=One-sample Wilcoxon signed-rank test |
| `Direct_AlphaMc_OneSample` | number | Significance level |  | Selection_Mode=Select method directly & Direct_Method_Selection=One-sample Wilcoxon signed-rank test |
| `Direct_PowerTargetMc_OneSample` | number | Desired power |  | Selection_Mode=Select method directly & Direct_Method_Selection=One-sample Wilcoxon signed-rank test |
| `Direct_Nsim_OneSample` | number | Simulations per step |  | Selection_Mode=Select method directly & Direct_Method_Selection=One-sample Wilcoxon signed-rank test |
| `Direct_NMin_OneSample` | number | Minimum n |  | Selection_Mode=Select method directly & Direct_Method_Selection=One-sample Wilcoxon signed-rank test |
| `Direct_NMax_OneSample` | number | Maximum n |  | Selection_Mode=Select method directly & Direct_Method_Selection=One-sample Wilcoxon signed-rank test |
| `Direct_Seed_OneSample` | number | Seed |  | Selection_Mode=Select method directly & Direct_Method_Selection=One-sample Wilcoxon signed-rank test |
| `Direct_Recommended_Test_TwoGroups_NonNormal` | text | Wilcoxon rank-sum test |  | Selection_Mode=Select method directly & Direct_Method_Selection=Wilcoxon rank-sum test |
| `Direct_SelectedPredictors_TwoGroups_NonNormal` | text | Predictors |  | Selection_Mode=Select method directly & Direct_Method_Selection=Wilcoxon rank-sum test |
| `Direct_NGroups_TwoGroups_NonNormal` | number | Number of groups |  | Selection_Mode=Select method directly & Direct_Method_Selection=Wilcoxon rank-sum test |
| `Direct_GroupTable_TwoGroups_NonNormal` | table | Groups (Name / Mean / SD) | Name (text); Mean (number); SD (number) | Selection_Mode=Select method directly & Direct_Method_Selection=Wilcoxon rank-sum test |
| `Direct_AlphaMc_TwoGroups_NonNormal` | number | Significance level |  | Selection_Mode=Select method directly & Direct_Method_Selection=Wilcoxon rank-sum test |
| `Direct_PowerTargetMc_TwoGroups_NonNormal` | number | Desired power |  | Selection_Mode=Select method directly & Direct_Method_Selection=Wilcoxon rank-sum test |
| `Direct_Nsim_TwoGroups_NonNormal` | number | Simulations per step |  | Selection_Mode=Select method directly & Direct_Method_Selection=Wilcoxon rank-sum test |
| `Direct_NMin_TwoGroups_NonNormal` | number | Minimum n per group |  | Selection_Mode=Select method directly & Direct_Method_Selection=Wilcoxon rank-sum test |
| `Direct_NMax_TwoGroups_NonNormal` | number | Maximum n per group |  | Selection_Mode=Select method directly & Direct_Method_Selection=Wilcoxon rank-sum test |
| `Direct_Seed_TwoGroups_NonNormal` | number | Seed |  | Selection_Mode=Select method directly & Direct_Method_Selection=Wilcoxon rank-sum test |
| `Direct_Recommended_Test_TwoGroups_Homogeneous` | text | t-test (Student) |  | Selection_Mode=Select method directly & Direct_Method_Selection=t-test (Student) |
| `Direct_Method_Header_TwoGroups_Homogeneous` | text | Sample size calculation parameters (Power analysis: t-test) |  | Selection_Mode=Select method directly & Direct_Method_Selection=t-test (Student) |
| `Direct_Calculation_Method_TwoGroups_Homogeneous` | select | Calculation Method | Closed-form power analysis / Monte Carlo simulation | Selection_Mode=Select method directly & Direct_Method_Selection=t-test (Student) |
| `Direct_CohensD_TwoGroups_Homogeneous_CF` | number | Effect size (Cohen's d) |  | Selection_Mode=Select method directly & Direct_Method_Selection=t-test (Student) & Direct_Calculation_Method_TwoGroups_Homogeneous=Closed-form power analysis |
| `Direct_SigLevelTTest_TwoGroups_Homogeneous_CF` | number | Significance level |  | Selection_Mode=Select method directly & Direct_Method_Selection=t-test (Student) & Direct_Calculation_Method_TwoGroups_Homogeneous=Closed-form power analysis |
| `Direct_DesiredPowerTTest_TwoGroups_Homogeneous_CF` | number | Desired power |  | Selection_Mode=Select method directly & Direct_Method_Selection=t-test (Student) & Direct_Calculation_Method_TwoGroups_Homogeneous=Closed-form power analysis |
| `Direct_SelectedPredictors_TwoGroups_Homogeneous_MC` | text | Predictors |  | Selection_Mode=Select method directly & Direct_Method_Selection=t-test (Student) & Direct_Calculation_Method_TwoGroups_Homogeneous=Monte Carlo simulation |
| `Direct_NGroups_TwoGroups_Homogeneous_MC` | number | Number of groups |  | Selection_Mode=Select method directly & Direct_Method_Selection=t-test (Student) & Direct_Calculation_Method_TwoGroups_Homogeneous=Monte Carlo simulation |
| `Direct_GroupTable_TwoGroups_Homogeneous_MC` | table | Groups (Name / Mean / SD) | Name (text); Mean (number); SD (number) | Selection_Mode=Select method directly & Direct_Method_Selection=t-test (Student) & Direct_Calculation_Method_TwoGroups_Homogeneous=Monte Carlo simulation |
| `Direct_AlphaMc_TwoGroups_Homogeneous_MC` | number | Significance level |  | Selection_Mode=Select method directly & Direct_Method_Selection=t-test (Student) & Direct_Calculation_Method_TwoGroups_Homogeneous=Monte Carlo simulation |
| `Direct_PowerTargetMc_TwoGroups_Homogeneous_MC` | number | Desired power |  | Selection_Mode=Select method directly & Direct_Method_Selection=t-test (Student) & Direct_Calculation_Method_TwoGroups_Homogeneous=Monte Carlo simulation |
| `Direct_Nsim_TwoGroups_Homogeneous_MC` | number | Simulations per step |  | Selection_Mode=Select method directly & Direct_Method_Selection=t-test (Student) & Direct_Calculation_Method_TwoGroups_Homogeneous=Monte Carlo simulation |
| `Direct_NMin_TwoGroups_Homogeneous_MC` | number | Minimum n per group |  | Selection_Mode=Select method directly & Direct_Method_Selection=t-test (Student) & Direct_Calculation_Method_TwoGroups_Homogeneous=Monte Carlo simulation |
| `Direct_NMax_TwoGroups_Homogeneous_MC` | number | Maximum n per group |  | Selection_Mode=Select method directly & Direct_Method_Selection=t-test (Student) & Direct_Calculation_Method_TwoGroups_Homogeneous=Monte Carlo simulation |
| `Direct_Seed_TwoGroups_Homogeneous_MC` | number | Seed |  | Selection_Mode=Select method directly & Direct_Method_Selection=t-test (Student) & Direct_Calculation_Method_TwoGroups_Homogeneous=Monte Carlo simulation |
| `Direct_Recommended_Test_TwoGroups_Unequal` | text | Welch t-test |  | Selection_Mode=Select method directly & Direct_Method_Selection=Welch t-test |
| `Direct_SelectedPredictors_TwoGroups_Unequal` | text | Predictors |  | Selection_Mode=Select method directly & Direct_Method_Selection=Welch t-test |
| `Direct_NGroups_TwoGroups_Unequal` | number | Number of groups |  | Selection_Mode=Select method directly & Direct_Method_Selection=Welch t-test |
| `Direct_GroupTable_TwoGroups_Unequal` | table | Groups (Name / Mean / SD) | Name (text); Mean (number); SD (number) | Selection_Mode=Select method directly & Direct_Method_Selection=Welch t-test |
| `Direct_AlphaMc_TwoGroups_Unequal` | number | Significance level |  | Selection_Mode=Select method directly & Direct_Method_Selection=Welch t-test |
| `Direct_PowerTargetMc_TwoGroups_Unequal` | number | Desired power |  | Selection_Mode=Select method directly & Direct_Method_Selection=Welch t-test |
| `Direct_Nsim_TwoGroups_Unequal` | number | Simulations per step |  | Selection_Mode=Select method directly & Direct_Method_Selection=Welch t-test |
| `Direct_NMin_TwoGroups_Unequal` | number | Minimum n per group |  | Selection_Mode=Select method directly & Direct_Method_Selection=Welch t-test |
| `Direct_NMax_TwoGroups_Unequal` | number | Maximum n per group |  | Selection_Mode=Select method directly & Direct_Method_Selection=Welch t-test |
| `Direct_Seed_TwoGroups_Unequal` | number | Seed |  | Selection_Mode=Select method directly & Direct_Method_Selection=Welch t-test |
| `Direct_Recommended_Test_Omnibus_NonNormal` | text | Kruskal-Wallis test |  | Selection_Mode=Select method directly & Direct_Method_Selection=Kruskal-Wallis test |
| `Direct_MeanTableOneWay_Omnibus_NonNormal` | table | Level means (table) | Level (text); Mean (number) | Selection_Mode=Select method directly & Direct_Method_Selection=Kruskal-Wallis test |
| `Direct_SdModelOneWay_Omnibus_NonNormal` | select | Variance model | cv / sd | Selection_Mode=Select method directly & Direct_Method_Selection=Kruskal-Wallis test |
| `Direct_CvOneWay_Omnibus_NonNormal` | number | Coefficient of variation |  | Selection_Mode=Select method directly & Direct_Method_Selection=Kruskal-Wallis test & Direct_SdModelOneWay_Omnibus_NonNormal=cv |
| `Direct_SdValueOneWay_Omnibus_NonNormal` | number | Standard deviation |  | Selection_Mode=Select method directly & Direct_Method_Selection=Kruskal-Wallis test & Direct_SdModelOneWay_Omnibus_NonNormal=sd |
| `Direct_SigLevelOneWay_Omnibus_NonNormal` | number | Significance level |  | Selection_Mode=Select method directly & Direct_Method_Selection=Kruskal-Wallis test |
| `Direct_PowerTargetOneWay_Omnibus_NonNormal` | number | Desired power |  | Selection_Mode=Select method directly & Direct_Method_Selection=Kruskal-Wallis test |
| `Direct_NsimOneWay_Omnibus_NonNormal` | number | Simulations per step |  | Selection_Mode=Select method directly & Direct_Method_Selection=Kruskal-Wallis test |
| `Direct_NMinOneWay_Omnibus_NonNormal` | number | Minimum n per level |  | Selection_Mode=Select method directly & Direct_Method_Selection=Kruskal-Wallis test |
| `Direct_NMaxOneWay_Omnibus_NonNormal` | number | Maximum n per level |  | Selection_Mode=Select method directly & Direct_Method_Selection=Kruskal-Wallis test |
| `Direct_SeedOneWay_Omnibus_NonNormal` | number | Seed |  | Selection_Mode=Select method directly & Direct_Method_Selection=Kruskal-Wallis test |
| `Direct_Recommended_Test_Omnibus_Homogeneous` | text | ANOVA (Student) |  | Selection_Mode=Select method directly & Direct_Method_Selection=ANOVA (Student) |
| `Direct_Method_Header_Omnibus_Homogeneous` | text | Power analysis: ANOVA |  | Selection_Mode=Select method directly & Direct_Method_Selection=ANOVA (Student) |
| `Direct_Calculation_Method_Omnibus_Homogeneous` | select | Calculation Method | Closed-form power analysis / Monte Carlo simulation | Selection_Mode=Select method directly & Direct_Method_Selection=ANOVA (Student) |
| `Direct_CohensF_Omnibus_Homogeneous_CF` | number | Effect size (Cohen's f) |  | Selection_Mode=Select method directly & Direct_Method_Selection=ANOVA (Student) & Direct_Calculation_Method_Omnibus_Homogeneous=Closed-form power analysis |
| `Direct_SigLevelAnova_Omnibus_Homogeneous_CF` | number | Significance level |  | Selection_Mode=Select method directly & Direct_Method_Selection=ANOVA (Student) & Direct_Calculation_Method_Omnibus_Homogeneous=Closed-form power analysis |
| `Direct_DesiredPowerAnova_Omnibus_Homogeneous_CF` | number | Desired power |  | Selection_Mode=Select method directly & Direct_Method_Selection=ANOVA (Student) & Direct_Calculation_Method_Omnibus_Homogeneous=Closed-form power analysis |
| `Direct_TargetEffect_Omnibus_Homogeneous_MC` | text | Target effect |  | Selection_Mode=Select method directly & Direct_Method_Selection=ANOVA (Student) & Direct_Calculation_Method_Omnibus_Homogeneous=Monte Carlo simulation |
| `Direct_MeanTable_Omnibus_Homogeneous_MC` | table | Cell means (table) | Cell (text); Mean (number) | Selection_Mode=Select method directly & Direct_Method_Selection=ANOVA (Student) & Direct_Calculation_Method_Omnibus_Homogeneous=Monte Carlo simulation |
| `Direct_SdModel_Omnibus_Homogeneous_MC` | select | Variance model | cv / sd | Selection_Mode=Select method directly & Direct_Method_Selection=ANOVA (Student) & Direct_Calculation_Method_Omnibus_Homogeneous=Monte Carlo simulation |
| `Direct_Cv_Omnibus_Homogeneous_MC` | number | Coefficient of variation |  | Selection_Mode=Select method directly & Direct_Method_Selection=ANOVA (Student) & Direct_Calculation_Method_Omnibus_Homogeneous=Monte Carlo simulation & Direct_SdModel_Omnibus_Homogeneous_MC=cv |
| `Direct_SdValue_Omnibus_Homogeneous_MC` | number | Standard deviation |  | Selection_Mode=Select method directly & Direct_Method_Selection=ANOVA (Student) & Direct_Calculation_Method_Omnibus_Homogeneous=Monte Carlo simulation & Direct_SdModel_Omnibus_Homogeneous_MC=sd |
| `Direct_SigLevelMcAnova_Omnibus_Homogeneous_MC` | number | Significance level |  | Selection_Mode=Select method directly & Direct_Method_Selection=ANOVA (Student) & Direct_Calculation_Method_Omnibus_Homogeneous=Monte Carlo simulation |
| `Direct_PowerTargetMcAnova_Omnibus_Homogeneous_MC` | number | Desired power |  | Selection_Mode=Select method directly & Direct_Method_Selection=ANOVA (Student) & Direct_Calculation_Method_Omnibus_Homogeneous=Monte Carlo simulation |
| `Direct_NsimAnova_Omnibus_Homogeneous_MC` | number | Simulations per step |  | Selection_Mode=Select method directly & Direct_Method_Selection=ANOVA (Student) & Direct_Calculation_Method_Omnibus_Homogeneous=Monte Carlo simulation |
| `Direct_NMinAnova_Omnibus_Homogeneous_MC` | number | Minimum n per cell |  | Selection_Mode=Select method directly & Direct_Method_Selection=ANOVA (Student) & Direct_Calculation_Method_Omnibus_Homogeneous=Monte Carlo simulation |
| `Direct_NMaxAnova_Omnibus_Homogeneous_MC` | number | Maximum n per cell |  | Selection_Mode=Select method directly & Direct_Method_Selection=ANOVA (Student) & Direct_Calculation_Method_Omnibus_Homogeneous=Monte Carlo simulation |
| `Direct_SeedAnova_Omnibus_Homogeneous_MC` | number | Seed |  | Selection_Mode=Select method directly & Direct_Method_Selection=ANOVA (Student) & Direct_Calculation_Method_Omnibus_Homogeneous=Monte Carlo simulation |
| `Direct_Recommended_Test_Omnibus_Unequal` | text | Welch-ANOVA |  | Selection_Mode=Select method directly & Direct_Method_Selection=Welch-ANOVA |
| `Direct_MeanTableOneWay_Omnibus_Unequal` | table | Level means (table) | Level (text); Mean (number) | Selection_Mode=Select method directly & Direct_Method_Selection=Welch-ANOVA |
| `Direct_SdModelOneWay_Omnibus_Unequal` | select | Variance model | cv / sd | Selection_Mode=Select method directly & Direct_Method_Selection=Welch-ANOVA |
| `Direct_CvOneWay_Omnibus_Unequal` | number | Coefficient of variation |  | Selection_Mode=Select method directly & Direct_Method_Selection=Welch-ANOVA & Direct_SdModelOneWay_Omnibus_Unequal=cv |
| `Direct_SdValueOneWay_Omnibus_Unequal` | number | Standard deviation |  | Selection_Mode=Select method directly & Direct_Method_Selection=Welch-ANOVA & Direct_SdModelOneWay_Omnibus_Unequal=sd |
| `Direct_SigLevelOneWay_Omnibus_Unequal` | number | Significance level |  | Selection_Mode=Select method directly & Direct_Method_Selection=Welch-ANOVA |
| `Direct_PowerTargetOneWay_Omnibus_Unequal` | number | Desired power |  | Selection_Mode=Select method directly & Direct_Method_Selection=Welch-ANOVA |
| `Direct_NsimOneWay_Omnibus_Unequal` | number | Simulations per step |  | Selection_Mode=Select method directly & Direct_Method_Selection=Welch-ANOVA |
| `Direct_NMinOneWay_Omnibus_Unequal` | number | Minimum n per level |  | Selection_Mode=Select method directly & Direct_Method_Selection=Welch-ANOVA |
| `Direct_NMaxOneWay_Omnibus_Unequal` | number | Maximum n per level |  | Selection_Mode=Select method directly & Direct_Method_Selection=Welch-ANOVA |
| `Direct_SeedOneWay_Omnibus_Unequal` | number | Seed |  | Selection_Mode=Select method directly & Direct_Method_Selection=Welch-ANOVA |
| `Direct_Recommended_Test_Pairwise_NonNormal` | text | pairwise Wilcoxon test |  | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise Wilcoxon test |
| `Direct_SelectedPredictors_Pairwise_NonNormal` | text | Predictors |  | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise Wilcoxon test |
| `Direct_NGroups_Pairwise_NonNormal` | number | Number of groups |  | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise Wilcoxon test |
| `Direct_GroupTable_Pairwise_NonNormal` | table | Groups (Name / Mean / SD) | Name (text); Mean (number); SD (number) | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise Wilcoxon test |
| `Direct_Mcc_Pairwise_NonNormal` | select | Multiplicity correction | holm / hochberg / bonferroni / none | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise Wilcoxon test |
| `Direct_Family_Pairwise_NonNormal` | select | Power definition | any / all | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise Wilcoxon test |
| `Direct_AlphaMc_Pairwise_NonNormal` | number | Significance level |  | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise Wilcoxon test |
| `Direct_PowerTargetMc_Pairwise_NonNormal` | number | Desired power |  | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise Wilcoxon test |
| `Direct_Nsim_Pairwise_NonNormal` | number | Simulations per step |  | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise Wilcoxon test |
| `Direct_NMin_Pairwise_NonNormal` | number | Minimum n per group |  | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise Wilcoxon test |
| `Direct_NMax_Pairwise_NonNormal` | number | Maximum n per group |  | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise Wilcoxon test |
| `Direct_Seed_Pairwise_NonNormal` | number | Seed |  | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise Wilcoxon test |
| `Direct_Recommended_Test_Pairwise_Homogeneous` | text | pairwise t-test (Student) |  | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise t-test (Student) |
| `Direct_Method_Header_Pairwise_Homogeneous` | text | Power analysis: t-test (pairwise) |  | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise t-test (Student) |
| `Direct_Calculation_Method_Pairwise_Homogeneous` | select | Calculation Method | Closed-form power analysis / Monte Carlo simulation | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise t-test (Student) |
| `Direct_CohensD_Pairwise_Homogeneous_CF` | number | Effect size (Cohen's d) |  | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise t-test (Student) & Direct_Calculation_Method_Pairwise_Homogeneous=Closed-form power analysis |
| `Direct_SigLevelTTest_Pairwise_Homogeneous_CF` | number | Significance level |  | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise t-test (Student) & Direct_Calculation_Method_Pairwise_Homogeneous=Closed-form power analysis |
| `Direct_DesiredPowerTTest_Pairwise_Homogeneous_CF` | number | Desired power |  | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise t-test (Student) & Direct_Calculation_Method_Pairwise_Homogeneous=Closed-form power analysis |
| `Direct_SelectedPredictors_Pairwise_Homogeneous_MC` | text | Predictors |  | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise t-test (Student) & Direct_Calculation_Method_Pairwise_Homogeneous=Monte Carlo simulation |
| `Direct_NGroups_Pairwise_Homogeneous_MC` | number | Number of groups |  | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise t-test (Student) & Direct_Calculation_Method_Pairwise_Homogeneous=Monte Carlo simulation |
| `Direct_GroupTable_Pairwise_Homogeneous_MC` | table | Groups (Name / Mean / SD) | Name (text); Mean (number); SD (number) | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise t-test (Student) & Direct_Calculation_Method_Pairwise_Homogeneous=Monte Carlo simulation |
| `Direct_Mcc_Pairwise_Homogeneous_MC` | select | Multiplicity correction | holm / hochberg / bonferroni / none | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise t-test (Student) & Direct_Calculation_Method_Pairwise_Homogeneous=Monte Carlo simulation |
| `Direct_Family_Pairwise_Homogeneous_MC` | select | Power definition | any / all | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise t-test (Student) & Direct_Calculation_Method_Pairwise_Homogeneous=Monte Carlo simulation |
| `Direct_AlphaMc_Pairwise_Homogeneous_MC` | number | Significance level |  | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise t-test (Student) & Direct_Calculation_Method_Pairwise_Homogeneous=Monte Carlo simulation |
| `Direct_PowerTargetMc_Pairwise_Homogeneous_MC` | number | Desired power |  | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise t-test (Student) & Direct_Calculation_Method_Pairwise_Homogeneous=Monte Carlo simulation |
| `Direct_Nsim_Pairwise_Homogeneous_MC` | number | Simulations per step |  | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise t-test (Student) & Direct_Calculation_Method_Pairwise_Homogeneous=Monte Carlo simulation |
| `Direct_NMin_Pairwise_Homogeneous_MC` | number | Minimum n per group |  | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise t-test (Student) & Direct_Calculation_Method_Pairwise_Homogeneous=Monte Carlo simulation |
| `Direct_NMax_Pairwise_Homogeneous_MC` | number | Maximum n per group |  | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise t-test (Student) & Direct_Calculation_Method_Pairwise_Homogeneous=Monte Carlo simulation |
| `Direct_Seed_Pairwise_Homogeneous_MC` | number | Seed |  | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise t-test (Student) & Direct_Calculation_Method_Pairwise_Homogeneous=Monte Carlo simulation |
| `Direct_Recommended_Test_Pairwise_Unequal` | text | pairwise Welch t-test |  | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise Welch t-test |
| `Direct_SelectedPredictors_Pairwise_Unequal` | text | Predictors |  | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise Welch t-test |
| `Direct_NGroups_Pairwise_Unequal` | number | Number of groups |  | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise Welch t-test |
| `Direct_GroupTable_Pairwise_Unequal` | table | Groups (Name / Mean / SD) | Name (text); Mean (number); SD (number) | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise Welch t-test |
| `Direct_Mcc_Pairwise_Unequal` | select | Multiplicity correction | holm / hochberg / bonferroni / none | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise Welch t-test |
| `Direct_Family_Pairwise_Unequal` | select | Power definition | any / all | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise Welch t-test |
| `Direct_AlphaMc_Pairwise_Unequal` | number | Significance level |  | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise Welch t-test |
| `Direct_PowerTargetMc_Pairwise_Unequal` | number | Desired power |  | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise Welch t-test |
| `Direct_Nsim_Pairwise_Unequal` | number | Simulations per step |  | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise Welch t-test |
| `Direct_NMin_Pairwise_Unequal` | number | Minimum n per group |  | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise Welch t-test |
| `Direct_NMax_Pairwise_Unequal` | number | Maximum n per group |  | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise Welch t-test |
| `Direct_Seed_Pairwise_Unequal` | number | Seed |  | Selection_Mode=Select method directly & Direct_Method_Selection=pairwise Welch t-test |
| `Direct_Recommended_Test_Paired_Normal` | text | Paired t-test |  | Selection_Mode=Select method directly & Direct_Method_Selection=Paired t-test |
| `Direct_Recommended_Test_Paired_NonNormal` | text | Wilcoxon signed-rank test (paired) |  | Selection_Mode=Select method directly & Direct_Method_Selection=Wilcoxon signed-rank test (paired) |
| `Direct_Method_Header_Paired` | text | Power analysis: paired t-test |  | Selection_Mode=Select method directly & Direct_Method_Selection=Paired t-test |
| `Direct_Calculation_Method_Paired` | select | Calculation Method | Closed-form power analysis / Monte Carlo simulation | Selection_Mode=Select method directly & Direct_Method_Selection=Paired t-test |
| `Direct_CohensDz_Paired_CF` | number | Effect size (Cohen's dz) |  | Selection_Mode=Select method directly & Direct_Method_Selection=Paired t-test & Direct_Calculation_Method_Paired=Closed-form power analysis |
| `Direct_SigLevel_Paired_CF` | number | Significance level |  | Selection_Mode=Select method directly & Direct_Method_Selection=Paired t-test & Direct_Calculation_Method_Paired=Closed-form power analysis |
| `Direct_PowerTarget_Paired_CF` | number | Desired power |  | Selection_Mode=Select method directly & Direct_Method_Selection=Paired t-test & Direct_Calculation_Method_Paired=Closed-form power analysis |
| `Direct_MeanDiff_Paired_MC` | number | Expected mean of differences |  | Selection_Mode=Select method directly & Direct_Method_Selection=Paired t-test & Direct_Calculation_Method_Paired=Monte Carlo simulation |
| `Direct_SdDiff_Paired_MC` | number | SD of differences |  | Selection_Mode=Select method directly & Direct_Method_Selection=Paired t-test & Direct_Calculation_Method_Paired=Monte Carlo simulation |
| `Direct_AlphaMc_Paired_MC` | number | Significance level |  | Selection_Mode=Select method directly & Direct_Method_Selection=Paired t-test & Direct_Calculation_Method_Paired=Monte Carlo simulation |
| `Direct_PowerTargetMc_Paired_MC` | number | Desired power |  | Selection_Mode=Select method directly & Direct_Method_Selection=Paired t-test & Direct_Calculation_Method_Paired=Monte Carlo simulation |
| `Direct_Nsim_Paired_MC` | number | Simulations per step |  | Selection_Mode=Select method directly & Direct_Method_Selection=Paired t-test & Direct_Calculation_Method_Paired=Monte Carlo simulation |
| `Direct_NMin_Paired_MC` | number | Minimum n |  | Selection_Mode=Select method directly & Direct_Method_Selection=Paired t-test & Direct_Calculation_Method_Paired=Monte Carlo simulation |
| `Direct_NMax_Paired_MC` | number | Maximum n |  | Selection_Mode=Select method directly & Direct_Method_Selection=Paired t-test & Direct_Calculation_Method_Paired=Monte Carlo simulation |
| `Direct_Seed_Paired_MC` | number | Seed |  | Selection_Mode=Select method directly & Direct_Method_Selection=Paired t-test & Direct_Calculation_Method_Paired=Monte Carlo simulation |
| `Direct_MedianDiff_Paired_NonNormal` | number | Expected median of differences |  | Selection_Mode=Select method directly & Direct_Method_Selection=Wilcoxon signed-rank test (paired) |
| `Direct_SdDiff_Paired_NonNormal` | number | SD / spread of differences |  | Selection_Mode=Select method directly & Direct_Method_Selection=Wilcoxon signed-rank test (paired) |
| `Direct_AlphaMc_Paired_NonNormal` | number | Significance level |  | Selection_Mode=Select method directly & Direct_Method_Selection=Wilcoxon signed-rank test (paired) |
| `Direct_PowerTargetMc_Paired_NonNormal` | number | Desired power |  | Selection_Mode=Select method directly & Direct_Method_Selection=Wilcoxon signed-rank test (paired) |
| `Direct_Nsim_Paired_NonNormal` | number | Simulations per step |  | Selection_Mode=Select method directly & Direct_Method_Selection=Wilcoxon signed-rank test (paired) |
| `Direct_NMin_Paired_NonNormal` | number | Minimum n |  | Selection_Mode=Select method directly & Direct_Method_Selection=Wilcoxon signed-rank test (paired) |
| `Direct_NMax_Paired_NonNormal` | number | Maximum n |  | Selection_Mode=Select method directly & Direct_Method_Selection=Wilcoxon signed-rank test (paired) |
| `Direct_Seed_Paired_NonNormal` | number | Seed |  | Selection_Mode=Select method directly & Direct_Method_Selection=Wilcoxon signed-rank test (paired) |
| `Direct_Recommended_Test_Repeated_Normal` | text | Repeated-measures ANOVA (RM-ANOVA) |  | Selection_Mode=Select method directly & Direct_Method_Selection=Repeated-measures ANOVA (RM-ANOVA) |
| `Direct_Recommended_Test_Repeated_NonNormal` | text | Friedman test |  | Selection_Mode=Select method directly & Direct_Method_Selection=Friedman test |
| `Direct_Method_Header_Repeated` | text | Power analysis: RM-ANOVA |  | Selection_Mode=Select method directly & Direct_Method_Selection=Repeated-measures ANOVA (RM-ANOVA) |
| `Direct_Calculation_Method_Repeated` | select | Calculation Method | Closed-form power analysis / Monte Carlo simulation | Selection_Mode=Select method directly & Direct_Method_Selection=Repeated-measures ANOVA (RM-ANOVA) |
| `Direct_CohensF_Repeated_CF` | number | Effect size (Cohen's f) |  | Selection_Mode=Select method directly & Direct_Method_Selection=Repeated-measures ANOVA (RM-ANOVA) & Direct_Calculation_Method_Repeated=Closed-form power analysis |
| `Direct_CorrRepeated_CF` | number | Correlation among repeated measures |  | Selection_Mode=Select method directly & Direct_Method_Selection=Repeated-measures ANOVA (RM-ANOVA) & Direct_Calculation_Method_Repeated=Closed-form power analysis |
| `Direct_SigLevel_Repeated_CF` | number | Significance level |  | Selection_Mode=Select method directly & Direct_Method_Selection=Repeated-measures ANOVA (RM-ANOVA) & Direct_Calculation_Method_Repeated=Closed-form power analysis |
| `Direct_PowerTarget_Repeated_CF` | number | Desired power |  | Selection_Mode=Select method directly & Direct_Method_Selection=Repeated-measures ANOVA (RM-ANOVA) & Direct_Calculation_Method_Repeated=Closed-form power analysis |
| `Direct_MeanTable_Repeated_MC` | table | Condition means (table) | Level (text); Mean (number) | Selection_Mode=Select method directly & Direct_Method_Selection=Repeated-measures ANOVA (RM-ANOVA) & Direct_Calculation_Method_Repeated=Monte Carlo simulation |
| `Direct_SdModel_Repeated_MC` | select | Variance model | cv / sd | Selection_Mode=Select method directly & Direct_Method_Selection=Repeated-measures ANOVA (RM-ANOVA) & Direct_Calculation_Method_Repeated=Monte Carlo simulation |
| `Direct_Cv_Repeated_MC` | number | Coefficient of variation |  | Selection_Mode=Select method directly & Direct_Method_Selection=Repeated-measures ANOVA (RM-ANOVA) & Direct_SdModel_Repeated_MC=cv & Direct_Calculation_Method_Repeated=Monte Carlo simulation |
| `Direct_SdValue_Repeated_MC` | number | Standard deviation |  | Selection_Mode=Select method directly & Direct_Method_Selection=Repeated-measures ANOVA (RM-ANOVA) & Direct_SdModel_Repeated_MC=sd & Direct_Calculation_Method_Repeated=Monte Carlo simulation |
| `Direct_CorrRepeated_MC` | number | Correlation among repeated measures |  | Selection_Mode=Select method directly & Direct_Method_Selection=Repeated-measures ANOVA (RM-ANOVA) & Direct_Calculation_Method_Repeated=Monte Carlo simulation |
| `Direct_AlphaMc_Repeated_MC` | number | Significance level |  | Selection_Mode=Select method directly & Direct_Method_Selection=Repeated-measures ANOVA (RM-ANOVA) & Direct_Calculation_Method_Repeated=Monte Carlo simulation |
| `Direct_PowerTargetMc_Repeated_MC` | number | Desired power |  | Selection_Mode=Select method directly & Direct_Method_Selection=Repeated-measures ANOVA (RM-ANOVA) & Direct_Calculation_Method_Repeated=Monte Carlo simulation |
| `Direct_Nsim_Repeated_MC` | number | Simulations per step |  | Selection_Mode=Select method directly & Direct_Method_Selection=Repeated-measures ANOVA (RM-ANOVA) & Direct_Calculation_Method_Repeated=Monte Carlo simulation |
| `Direct_NMin_Repeated_MC` | number | Minimum n |  | Selection_Mode=Select method directly & Direct_Method_Selection=Repeated-measures ANOVA (RM-ANOVA) & Direct_Calculation_Method_Repeated=Monte Carlo simulation |
| `Direct_NMax_Repeated_MC` | number | Maximum n |  | Selection_Mode=Select method directly & Direct_Method_Selection=Repeated-measures ANOVA (RM-ANOVA) & Direct_Calculation_Method_Repeated=Monte Carlo simulation |
| `Direct_Seed_Repeated_MC` | number | Seed |  | Selection_Mode=Select method directly & Direct_Method_Selection=Repeated-measures ANOVA (RM-ANOVA) & Direct_Calculation_Method_Repeated=Monte Carlo simulation |
| `Direct_MeanTable_Repeated_NonNormal` | table | Condition means/medians (table) | Level (text); Mean (number) | Selection_Mode=Select method directly & Direct_Method_Selection=Friedman test |
| `Direct_SdModel_Repeated_NonNormal` | select | Variance model | cv / sd | Selection_Mode=Select method directly & Direct_Method_Selection=Friedman test |
| `Direct_Cv_Repeated_NonNormal` | number | Coefficient of variation |  | Selection_Mode=Select method directly & Direct_Method_Selection=Friedman test & Direct_SdModel_Repeated_NonNormal=cv |
| `Direct_SdValue_Repeated_NonNormal` | number | Standard deviation |  | Selection_Mode=Select method directly & Direct_Method_Selection=Friedman test & Direct_SdModel_Repeated_NonNormal=sd |
| `Direct_CorrRepeated_NonNormal` | number | Correlation among repeated measures |  | Selection_Mode=Select method directly & Direct_Method_Selection=Friedman test |
| `Direct_AlphaMc_Repeated_NonNormal` | number | Significance level |  | Selection_Mode=Select method directly & Direct_Method_Selection=Friedman test |
| `Direct_PowerTargetMc_Repeated_NonNormal` | number | Desired power |  | Selection_Mode=Select method directly & Direct_Method_Selection=Friedman test |
| `Direct_Nsim_Repeated_NonNormal` | number | Simulations per step |  | Selection_Mode=Select method directly & Direct_Method_Selection=Friedman test |
| `Direct_NMin_Repeated_NonNormal` | number | Minimum n |  | Selection_Mode=Select method directly & Direct_Method_Selection=Friedman test |
| `Direct_NMax_Repeated_NonNormal` | number | Maximum n |  | Selection_Mode=Select method directly & Direct_Method_Selection=Friedman test |
| `Direct_Seed_Repeated_NonNormal` | number | Seed |  | Selection_Mode=Select method directly & Direct_Method_Selection=Friedman test |

## Inclusion and Exclusion Criteria  (`inclusion_criteria`)

| field | type | label | options / columns | shown if |
|---|---|---|---|---|
| `Introduction_Incl_Excl` | text | For more Information on Inclusion and Exclusion Criteria please refer to:  |  |  |
| `Inclusion_Criteria` | table | Define the Inclusion Criteria applied in the Study. | Criteria (text); Description (text); Value (text); Unit (text) |  |
| `Exclusion_Criteria` | table | Define the Exclusion Criteria applied in the Study. | Criteria (text); Description (text); Value (text); Unit (text) |  |

## Randomization  (`randomization`)

| field | type | label | options / columns | shown if |
|---|---|---|---|---|
| `Introduction_Randomization` | text | For more Information on Randomization please refer to: |  |  |
| `Randomization_applied` | checkbox | Randomization will be applied in the study |  |  |

## Blinding  (`blinding`)

| field | type | label | options / columns | shown if |
|---|---|---|---|---|
| `Introduction_Blinding` | text | For more Information on Blinding please refer to: |  |  |
| `Blinding_applied` | checkbox | Blinding will be applied in the study |  |  |

# Select options (key → label, ⚠ = differ)

- **sex**: Male · Female · Unknown/Undetermined · Hermaphrodite
- **role**: ⚠ `Principal Investigator` → Project Leader · ⚠ `Deputy Investigator` → Deputy Project Leader · ⚠ `Study Designer(s)` → Study Planner · ⚠ `Procedure Personnel` → Procedure / Euthanasia Personnel · Anesthesia Personnel · ⚠ `Training_Supervisor` → Training Supervisor · Trainee · ⚠ `Animal_Health_Care_Responsible` → Animal Health Care Responsible · ⚠ `Post_Study_Assessor` → Post-Study Assessor · ⚠ `Animal_Welfare_Officer` → Animal Welfare Officer
- **databases**: DB-ALM (Database on Alternative Methods) · NORINA · ICCVAM / NICEATM · PubMed · Embase · Web of Science · Scopus · Google Scholar · SMAFIRA · NAT Database · TSAR
- **mcc_select**: holm · hochberg · bonferroni · none
- **time_stamp**: Automatic system timestamp · Manual timestamp entry · ⚠ `Instrument-generated timestamp` → Instrument timestamp · File metadata timestamp · ⚠ `Audit trail with versioning` → Audit trail versioning · ⚠ `No formal timestamping procedure` → No formal timestamping
- **prereg_type**: Study Protocol · Systematic Review · ⚠ `Report` → Registered Report
- **burden_level**: None · Mild · Moderate · Severe
- **control_type**: ⚠ `Positive control` → Positive Control · ⚠ `Negativ control` → Negative control · Sham · Genetic Control
- **outcome_role**: Primary Outcome · Secondary Outcome · Exploratory Outcome
- **outcome_type**: Categorial · Ordinal · Continuous
- **routes_admin**: Oral · Intraveneous (i.v.) · Intraperitoneal (i.p.) · Subcutaneous (s.c.) · Intramuscular (i.m.) · Intradermal · Topical/dermal · Inhalation · Intranasal · Intrathecal · Rectal · Intraocular · Implantation · Other
- **family_select**: any · all
- **qualification**: Veterinarian (licensed) · Specialist Veterinarian – Laboratory Animal Science · Certified Laboratory Animal Technician · Veterinary Technical Assistant · Biology Laboratory Technician · PhD / Doctoral Researcher · MSc / Academic Degree · Animal Welfare Officer · Other · None
- **predictor_role**: Primary Factor · Secondary Factor · Covariate · Confounder · Blocking variable
- **predictor_type**: Categorial · Ordinal · Continuous
- **substance_type**: ⚠ `drug` → Drug · Anaesthetic · Analgesic · Vehicle · Cell Line · Other
- **model_term_type**: Fixed Effect · Random Effect · unknown · none
- **sd_model_select**: ⚠ `cv` → cv (proportional) · ⚠ `sd` → sd (constant)
- **data_format_type**: Open · Proprietary · Open standard · Partially open
- **quality_assurance**: SOP compliance · Duplicate measurements · Calibration of instruments · Controls included · Data validation rules · Peer-Review · Statistical quality control · No formal QA measures
- **calc_method_select**: Closed-form power analysis · Monte Carlo simulation
- **regulatory_function**: Function A – Procedures · Function B – Design · Function C – Animal Care · Function D – Euthanasia · Supervisor · Other · None
- **direct_method_select**: One-sample t-test · One-sample Wilcoxon signed-rank test · Wilcoxon rank-sum test · t-test (Student) · Welch t-test · Kruskal-Wallis test · ANOVA (Student) · Welch-ANOVA · pairwise Wilcoxon test · pairwise t-test (Student) · pairwise Welch t-test · Paired t-test · Wilcoxon signed-rank test (paired) · Repeated-measures ANOVA (RM-ANOVA) · Friedman test
- **selection_mode_select**: External tool / guidance · Select method directly
- **preregistration_select**: ⚠ `Preclinicaltrials.eu` → preclinicaltrials.eu · ⚠ `www.animalstudyregistry.org` → animalstudyregistry.org · OSF.io · PROSPERO · aspredicted.org · Center for Open Science
- **risk_descriptor_mentioned**: yes · no
