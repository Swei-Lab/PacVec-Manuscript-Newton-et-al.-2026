# PacVec Manuscript - Newton et al. 2026
Abstract

Rodents function as important reservoir hosts for many infectious disease agents in North America, particularly contributing to enzootic cycles of the Lyme disease pathogen, Borrelia burgdorferi sensu stricto (s.s.). Rodent reservoir-targeted acaricide treatments aim to disrupt pathogen transmission between ticks and rodents to reduce human tick-borne disease risk. In the western US, the efficacy of rodent-targeted topical acaricides to reduce B. burgdorferi s.s. infection prevalence in the primary tick vector, the western blacklegged tick (Ixodes pacificus), has been understudied. We deployed rodent-targeted topical acaricides in Northern California during peak larval and nymphal I. pacificus host-seeking activity from March - May in 2023 and 2024, measuring B. burgdorferi s.s. infection prevalence in host-seeking I. pacificus nymphs and rodent reservoir host species at three paired treatment and control plots. Sampling began 20 days after acaricide bait station deployment each year. A permethrin-based topical acaricide was tested at a one plot and a deltamethrin-based acaricide at two plots. Mean rodent reservoir host infection prevalence declined from 10.3% in 2023 to 6.9% in 2024 post-treatment, though this reduction was not significant. However, I. pacificus nymphal infection prevalence was significantly reduced in the permethrin-based treatment plot but significantly increased in deltamethrin-based treatment plots in 2024. Rodent abundance and tick re-acquisition rates on recaptured rodents also differed significantly between treatment plots. These findings suggest that permethrin and deltamethrin-based acaricides may differ in their effectiveness at preventing tick-borne pathogen transmission and that ecological factors, including rodent host composition and species-specific efficacy, may influence treatment outcomes.

Description of data and file structure - 
  "Newton_Manuscript_2_Data" R script in .R format to be used in R or RStudio. Script includes all necessary packages and instructions to set up analysis of questing nymphal I. pacificus infection prevalence, density of infected nymhal I. pacificus, nymphal I. pacificus infection status, small mammal abundance, small mammal infection prevalence, and recaptured mammal tick reacquisition rate.

Source Code:
  "Newton_Manuscript_2_Data"

Packages used - 
  "ggplot2", "ggpattern", "glmmTMB", "logistf", "dplyr", "datasets", "tidyr"

NIP_BINOM.xlsx
  Headers -
    Site: Name of park, regional open space, or general location where data was collected
    County: Name of county where data was collected 
    Plot: Name of individual plot where data was collected from. Name of Plot is Area name followed by treatment type abbreviated. Secondary 
    control plots are followed by a 2. ex. San Pedro Acaricide (SPA), San Pedro Control (SPC), San Pedro secondary control (SPC2)
    Treatment: Describes if animal was captured from acaricide or control plot
    Infection: Describes if animal tested positive for B.b.s.s.
    Year: Year animal was captured 

DIN_BINOM.xlsx
  Headers -
    Site: Name of park, regional open space, or general location where data was collected
    County: Name of county where data was collected 
    Plot: Name of individual plot where data was collected from. Name of Plot is Area name followed by treatment type abbreviated. Secondary 
    control plots are followed by a 2. ex. San Pedro Acaricide (SPA), San Pedro Control (SPC), San Pedro secondary control (SPC2)
    Treatment: Describes if animal was captured from acaricide or control plot
    Infection: Describes if animal tested positive for B.b.s.s.
    Year: Year animal was captured 

MIP_BINOM.xlsx
  Headers -
    Site: Name of park, regional open space, or general location where data was collected
    County: Name of county where data was collected 
    Plot: Name of individual plot where data was collected from. Name of Plot is Area name followed by treatment type abbreviated. Secondary 
    control plots are followed by a 2. ex. San Pedro Acaricide (SPA), San Pedro Control (SPC), San Pedro secondary control (SPC2)
    Treatment: Describes if animal was captured from acaricide or control plot
    Infection: Describes if animal tested positive for B.b.s.s.
    Year: Year animal was captured 
    Species: Abbreviation of animal species scientific name ex. Neotoma fuscipes (NEFU)

MIP_BINOM.xlsx
  Headers -
    Site: Name of park, regional open space, or general location where data was collected
    County: Name of county where data was collected 
    Plot: Name of individual plot where data was collected from. Name of Plot is Area name followed by treatment type abbreviated. Secondary 
    control plots are followed by a 2. ex. San Pedro Acaricide (SPA), San Pedro Control (SPC), San Pedro secondary control (SPC2)
    Treatment: Describes if animal was captured from acaricide or control plot
    Infection: Describes if animal tested positive for B.b.s.s.
    Year: Year animal was captured 
    Species: Abbreviation of animal species scientific name ex. Neotoma fuscipes (NEFU)

Recaptured_Mammal_Burdens.xlsx
  Headers -
    Plot: Name of individual plot where data was collected from. Name of Plot is Area name followed by treatment type abbreviated. Secondary 
    control plots are followed by a 2. ex. San Pedro Acaricide (SPA), San Pedro Control (SPC), San Pedro secondary control (SPC2)
    County: Name of county where data was collected 
    ID: Unique identifier for captured animal 
    Species: Abbreviation of animal species scientific name ex. Neotoma fuscipes (NEFU)
    Burden: Number of larval Ixodes pacificus tick burdens on captured animal 
    Rate: Number of larval Ixodes pacificus on captured animal divided by the days since previous capture
    Treatment: Describes if animal was captured from acaricide or control plot
    Year: Year animal was captured 
  
Code/Software - 
All analyses were run in R v.4.3.1 and implemented the packages "ggplot", "ggpattern", "glmmTMB", "logistf", "dplyr", "datasets", and "tidyr" 
to generate figures and to evaluate treatment effect on questing nymphal I. pacificus infection prevalence, density of infected nymhal I. 
pacificus, nymphal I. pacificus infection status, small mammal abundance, small mammal infection prevalence, and recaptured mammal tick 
reacquisition rate.
