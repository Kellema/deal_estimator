---
Title: "Propensity Score Estimation (PSM) for DEAL Data"
output:
  pdf_document:
    toc: true
  html_document:
    theme:
      version: 4
    toc: true
    number_sections: true
    toc_float:
      collapsed: false
  word_document:
    toc: true
---
# Objectives
The joint Monitoring, Evaluation, Accountability and Learning (MEAL) and Data In Emergency (DIEM) impact assessment methodology (DEAL) introduces a robust, harmonized framework for evaluating the outcomes, impacts, and economic value of emergency and resilience-focused agricultural interventions.
This project leverages R and Shiny to deliver a user-friendly web application designed for conducting propensity score matching on DEAL datasets. It makes use of MatchIt package for the PSM calculations. 

# Requirements
- To successfully run the Shiny application, the following criteria must be met:
- Input data should be in **CSV, RDS, or DTA** format.
- Data should be cleaned for duplicate observations, outliers, miscoded values, and missing values. Any missing values will be automatically removed prior to matching.
- Factor variables must be appropriately defined within **CSV and RDS** files.
- Treatment variables must be binary and coded as 0 (non-treated) and 1 (treated).
- Large files **(>10 MB)** are not supported; users should consider pre-filtering and/or compressing files.

# Key Operating Instructions

##	Data Import:
- Import the cleaned DEAL dataset.
- Proceed by selecting **“Next”** or **“Set Variables”** from the left panel.
  
##	Variable Setting:
- Select the outcome variable using the drop-down menu in the **“Outcome variable”** field. Outcome variables may be binary, continuous, or discrete.
- Select the treatment variable in the **“Treatment variable”** field, ensuring compliance with the aforementioned requirements.
- Choose the covariates from the available list. For each selected variable, the application performs a Welch Two Sample test to assess balance between treated and non-treated groups.
- Click **“Next”** or **“Run matching”** in the side panel.
  
##	Matching Configuration:
- Specify the matching method: Nearest Neighbor (default), Optimal, or Full.
- Set the controls-to-treated ratio; the default is 1:1.
- Precise the distance metric to be applied between the **Logit, the GAM and the Gradient Boosting**.
- Indicate whether matching with replacement should be performed; the default is "No".
- Initiate matching by clicking **“Run the matching”** in the Matching Configuration group on the main tab.
- Click “Next” or **“Run matching”** in the side panel to proceed.
  
##	Visual and Numerical Checks Post-Matching:
- Select covariates for review.
- Define the number of plots per row.
- Specify the plot height in pixels.
- Proceed by clicking “Next” or **“Treatment Effect”** in the side panel.
  
##	Treatment Effect Estimates Visualization

# AI Use Statement
This content was developed by a human (Assad Bori) utilizing AI tools such as Copilot and Claude to facilitate brainstorming, refine ideas, and tailor communications for diverse audiences and contexts. All AI usage followed a human-in-the-loop approach, ensuring human oversight and final decision-making throughout every stage.
