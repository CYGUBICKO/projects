PROVIEW: Research Proposal Summary
----------------------------------------------------------------------
2019 May 07 (Tue)

----------------------------------------------------------------------

* Main object: Developing a Tool to Support Ealier Palliative Care
	* The focus is in Gastrointestinal (GI) cancer 
		- Pancrease
		- Stomach
		- Liver
		- Bladder
		- Bowel
	* Past studies demonstrated benefits of palliative care interventions in comparison to usual care. It consists of:
		- Timely access to symptoms management
		- Psychosocial support
		- Care coordination
	* Some of the benefits include:
		- Improved quality of life (QoL)
		- Reduced symptom burden
		- Fewer hospitalizations
		- Reduce health care costs
	* Early integration (at diagnosis) with active cancer treatment (oncological care) is recommended
	* Previous studies have showed late introduction, little or no use of palliative care. Why?
		- Increased treatment options
		- Palliative is viewed as a way to take away patient's hopes
	* Prognostic tools to predict death have been developed to help encologists discuss palliative care:
		- Biological factors
		- Laboratory factors
			- Antigen levels
			- Elevated C-reactive protein
			- Leukocytosis
	* Need to develop a web-based prognostic cancer tools for cancer
		- 22 online tools
			- 89 different cancers
				- 5 GI cancers
		- The tools have failed to integrate palliative care earlier in the disease trajectory
			- Oncologist avoid discussing prognosis with advanced cancer patients
			- Mostly focus on predicting death (no QoL as one of the outcomes)
			- Do not include data on palliative care interventions
	* Some of the palliative care considered:
		- Physician home visits
		- Home care services
		- Caregiver support

* Specific Objectives
	- Derive and validate statistical models to predict
		- Risk of death
		- Level of pain
		- Pefromance status
	- Co-develop a web-based tool 

* Method
	* Study design
		- Population-based, retrospective study
		- Adults diagnosed with stage IV GI cancer
		- Study period: 2008-2015
	* Data sources
		- From Ontario Cance Registry datatabases
	* Index dates
		- Death outcome, initial index date is the date of diagnosis
		- **To discuss part 3.1.4 on predictive survival models**
		- Random index date for other outcomes
	* Outcomes
		- Death
		- Moderate to severe pain
		- Low performance status within six months
	* Covariates
		- Demographic
			* Age at index date
			* Sex
			* Income quintile
			* Region
			* Rurality
		- Clinical and prior anti-cancer treatment
			* Diagnosis date
			* Cancer stage
			* Cancer type
			* Deyo-Charlson comorbidity score
			* Prior chemotherapy
			* Radiation treatment
			* Prior cancer surgery
		- Health utilization in the last one year
			* Hospitalizations
			* ED visits
			* Pysician claims
			* Narcotics
		- QoL in the last year
			- Controlling for palliative for the last 3 months
		- End-of-life home care services
		- Caregiver support 
		- Physiscian home visits

	* Analysis plan
		- We can go through it together
			- Multivariate Cox regression to predict instantaneous risk of death as a function of time

