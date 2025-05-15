# Socio-demographic variation in the association between modes of access and patient experience of primary care

## Description
This repository includes code used to run the The Health Foundation Improvement Analytics Unit's analysis of socio-demographic variation in the association between modes of access and patient experience of primary care: a retrospective cross-sectional patient-level analysis of the General Practice Patient Survey in England in 2023.

## Data sources

Data comes from:
- [General Practice Patient Survey (GPPS) 2023](https://www.england.nhs.uk/statistics/2023/07/13/gp-patient-survey-2023/)
- [NHS Digital: Patients Registered at a GP Practice](https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice)
- [NHS Digital: General practice workforce 2023](https://digital.nhs.uk/data-and-information/publications/statistical/general-and-personal-medical-services) 
- [Department for Environment, Food and Rural Affairs: Rural urban classification](https://www.gov.uk/government/collections/rural-urban-classification)
- [Care Quality Commission: GP Ratings](https://www.cqc.org.uk/guidance-providers/gps/levels-ratings-gp-practices)
- [Office for National Statistics: English indices of deprivation](https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019) 

## Requirements

These scripts were written in R version 4.0.2 and RStudio Workbench Version 1.1.383. Analyses were completed using the [lme4 package](https://cran.r-project.org/web/packages/lme4/lme4.pdf) version 1.1-35.1. 

## How to use this repo

Scripts should be run in order as numbered. GPPS data are not publicly available so must be accessed via [application](https://www.gp-patient.co.uk/contact). 

## Contributors

Authors: Tatjana Marks, Xiaochen Ge, Sarah-Opie Martin, Rebecca Xu, Geraldine Clarke

The Improvement Analytics Unit (IAU) is a partnership between NHS England and the Health Foundation. The research was undertaken by the IAU and jointly funded by the Health Foundation and NHS England. The HES data are used with permission from NHS England who retain the copyright for those data. The authors thank the Surgical Hubs team at Getting It Right First Time (NHS England) for providing advice on the context and interpretation of results, and colleagues in the IAU (the Health Foundation) for data management and preparation.

## License

This project is licensed under the [MIT License](https://opensource.org/license/mit/).
