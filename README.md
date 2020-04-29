---
title: "Postdoc project 1.1.1.2/VIAA/1/16/112"
author: "Dmitry Pavlyuk"
date: "Feb 6 2019 - Jan 7, 2020"
output: html_document
---

This repository supports researches executed within the scope of Dmitry Pavlyuk's postdoctoral research project No. 1.1.1.2/VIAA/1/16/112 "Spatiotemporal urban traffic modelling using big data"

The main purpose of the repository is to ensure robust and reproducable research results.


Data description
----------------
Mainly in this project we use data in ADUS (.traffic) format. The Archived Data User Service (ADUS) provides the U.S. National ITS Architecture with the requirements for archiving and re-use of data collected for ITS operations. Several popular research data providers utilise ADUS, e.g. California Freeway Performance Measurement Project (PeMS), Minnesota - Traffic Management Center, etc.


Main markdowns
----------------
A usual study includes following data processing steps:

1. Data collection. We assume that data is publicly available and can be downloaded (like ADUS data).   
See [data collection markdown](./data_collection.Rmd) for download routines.
2. Data transformation. Collected data are trnasformed into convenietn format.
See [data transformation markdown](./data_transformation.Rmd) for transformation routines.
3. Data sampling and cleaning. See [data sampling and cleaning markdown](./sampling.Rmd) for transformation routines.
4. Conference- or article-specific markdowns
    + 6th International Conference on Models and Technologies for Intelligent Transportation Systems - MT-ITS 2019 and further extension to the journal article in Algoritms [markdown](./MTITS2019.Rmd) https://www.mt-its2019.pk.edu.pl
    + 22nd Euro Working Group on Transportation Meeting - EWGT 2019 [markdown](./EWGT2019.Rmd) https://ewgt19.upc.edu/en
    + 6th International conference on Time Series and Forecasting - ITISE 2019 [markdown](./ITISE2019.Rmd) http://itise.ugr.es/
    + 19th International Multi-Conference Reliability and Statistics in Transportation and Communication - RelStat-2019 [markdown](./RelStat2019.Rmd) http://relstat2019.tsi.lv/
    + "Robust Learning of Spatiotemporal Urban Traffic Flow Relationships" prepared for submission to Transportation Research: Part C [markdown](./TRC2020.Rmd) http://relstat2019.tsi.lv/
