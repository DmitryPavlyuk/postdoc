---
title: "Postdoc project 1.1.1.2/VIAA/1/16/112"
author: "Dmitry Pavlyuk"
date: "February 6, 2019"
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
2. Data transformation
3. Data cleaning
4. Conference- or article-specific markdowns
    + [EWGT-2019]
