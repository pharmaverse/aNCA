---
title: "suggestions.md to improve background.md "
format: html
---

as of 25 APR 2026

 DRAFT 

## Vignettes 

* background.Rmd (https://github.com/pharmaverse/aNCA/blob/main/vignettes/background.Rmd)
* manual.Rmd (https://github.com/pharmaverse/aNCA/blob/main/vignettes/manual.Rmd)


## Contents

* Objective
* Audience?
* Set goals

## OBJECTIVE
The goal is improve the document "What is NCA?" (background.md).   Other than a few minor suggestions, I think document is already very good.

However, there is a fairly large gap between "What is NCA?" and the next
vignette, "Manual" .  So I want to suggest supplementing "What is NCA?" or
adding a new document before manual.Rmd

In particular, I would include:

* few videos, to introducing NCA and introduce the implementation with PKNCA and aNCA packages
   * Appilson, 5/29/25,   (https://www.youtube.com/watch?v=hvVS80I6s_U) (reduce playback speed to 0.8 if accents are new to you)
   * Basic pharmakinetics 1 Paths of drug through body (https://www.youtube.com/watch?v=WCmFrheYtcc ) 
   * Basic pharmakinetics 2 - How choice of route affects NCA parameters https://www.youtube.com/watch?v=WCmFrheYtcc 
* a diagram or two to illustrate the flow of data  
  * input -> create objects -> run calculations
  * video (2016) - Bill P introduces PKNCA package & process  (https://www.youtube.com/watch?v=WCmFrheYtcc)
  * PKNCA architecture https://github.com/humanpred/pknca/blob/ee1b433a0efcf102f4ec5260a1e3b39803ba26b2/design/01-architecture.md

* introduce some of subleties NCA must accommodate :  
  * possible time measurements, 
  * possible NCA parmaters, exclusions...
* where to find all the allowable variables and meaning (CDISC document)

## AUDIENCE?

Is this document for:

* a new developer?   
* a new user of the package **aNCA** ?   

What assumptions should we make about the reader?

* Knows R?
* Knows NCA?
* Knows CDISC?

I would like to write so useful to someone like me.   Familar with R, but new to pharmacokinetics (PK) , CDISC standards and the functions of aNCA package.  This person wants  to contribute to the codebase, ie a new developer.
