---
title: "suggestions.md"
format: html
---

as of 23 APR 2026

DRAFT 

## PURPOSE
The purpose of **suggestions.md** is to collect suggestions, ideas, snippets of code that might improve or extend the vignette "What is aNCA?" in the file **background.rmd**.  **suggestions.md** exisits only on branch [#1247](https://github.com/pharmaverse/aNCA/issues/1247) in the directory **inst/** and will be removed before final merge to main.

Specifically, @Gero1999 (or anyone else) is encouraged to comment or review **suggestions.md** before changes are made to the actual vignette 

@jimrothstein (jim) will keep **suggestions.md** up-to-date by including the various comments from others.  The idea is to come to  general agreement **before** making major changes to vignette.  Of course, nothing is ever final but hopefully is approach will make process as simple as possible.

## AUDIENCE?
Is document for  a new developer?   For the user of the package **aNCA** ?   Someone else?

I would like to write so useful to someone like me.   Familar with R, but new to pharmacokinetics (PK) , CDISC standards and the functions of aNCA package.  This person wants  to contribute to the codebase, ie a new developer.

## Not a Book (LOL) :smile:
"What is NCA?" should be as short as possible, and definitely not lengthy.  Instead, the vignette should offer links where the reader can find more detailed information.  

It seems the entire clinical trials industry is filled with dataset words that I find difficult to follow when learning the code base.  The basic ideas of concentration, time, doses do not seem difficult.   So I propose to have many simple examples as possible so the reader gains confidence.  

Please see LLM chat (below)

## NEXT?

## Big List for Discussion ...
Possible inclusions?
- link to official CDISC documentation (https://www.cdisc.org/system/files/members/standard/foundational/ADaMIG_for_Non-compartmental_Analysis_Input_Data_v1.0_1.pdf)
- ADNCA format ( explain,  show trival example, ~ 5 rows, example or two in codebase)
- (related?) link to `admiral` https://pharmaverse.github.io/admiral/articles/pk_adnca.html
- edit chat with Gemini (below) 
- much, much more

## Initial Chat with Gemini LLM (unedited, if reader wants "more information link to list)
 Gemini
**ADNCA Format for NCA Analysis**
Conversation with Gemini

**Please explain ADNCA format in non-compartmental analysis (NCA). Give simiple example of a dataset in ADNCA format. No more than 5 rows.**

In the world of clinical data standards, specifically CDISC ADaM, the ADNCA (Analysis Data for Non-Compartmental Analysis) dataset serves as the bridge between raw concentration-time data and the final calculated PK parameters (like Cmax​ or AUC).

Its primary purpose is to organize clinical trial data into a structure that NCA software (like WinNonlin or R packages like nca) can ingest to perform calculations.
Key Characteristics of ADNCA

    One Record per Timepoint: Each row represents a single blood draw or observation for a specific subject at a specific time.

    Traceability: It maintains a clear link back to the source SDTM datasets (usually PC for Pharmacokinetics Concentrations and EX for Exposure).

    Ready for Calculation: It includes "Analysis" variables (prefixed with A) that have been cleaned—for example, handling values below the limit of quantification (BLQ) or converting actual sampling times into relative time from dose.

Simple Example: ADNCA Dataset

Below is a simplified example showing a single subject receiving a single dose.
USUBJID	EXTRT	ATIME	AVAL	AVALU	AVALCAT1
001-01	Drug X	0	0	ng/mL	BLQ
001-01	Drug X	0.5	12.5	ng/mL	Analyzed
001-01	Drug X	1.0	25.2	ng/mL	Analyzed
001-01	Drug X	2.0	18.7	ng/mL	Analyzed
001-01	Drug X	4.0	8.3	ng/mL	Analyzed
Core Variables Explained

    USUBJID: Unique Subject Identifier.

    EXTRT: The treatment/drug name.

    ATIME (Analysis Time): The relative time from the dose (e.g., hours). This is crucial for calculating the area under the curve (AUC).

    AVAL (Analysis Value): The numerical concentration result used for the math.

    AVALU: The unit for the concentration (e.g., ng/mL).

    AVALCAT1: Often used to flag values like "BLQ" (Below Limit of Quantitation), which tells the software whether to treat the value as zero or missing.

**Where did you find "ATIME", "AVAL" as core variables and their purpose?**

The definitions for AVAL and ATIME come directly from the CDISC ADaM (Analysis Data Model) standards. Specifically, they are part of the Basic Data Structure (BDS) and the ADaM Implementation Guide for Non-compartmental Analysis (ADNCA).
1. AVAL (Analysis Value)

This is a core variable in the BDS class, which is the foundation for almost all analysis datasets.

    Source: CDISC ADaM Implementation Guide (ADaMIG).

    Purpose: It is the primary numeric variable used for statistical analysis. In the context of ADNCA, AVAL holds the drug concentration value.

    Why it's used: By standardizing the results into AVAL, programmers and regulatory reviewers (like the FDA) can run automated scripts for calculations or validation (e.g., using Pinnacle 21) without needing to know if the original raw variable was named PCORRES or CONC.

2. ATIME (Analysis Time)

While "Time" variables in SDTM (raw data) are often ISO 8601 strings (like 2026-04-22T14:30), NCA requires numeric time for calculations.

    Source: ADaM Implementation Guide for Non-compartmental Analysis (ADNCA) v1.0.

    Purpose: It represents the Analysis Time, typically the actual time elapsed since the dose was administered.

    Why it's used: You cannot calculate an AUC (Area Under the Curve) using a clock timestamp; you need a numeric value (e.g., 0.5 hours, 1.0 hours) to serve as the x-axis for the trapezoidal rule.

**Where to Find the Official Documentation**

If you are building these datasets in your R environment (using packages like admiral or admiralneuro), these definitions are pulled from:

    CDISC ADaMIG for NCA: The specialized supplement that defines how to bridge SDTM PC (Concentrations) and EX (Exposure) into an NCA-ready format.

    The pharmaverse admiral documentation: Specifically the NCA vignette, which maps these CDISC variables to R functions.

Review of proposed NCA ADAM standard

This video provides an industry-level overview of the ADNCA standard and how it aligns with regulatory expectations for pharmacokinetic data.
Review of proposed NCA ADAM standard - YouTube
International Society of Pharmacometrics (ISOP) · 403 views

**Is there a pdf file or url list these and their definitions?**

Yes, there are several official and industry-standard resources where you can find these definitions. Depending on whether you want the formal regulatory standard or a practical "how-to" guide for R, you can use the following:
1. The Official CDISC ADNCA Standard

The formal source is the ADaM Implementation Guide for Non-compartmental Analysis Input Data (ADNCA) v1.0.

    Access: You can find the landing page and document link on the CDISC ADaM Foundational Standards page.

    What it contains: It includes the full metadata tables for every variable permitted in an ADNCA dataset, including AVAL, ATIME, and others like NFRLT (Nominal Relative Time from Last Dose).

    Format: It is provided as a PDF (once you sign in to a free CDISC account).

2. Pharmaverse admiral Documentation (Best for R Users)

If you are working in R, the admiral package (which is part of the pharmaverse) provides a comprehensive mapping of these variables with clear, developer-friendly definitions.

    URL: Creating a PK ADNCA Dataset (admiral)

    Why it's useful: It explains how to derive ATIME from raw SDTM date/times and maps it directly to the CDISC standard. It also lists "Required Variables" versus "Permissible Variables."

3. CDISC Knowledge Base (Quick Reference)

The CDISC Knowledge Base has a specific page for the Basic Data Structure (BDS), which defines the core variables used in ADNCA (since ADNCA is a type of BDS dataset).

    URL: ADaM Basic Data Structure (BDS) Example

    Table Content: This link provides a table similar to the one below, detailing the Variable Name, Label, and Source/Derivation.

Variable Definition Summary

If you just need a quick cheat sheet for the variables you mentioned:
Variable	Label	Definition / Purpose
AVAL	Analysis Value	The numeric result (concentration) used for the analysis.
ATIME	Analysis Time	The elapsed time since dose (e.g., in hours) used as the x-axis for NCA.
PARAM	Parameter	The descriptive name (e.g., "Drug X Concentration").
PARAMCD	Parameter Code	The short code for the parameter (e.g., "CONC").
ADTM	Analysis Date/Time	The actual clock date and time of the sample (ISO 8601).
