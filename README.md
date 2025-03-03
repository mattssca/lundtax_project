# LundTax Manuscript

This repo holds everything related to the upcoming bioinformatics publications on the LundTax2023 R package.

## Outline

This is a rough outline that will eb expanded upon as the project progresses. Each header is annotating a different section in the draft paper. 

### Preprocessing

Look into "all" available prepproccessing methods relating to RNA sequencing data. Possibly, check Elenas work for leads on where to look. One key point is to take notes of how the different preprocessing methods are different in terms of the returned data.

In this study, we have included the three datasets TCGA, UC Genome and Leeds. 

#### TCGA (preprocessed)

We have implemented our algorithm on data preprocessed as described in this study (Salmon), refer to Elenas work showing that preprocessing doesn not matter much (Kallisto vs. Salmon)

#### UC Genome (raw)

Original data obtained from the paper (raw)

#### Leeds (Micro array)

Array data from this publication

### Classification

Describe how our random forest operates and the main principles. We can refer to Pontus publication. Important to state that all subtypes get their own prediction values. By default the algorithm predicts classification based on the five subtype system (Uro, GU, BaSq, Mes, ScNE). Smaples that are being classified as Uro are further subtyped into UroA, UroB, UroC.

### Robustness

The main point here is to look into if the delta values are relted to the quality of the incoming data or more driven by the specifc biology of the tumnors.

1. Generate a function that takes the RNA seq data and returns a point plot over library sizes for each sample.
2. Make the delta-graph seperatly for each dataset. The delta should be the highest prediction value for a subtype, compared to the next highest value, for another subtype.
3. Redo figures but in subtype dependent manner (5 Class).
4. Repeat for 7 Class subtypes.

### TODO

* Do boxplot with delta values for Leeds.
* Redo delta plot, line+dots for all samples (not split by subtype).
* Set the lowest 50 samples and highlight in library size plot.
* Redo heatmap with scaled scores, move put label on top and update colours.
* Update the six-panel figure and match the point size in the last figure.
* Work on the R package:
  * Fix vignette.
  * Run checks and documentation.
  * Add function for preoducing reports.
  * Start writing on the paper.
