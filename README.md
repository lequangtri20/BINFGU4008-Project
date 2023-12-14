# Impact of Model Design on Performance and Gender Bias for Bone Age Prediction Tasks

## Introduction
Bone age estimation is a very common procedure conducted on adolescents to
assess their skeletal maturity. These estimations are crucial for the diagnosis and
treatment of growth disorders and other orthopedic issues. Current methods for
bone age estimation rely on time-consuming and highly subjective techniques that
waste hospital resources and can lead to adverse patient outcomes. In this study, we
explore the impact of model design on performance and gender bias for bone age
prediction tasks using the RSNA Pediatric Bone Age Machine Learning Challenge
dataset.

## File Descriptions
- `VGG-16.ipynb`, `AlexNet.ipynb`, `ResNet-18.ipynb` and `ResNet-50.ipynb`: Main implementations of architecture used in the project.
- `Models.ipynb`: Include parent class for all models. It includes training code for a single epoch and validation which are called from `Trainer.ipynb`.
- `EDA_and_Preprocessing.ipynb`: Include preprocessing code and data exploration.
- `CustomDataset.ipynb`: Include the Pytorch dataset customized for the project.
- `Trainer.ipynb`: Include the main training loop with customized epoch and bootstrapping test errors.
- `Metrics.ipynb`: Include implementations of metrics (Correlation, R2, MSE, MAE and MAPE) and plotting for evaluation.
- `Results.ipynb`: Include code to explore results and export relevant tables for the report.
- `Create_Report_Plots.R`: R implementations for other plots.

## Other links
- Dataset, Weights, Results and Plots are available at: https://drive.google.com/drive/folders/1DefjK5ear8NeNRqZsclHu5l-c5y3vHcK?usp=drive_link
