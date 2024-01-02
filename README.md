
# Artificial intelligence enabled electrocardiography contributes to hyperthyroidism detection and outcome prediction

This study focuses on the utilization of AI-ECG for predicting hyperthyroidism. Due to privacy concerns, we are unable to provide the original files of ECGs for all patients. However, we have included some sample data to enable researchers to replicate the model training process. Once the model training is completed, de-identified tabular data is made available in the data folder. It's worth noting that this repository is built using the R software environment, specifically version 3.4.4, and utilizes MXNet version 1.3.0.

The overall file structure is as follows:

```shell
ECG-for-HT
├── results
│   ├── ...
├── code
│   ├── ...
├── code-for-training
│   ├── all-in-one.R
│   ├── suppoting-code
│   │   ├── ...
├── data
│   ├── raw_data.RData
├── example-for-traning
│   ├── train.csv
│   ├── test.csv
│   ├── ecg
│   │   ├── U0001.csv
│   │   ├── U0002.csv
│   │   ├── ...
```

## Model training

The "code-for-training/supporting-code" folder contains three scripts, which do not require preloading. You can simply execute the "all-in-one.R" script. Before running this script, make sure to place the "train.csv" and "test.csv" files in the "example-for-training" folder. The "train.csv" file should include labels indicating whether the ECGs have any diseases. Additionally, within the "example-for-training/ecg" folder, please ensure you have the corresponding CSV files describing the waveform of each ECG (5000x12).

## Analysis for the randomized controlled trial

The "code" folder contains the syntax for generating all the charts. Simply execute it, and the corresponding files will appear in the "results" folder.

## Related publications

The paper is accepted by Communications Medicine. 