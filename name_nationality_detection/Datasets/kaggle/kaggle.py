import pandas as pd

import os
from Datasets.datasets import MyDataset


class Kaggle(MyDataset):
    def __init__(self):
        super().__init__()
        self.name = 'kaggle'
        datasets_path = os.environ['NationalityDatasets']
        self.raw_data_path = os.path.join(datasets_path, 'Name and Country of Origin dataset/final_all_names_code.csv')

    def load_raw_data(self):
        self.raw_data = pd.read_csv(self.raw_data_path)
