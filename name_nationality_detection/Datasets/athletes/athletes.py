import pandas as pd
import os
import os
from Datasets.datasets import MyDataset


class Athletes(MyDataset):
    def __init__(self):
        super().__init__()
        self.name = 'athletes'
        datasets_path = os.environ['NationalityDatasets']
        self.raw_data_path = os.path.join(datasets_path, 'List of People names by Countries/Country-People-Listing.csv')


    def load_raw_data(self):
        self.raw_data = pd.read_csv(self.raw_data_path)
