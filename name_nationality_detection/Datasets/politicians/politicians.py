import pandas as pd

import os
from Datasets.datasets import MyDataset


class Politicians(MyDataset):
    def __init__(self):
        super().__init__()
        self.name = 'politicians'
        datasets_path = os.environ['NationalityDatasets']
        self.raw_data_path = os.path.join(datasets_path, 'politician-data-wikipedia-edge-list/politician-data.csv')

    def load_raw_data(self):
        data = pd.read_csv(self.raw_data_path, sep='\t')
        self.raw_data = data


if __name__ == '__main__':
    politicians = Politicians()
    politicians.get_raw_data()
