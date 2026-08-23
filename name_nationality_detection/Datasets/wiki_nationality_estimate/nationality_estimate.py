import os

import pandas as pd

import os
from Datasets.datasets import MyDataset


class WikiNationalityEstimate(MyDataset):
    def __init__(self):
        super().__init__()
        self.name = 'wiki_nationality_estimate'
        datasets_path = os.environ['NationalityDatasets']
        self.raw_data_path = os.path.join(datasets_path, 'wiki_nationality_estimate/scraped_names.txt')
        self.raw_data_name = 'data/raw_data_1.csv'

    def load_raw_data(self):
        data = pd.read_csv(self.raw_data_path, sep="\t", names=['name', 'country'])
        self.raw_data = pd.DataFrame(data)


if __name__ == '__main__':
    estimate = WikiNationalityEstimate()
    estimate.load_raw_data()
    print(estimate.raw_data.shape)
