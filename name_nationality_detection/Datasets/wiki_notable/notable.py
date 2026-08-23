import pandas as pd
import os
from Datasets.datasets import MyDataset


class WikiNotable(MyDataset):
    def __init__(self):
        super().__init__()
        self.name = 'wiki_notable'
        datasets_path = os.environ['NationalityDatasets']
        self.raw_data_path = os.path.join(datasets_path, 'wikipedia-notable-people/cross-verified-database.csv')
        self.raw_data_name = 'data/raw_data_1.csv'

    def load_raw_data(self):
        data = pd.read_csv(self.raw_data_path, encoding='latin1')
        self.raw_data = pd.DataFrame(data)


if __name__ == '__main__':
    notable = WikiNotable()
    notable.load_raw_data()
    print(notable.raw_data.shape)
