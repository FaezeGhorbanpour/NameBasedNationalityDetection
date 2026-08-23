import pyarrow.parquet as pq

import os
from Datasets.datasets import MyDataset


class Wikidata(MyDataset):
    def __init__(self):
        super().__init__()
        self.name = 'wikidata'
        datasets_path = os.environ['NationalityDatasets']
        self.raw_data_path = os.path.join(datasets_path, '2022-12-wikidata_all_persons_withCountry_sample.parquet')

    def load_raw_data(self):
        parquet_file = pq.read_table(self.raw_data_path)
        self.raw_data = parquet_file.to_pandas()
