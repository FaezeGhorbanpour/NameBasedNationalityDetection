import os

import pandas as pd

import os
from Datasets.datasets import MyDataset


class WikiBio(MyDataset):
    def __init__(self):
        super().__init__()
        self.name = 'wiki_bio'
        datasets_path = os.environ['NationalityDatasets']
        self.raw_data_path = os.path.join(datasets_path, 'wikipedia-biography-dataset')
        self.raw_data_name = 'data/raw_data.csv'

    def load_raw_data(self):
        if os.path.exists(self.raw_data_name):
            self.raw_data = pd.read_csv(self.raw_data_name)
        else:
            keys = ['name', 'birth name', 'birth place', 'birthplace', 'residence', 'nationality', 'ethnicity',
                    'citizenship', 'other names', 'home town', 'fullname', ]

            data = list()
            for data_type in ['train', 'test', 'valid']:
                with open(os.path.join(self.raw_data_path, data_type, data_type + '.box'), 'r', encoding="utf8") as f:
                    lines = f.readlines()
                    for line in lines:
                        line_dict = dict()
                        for i in line.split():
                            if ':' in i:
                                key_value = i.split(':')
                                key = " ".join(
                                    key_value[0].split('_')[:-1] if key_value[0].split('_')[-1] in [str(i) for i in
                                                                                                    set(range(50))] else
                                    key_value[0].split('_'))
                                if key in keys:
                                    if key in line_dict:
                                        line_dict[key] += ' ' + key_value[1]
                                    else:
                                        line_dict[key] = key_value[1]

                        for i in keys:
                            if i not in line_dict.keys():
                                line_dict[i] = ''
                            elif 'none' in line_dict[i]:
                                line_dict[i] = ''
                        data.append(line_dict)

            self.raw_data = pd.DataFrame(data)
            self.raw_data.to_csv(self.raw_data_name)


if __name__ == '__main__':
    bio = WikiBio()
    bio.load_raw_data()
