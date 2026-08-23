import json
import os

import pandas as pd

import os
from Datasets.datasets import MyDataset


class WikiPerson(MyDataset):
    def __init__(self):
        super().__init__()
        self.name = 'wiki_person'
        datasets_path = os.environ['NationalityDatasets']
        self.raw_data_path = os.path.join(datasets_path, 'Wiki_dataset/wiki_person.json')
        self.raw_data_name = 'data/raw_data.csv'

    def load_raw_data(self):
        if os.path.exists(self.raw_data_name):
            self.raw_data = pd.read_csv(self.raw_data_name)
            self.raw_data.to_csv(self.raw_data_name)
        else:
            keys = ['Name_ID', 'sex or gender', 'occupation', 'date of birth', 'place of birth',
                    'country of citizenship', 'educated at', 'work location', 'place of death', 'place of burial',
                    'country of origin', 'given name', 'residence', 'country', ]

            with open(self.raw_data_path, 'r') as f:
                lines = f.readlines()
                new_lines = list()
                for line in lines:
                    line_json = json.loads(line)

                    new_l = dict()
                    if 'instance of' not in line_json or line_json['instance of'][0]['mainsnak'] != 'Human':
                        continue
                    for key in keys:
                        if key in line_json:
                            values = line_json[key]
                            if isinstance(values, list):
                                new_l[key] = values[0]['mainsnak']
                            else:
                                new_l[key] = values
                        else:
                            new_l[key] = ''
                    new_lines.append(new_l)
            self.raw_data = pd.DataFrame(new_lines)
            self.raw_data.to_csv(self.raw_data_name)


if __name__ == '__main__':
    wiki_person = WikiPerson()
    wiki_person.load_raw_data()
