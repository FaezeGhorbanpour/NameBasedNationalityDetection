import pandas as pd
import os
import os
from Datasets.datasets import MyDataset
from Datasets.utilities import get_person_nationality_with_dbpedia_link


class WikiPeople(MyDataset):
    def __init__(self):
        super().__init__()
        self.name = 'wiki_people'

        datasets_path = os.environ['NationalityDatasets']
        self.raw_data_path = os.path.join(datasets_path, 'People Wikipedia Data/people_wiki.csv')
        self.raw_data_name = 'data/raw_data.csv'

    def load_raw_data(self):
        if os.path.exists(self.raw_data_name):
            self.raw_data = pd.read_csv(self.raw_data_name)
        else:
            self.raw_data = pd.read_csv(self.raw_data_path)


if __name__ == '__main__':
    wiki_people = WikiPeople()
    data = wiki_people.get_raw_data()
    data.fillna('', inplace=True)
    data['birth place'] = data['URI'].apply(lambda x: get_person_nationality_with_dbpedia_link(x).split('/')[-1])
    data.to_csv('data/raw_data.csv')
