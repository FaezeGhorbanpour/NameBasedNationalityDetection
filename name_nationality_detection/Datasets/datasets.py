import os
import pandas as pd
from sklearn.model_selection import train_test_split

from Datasets import train_datasets

pd.options.plotting.backend = "plotly"
import warnings
warnings.filterwarnings('always')
class MyDataset:

    def __init__(self):
        self.name = ''

        self.data = None

        self.validation_size = 0.15
        self.test_size = 0.2
        self.train_size = 1 - (self.validation_size + self.test_size)
        self.train = None
        self.test = None
        self.validation = None
        self.train_validation = None

        self.raw_data_path = ''
        self.raw_data = None

        self.file_path = os.environ['NationalityDatasets']

        self.data_columns = ['id', 'name', 'level_1', 'level_2', 'level_3', 'country']



    def get_data_path(self):
        return os.path.join(self.file_path, self.name, 'data')

    def save_split_data(self):
        if self.train is None:
            self.split_data()
        self.test.to_csv(os.path.join(self.get_data_path(), 'test.csv'), index=False)
        self.train_validation.to_csv(os.path.join(self.get_data_path(), 'train_validation.csv'), index=False)
        self.train.to_csv(os.path.join(self.get_data_path(), 'train.csv'), index=False)
        self.validation.to_csv(os.path.join(self.get_data_path(), 'validation.csv'), index=False)

    def split_data(self):
        if self.data is None:
            self.load_data()

        self.train_validation, self.test = train_test_split(self.data, test_size=self.test_size, shuffle=True)
        self.train, self.validation = train_test_split(self.train_validation, test_size=self.validation_size / (
                self.train_size + self.validation_size), shuffle=True)

    def load_split_data(self):
        try:
            self.train = self.get_train()
            self.train_validation = self.get_train_validation()
            self.test = self.get_test()
            self.validation = self.get_validation()
        except:
            self.load_data()
            self.split_data()

    def get_train(self):
        if self.train is None:
            data_path = os.path.join(self.get_data_path(), 'train.csv')
            if os.path.exists(data_path):
                self.train = pd.read_csv(data_path)
                self.train.fillna('', inplace=True)
            else:
                self.split_data()
        return self.train

    def get_test(self):
        if self.test is None:
            data_path = os.path.join(self.get_data_path(), 'test.csv')
            if os.path.exists(data_path):
                self.test = pd.read_csv(data_path)
                self.test.fillna('', inplace=True)
            else:
                self.split_data()
        return self.test

    def get_validation(self):
        if self.validation is None:
            data_path = os.path.join(self.get_data_path(), 'validation.csv')
            if os.path.exists(data_path):
                self.validation = pd.read_csv(data_path)
                self.validation.fillna('', inplace=True)
            else:
                self.split_data()
        return self.validation

    def get_train_validation(self):
        if self.train_validation is None:
            data_path = os.path.join(self.get_data_path(), 'train_validation.csv')
            if os.path.exists(data_path):
                self.train_validation = pd.read_csv(data_path)
                self.train_validation.fillna('', inplace=True)
            else:
                self.split_data()
        return self.train_validation

    def load_raw_data(self):
        raise Exception('Not implemented!')

    def get_raw_data(self):
        if self.raw_data is None:
            self.load_raw_data()
        return self.raw_data

    def save_data(self, data=None, name='data.csv'):
        if data is None:
            if self.data is None:
                self.load_data()
                data = self.data
        data_path = os.path.join(self.get_data_path(), name)
        data.to_csv(data_path)

    def load_data(self, reload=False):
        data_path = os.path.join(self.get_data_path(), 'data.csv')
        if os.path.exists(data_path) and not reload:
            self.data = pd.read_csv(data_path)
            self.data.fillna('', inplace=True)
        else:
            datas = []
            for dataset_name in train_datasets:
                dataset_path = os.path.join(self.file_path, dataset_name, 'data', 'data.csv')
                dataset = pd.read_csv(dataset_path)
                print(dataset_name, dataset.shape)
                datas.append(dataset)
            self.data = pd.concat(datas, ignore_index=True, sort=False)

    def get_data(self):
        if self.data is None:
            self.load_data()
        return self.data

    def load_external_evaluation_data(self, name='athletes'):
        dataset_path = os.path.join(self.file_path, name, 'data', 'data.csv')
        dataset = pd.read_csv(dataset_path)
        dataset.fillna('', inplace=True)
        self.external_evaluation_data = dataset

    def get_external_evaluation_data(self):
        if self.external_evaluation_data is None:
            self.load_external_evaluation_data()
        return self.external_evaluation_data






