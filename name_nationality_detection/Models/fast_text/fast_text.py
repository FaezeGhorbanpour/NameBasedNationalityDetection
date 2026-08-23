import os
import json
import fasttext
import pandas as pd

from Models.models import Model


class FastText(Model):
    def __init__(self, column, data=None, type_='label'):
        super().__init__()
        self.name = 'fast_text'
        self.column = column
        self.type_ = type_

        self.label_encoding = {}
        self.generate_label_encoding(data, type_=type_)

        self.train_path = os.path.join(self.get_data_path(), column + '_train.txt')
        self.test_path = os.path.join(self.get_data_path(), column + '_test.txt')
        self.validation_path = os.path.join(self.get_data_path(), column + '_dev.txt')
        self.model = None
        self.hyper_params = {
            'seed': 0,
            'autotuneValidationFile': self.validation_path,
        }

    def load_model(self, model_path=None):
        path = os.path.join(self.get_model_path(), model_path)
        self.model = fasttext.load_model(path)

    def training(self):
        self.model = fasttext.train_supervised(self.train_path, **self.hyper_params)

    def save_model(self, model_path=None):
        self.model.save_model(os.path.join(self.get_model_path(), model_path))

    def testing(self):
        result = self.model.test(self.test_path)
        return result[1]

    def predict_one(self, text: str) -> int:
        labels, probabilities = self.model.predict(text, 1)
        pred = int(labels[0].replace('__label__', ''))
        return pred

    def predict(self, text: pd.Series) -> pd.Series:
        pred = text.apply(self.predict_one)
        return pred

    def predict_plus_decode(self, text: pd.Series) -> pd.Series:
        pred = self.predict(text)
        return self.decode_label(pred)

    def decode_label(self, pred: pd.Series) -> pd.Series:
        label_decoding = {int(i.replace('__label__','')):j for j, i in self.label_encoding.items()}
        return pred.apply(lambda x: label_decoding[x])

    def generate_label_encoding(self, data=None, type_='label'):
        if data is None:
            with open(os.path.join(self.get_data_path(), self.column+'_' + type_ + '.json'), 'r') as file:
                self.label_encoding = json.load(file)
        else:
            labels = data[self.column].value_counts().index
            self.label_encoding = {label: '__label__' + str(i) for i, label in enumerate(labels)}
            with open(os.path.join(self.get_data_path(), self.column+'_' + type_ + '.json'), 'w') as file:
                json.dump(self.label_encoding, file)

    def save_df(self, df, path):
        columns_name = ['text', 'label']
        df = df.copy()
        df['text'] = df['name']
        if self.column in df.columns:
            df['label'] = df[self.column].apply(lambda x: self.label_encoding[x])
            df = df[columns_name]
        df.to_csv(path, sep='\t', header=None)

    def save_train_test(self, train, test, validation=None):
        self.generate_label_encoding(train)

        self.save_df(train, self.train_path)
        self.save_df(test, self.test_path)

        if validation is not None:
            self.save_df(validation, self.validation_path)

    def load_df(self, fname: str = ''):
        if fname == '':
            fname = self.test_path
        df = pd.read_csv(fname, sep='\t', names=['text', 'label'])
        df['label'] = df['label'].apply(lambda x: int(x.replace('__label__', '')))
        df.fillna('', inplace=True)
        return df

    def get_parameters(self):
        s = ''
        args_obj = self.model.f.getArgs()
        for hparam in dir(args_obj):
            if not hparam.startswith('__'):
                print(f"{hparam} -> {getattr(args_obj, hparam)}")
                s+= f"{hparam} -> {getattr(args_obj, hparam)}\n"
        return s




if __name__ == '__main__':
    from Datasets.datasets import MyDataset

    dt = MyDataset()
    data = dt.get_data()
    train = dt.get_train()
    test = dt.get_test()
    validation = dt.get_validation()

    region_1_ft = FastText(data, column='level_1')
    region_1_ft.save_train_test(train=train, test=test, validation=validation)
    region_1_ft.training()

    accuracy = region_1_ft.testing()
    print('test accuracy: ', str(accuracy))
