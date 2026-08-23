import argparse

from Datasets.datasets import MyDataset
from Models.NeuralNetworks.char_cnn.char_cnn import CharCNN, CharCNNConfig
from Models.NeuralNetworks.char_lstm.char_lstm import CharLSTM, CharLSTMConfig
from Models.NeuralNetworks.config import Config
from Models.NeuralNetworks.data_loader import DatasetLoader
from Models.NeuralNetworks.neural_network import NeuralModel
import torch
# Check if CUDA is available
import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)
import os

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-l", "--level", required=True, type=str)
    parser.add_argument("-m", "--model", required=True, type=str)
    parser.add_argument("-g", "--gpu", required=True, type=str)
    parser.add_argument("-e", "--epoch", required=False, default=30, type=int)
    parser.add_argument("-b", "--batch", required=False, default=1024, type=int)
    parser.add_argument("-lr", "--learning_rate", required=False, default=3e-3, type=float)
    parser.add_argument("-do", "--dropout", required=False, default=0.2, type=float)
    parser.add_argument("-wd", "--weight_decay", required=False, default=1e-7, type=float)
    args = parser.parse_args()
    config = vars(args)

    model_name = config['model']
    level = config['level']
    gpu = config['gpu']
    max_epochs = config['epoch']
    batch_size = config['batch']
    learning_rate = config['learning_rate']
    dropout = config['dropout']
    weight_decay = config['weight_decay']

    os.environ['CUDA_VISIBLE_DEVICES'] = gpu

    dt = MyDataset()

    data = dt.get_data()
    train_validation = dt.get_train()
    test = dt.get_validation()

    config = Config()
    config.batch_size = batch_size
    config.epoch_number = max_epochs
    config.dropout = dropout
    config.lr = learning_rate
    config.weight_decay = weight_decay
    config.max_length = 64

    dl = DatasetLoader(data, level, config)

    train_validation_loader = dl.get_data_loader(train_validation, True)
    test_loader = dl.get_data_loader(test)

    if model_name == 'cnn':
        name = 'char_cnn'
        model_config = CharCNNConfig(config)
        model = CharCNN(model_config)
    elif model_name == 'lstm':
        name = 'char_lstm'
        model_config = CharLSTMConfig(config)
        model = CharLSTM(model_config)
    else:
        raise Exception('which model?')




    nn = NeuralModel(name=name, column=level, model=model, config=config,)
    labels = list(train_validation[level].unique())
    nn.label_encoding = {i: labels.index(i) for i in labels}



    f1 = nn.learner(train_validation_loader, test_loader)

    predicteds, y_trues = nn.testing(test_loader, )

    nn.print_classification_metrics(pred=predicteds, label=y_trues)

    cm = nn.confusion_matrix(label=y_trues, pred=predicteds)
    nn.display_confusion_matrix(cm)

    nn.print_classification_report(label=y_trues, pred=predicteds)