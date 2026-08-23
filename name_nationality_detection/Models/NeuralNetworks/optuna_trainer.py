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


import optuna
from optuna.pruners import MedianPruner
from optuna.trial import TrialState



if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-l", "--level", required=True, type=str)
    parser.add_argument("-m", "--model", required=True, type=str)
    parser.add_argument("-g", "--gpu", required=True, type=str)
    parser.add_argument("-e", "--epoch", required=False, default=30, type=int)
    parser.add_argument("-b", "--batch", required=False, default=1024, type=int)
    args = parser.parse_args()
    config = vars(args)

    model_name = config['model']
    level = config['level']
    gpu = config['gpu']
    max_epochs = config['epoch']
    batch_size = config['batch']

    os.environ['CUDA_VISIBLE_DEVICES'] = gpu

    dt = MyDataset()

    data = dt.get_data()
    train = dt.get_train()
    test = dt.get_test()
    validation = dt.get_validation()

    def objective(trial):
        learning_rate = trial.suggest_float('learning_rate', 1e-6, 1e-1, log=True)
        weight_decay = trial.suggest_float('weight_decay', 1e-7, 1e-3, log=True)
        opt = trial.suggest_categorical('optimizer', ['AdamW', 'RMSprop'])
        dropout = trial.suggest_categorical("dropout", [0.2, 0.3, 0.4, 0.5])


        config = Config()

        config.batch_size = batch_size
        config.epoch_number = max_epochs
        config.dropout = dropout
        config.lr = learning_rate
        config.weight_decay = weight_decay
        config.max_length = 64

        dl = DatasetLoader(data, level, config)

        train_loader = dl.get_data_loader(train, True)
        validation_loader = dl.get_data_loader(validation)

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

        if opt == 'AdamW':
            optimizer = torch.optim.AdamW(model.parameters(), lr=learning_rate, weight_decay=weight_decay)
        else:
            optimizer = torch.optim.RMSprop(model.parameters(), lr=learning_rate, weight_decay=weight_decay)


        nn = NeuralModel(name=name, column=level, model=model, config=config, optimizer=optimizer)
        labels = list(train[level].unique())
        nn.label_encoding = {i: labels.index(i) for i in labels}

        f1 = nn.learner(train_loader, validation_loader)

        return f1


    optune_default_path = os.path.join(os.getcwd(), 'optuna')
    os.makedirs(optune_default_path, exist_ok=True)

    study = optuna.create_study(study_name=f"{model_name}-{level}",
                                sampler=optuna.samplers.TPESampler(),
                                storage=f'sqlite:///{optune_default_path + "/optuna.db"}',
                                load_if_exists=True,
                                direction="maximize",
                                pruner=MedianPruner(n_startup_trials=10, n_warmup_steps=5)
                                )
    study.optimize(objective, n_trials=100)

    pruned_trials = study.get_trials(deepcopy=False, states=[TrialState.PRUNED])
    complete_trials = study.get_trials(deepcopy=False, states=[TrialState.COMPLETE])

    s = ''
    s += "Study statistics: \n"
    s += "  Number of finished trials: " + str(len(study.trials)) + "\n"
    s += "  Number of pruned trials: " + str(len(pruned_trials)) + "\n"
    s += "  Number of complete trials: " + str(len(complete_trials)) + "\n"

    trial = study.best_trial
    s += 'number: ' + str(trial.number) + '\n'
    s += 'value: ' + str(trial.value) + '\n'

    s += 'params: \n'
    for key, value in trial.params.items():
        s += "    {}: {}\n".format(key, value)

    print(s)


