

import torch
from torch import nn
import torch.nn.functional as F

from Models.NeuralNetworks.config import Config


class CharLSTM(nn.Module):

    def __init__(self, config):
        super().__init__()


        self.embed = nn.Embedding(config.max_length, config.embedding_size)

        self.bilstm = nn.LSTM(config.max_length, config.hidden_dim, config.num_layers, dropout=config.dropout, bidirectional=True)

        # self.fc1 = nn.Linear(config.hidden_dim * 2, config.num_affine_neurons)
        # self.norm1 = nn.LayerNorm(config.num_affine_neurons)
        #
        # self.dropout1 = nn.Dropout(config.dropout)
        # self.fc2 = nn.Linear(config.num_affine_neurons, config.num_affine_neurons)
        # self.norm2 = nn.LayerNorm(config.num_affine_neurons)

        self.dropout2 = nn.Dropout(config.dropout)
        self.fc3 = nn.Linear(config.hidden_dim * 2, config.class_number)
        self.softmax = nn.Softmax(dim=1)

    def forward(self, x, **kwargs):
        x = x.type(torch.cuda.FloatTensor)

        x, self.hidden = self.bilstm(x)

        # x = x.squeeze(2).view(x.size(0), -1)

        # x = F.gelu(self.norm1(self.fc1(x)))
        # x = self.dropout1(x)
        # x = F.gelu(self.norm2(self.fc2(x)))
        x = self.dropout2(x)
        return self.softmax(self.fc3(x))


# instantiate model config -- set ex-post from optuna search
class CharLSTMConfig:
    def __init__(self, config):
        super().__init__()
        self.hidden_dim = 128
        self.num_layers = 2
        self.num_affine_neurons = 16
        self.embedding_size = 32
        self.max_length = config.max_length
        self.class_number = config.class_number
        self.dropout = config.dropout

    def optuna(self, trial):
        self.hidden_dim = trial.suggest_int("hidden_dim", 8, 64)
        self.num_layers = trial.suggest_int("num_layers",  2, 6)
        self.num_affine_neurons = trial.suggest_int("num_affine_neurons",  8, 64)
        self.dropout = trial.suggest_categorical("dropout", [0.2, 0.3, 0.4, 0.5])
        self.lr = trial.suggest_float("lr", 1e-7, 1e-1, log=True)
        self.weight_decay = trial.suggest_float("weight_decay", 1e-7, 1e-1, log=True)