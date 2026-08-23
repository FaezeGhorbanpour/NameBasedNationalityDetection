### http://seekinginference.com/applied_nlp/CharCNN.html

import torch
from torch import nn
import torch.nn.functional as F

from Models.NeuralNetworks.config import Config


class CharCNN(nn.Module):

    def __init__(self, config):
        super().__init__()

        self.conv_layer_number = config.num_conv_layers - 2

        self.embed = nn.Embedding(config.max_length, config.embedding_size)
        self.conv1 = nn.Conv1d(in_channels=config.max_length, out_channels=config.num_conv_filters, kernel_size=4, bias=False)
        self.norm1 = nn.BatchNorm1d(config.num_conv_filters)

        layers = []
        for i in range(self.conv_layer_number):
            if i < self.conv_layer_number/2:
                layers.append(nn.Conv1d(config.num_conv_filters, config.num_conv_filters, kernel_size=4, bias=False))
                layers.append(nn.BatchNorm1d(config.num_conv_filters))
            else:
                layers.append(nn.Conv1d(config.num_conv_filters, config.num_conv_filters, kernel_size=2, bias=False))
                layers.append(nn.BatchNorm1d(config.num_conv_filters))

        self.conv_layers = nn.Sequential(*layers)

        self.conv2 = nn.Conv1d(in_channels=config.num_conv_filters, out_channels=config.output_channel, kernel_size=2)

        # self.dropout1 = nn.Dropout(config.dropout)
        # self.fc1 = nn.Linear(config.output_channel, config.num_affine_neurons)
        # self.norm4 = nn.LayerNorm(config.num_affine_neurons)

        # self.dropout2 = nn.Dropout(config.dropout)
        # self.fc2 = nn.Linear(config.num_affine_neurons, config.num_affine_neurons)
        # self.norm5 = nn.LayerNorm(config.num_affine_neurons)

        self.dropout3 = nn.Dropout(config.dropout)
        self.fc3 = nn.Linear(config.output_channel, config.class_number)
        self.softmax = nn.Softmax(dim=1)

    def forward(self, x, **kwargs):

        x = self.embed(x)

        x = self.conv1(x)
        x = F.gelu(x)
        x = self.norm1(x)

        if self.conv_layer_number > 0:
            x = self.conv_layers(x)

        x = self.conv2(x)

        x = F.max_pool1d(x, x.size(2)).squeeze(2).view(x.size(0), -1)

        # x = self.dropout1(x)
        # x = F.gelu(self.norm4(self.fc1(x)))
        # x = self.dropout2(x)
        # x = F.gelu(self.norm5(self.fc2(x)))
        x = self.dropout3(x)
        return self.softmax(self.fc3(x))


# instantiate model config -- set ex-post from optuna search
class CharCNNConfig:
    def __init__(self, config):
        super().__init__()
        self.num_conv_filters = 16
        self.output_channel = 16
        self.num_affine_neurons = 32
        self.num_conv_layers = 2
        self.embedding_size = 32
        self.max_length = config.max_length
        self.class_number = config.class_number
        self.dropout = config.dropout


    def optuna(self, trial):
        # self.num_conv_layers = trial.suggest_int("num_conv_layers", 2, 3)
        self.num_conv_filters = trial.suggest_int("num_conv_filters", 8, 64)
        self.output_channel = trial.suggest_int("output_channel",  8, 64)
        self.num_affine_neurons = trial.suggest_int("num_affine_neurons",  16, 64)
        self.dropout = trial.suggest_categorical("dropout", [0.2, 0.3, 0.4, 0.5])
        self.lr = trial.suggest_float("lr", 1e-7, 1e-1, log=True)
        self.weight_decay = trial.suggest_float("weight_decay", 1e-7, 1e-1, log=True)