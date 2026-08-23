import os
from collections import Counter

import numpy as np
import pandas as pd
import seaborn as sns
import torch
from matplotlib import pyplot as plt
from sklearn.metrics import classification_report, confusion_matrix, f1_score, balanced_accuracy_score, precision_score, \
    recall_score, accuracy_score


class Model:

    def __init__(self):
        self.name = ''
        self.type_ = ''
        self.column = ''
        self.file_path = os.environ['NationalityDatasets']
        self.label_encoding = {}


    def get_labels(self):
        return list(self.label_encoding.keys())

    def get_encoding(self):
        return self.label_encoding

    def encode_label(self, label: pd.Series) -> pd.Series:
        pass

    def get_data_path(self):
        return os.path.join(self.file_path, self.name, 'data')

    def get_model_path(self):
        return os.path.join(self.file_path, self.name, 'models')

    def get_results_path(self):
        return os.path.join(self.file_path, self.name, 'results')

    # def training(self, **kwargs):
    #     pass
    #
    # def testing(self, **kwargs):
    #     pass

    def predict(self, text):
        pass

    def confusion_matrix(self, label, pred, type='precision'):
        if type == 'precision':
            normalize = 'pred'
        elif type == 'recall':
            normalize = 'true'
        else:
            normalize = 'all'
        cm = confusion_matrix(label, pred, normalize=normalize)
        return cm

    def print_classification_report(self, label, pred, labels=None):
        print(classification_report(label, pred, labels=labels, target_names=self.get_labels(), zero_division=0))

    @staticmethod
    def f1_micro_score(label, pred):
        return f1_score(label, pred, average='micro')

    @staticmethod
    def f1_macro_score(label, pred):
        return f1_score(label, pred, average='macro')

    @staticmethod
    def f1_weighted_score(label, pred):
        return f1_score(label, pred, average='weighted')

    @staticmethod
    def precision_score(label, pred):
        return precision_score(label, pred, average='weighted')

    @staticmethod
    def recall_score(label, pred):
        return recall_score(label, pred, average='weighted')

    @staticmethod
    def accuracy_score(label, pred):
        return accuracy_score(label, pred)
    @staticmethod
    def balanced_accuracy_score(label, pred):
        return balanced_accuracy_score(label, pred)

    def print_classification_metrics(self, label, pred):
        print('Micro F1-score', f1_score(label, pred, average='micro'))
        print('Macro F1-score', f1_score(label, pred, average='macro'))
        print('Weighted F1-score', f1_score(label, pred, average='weighted'))
        print('Weighted Precision', precision_score(label, pred, average='weighted'))
        print('Weighted Recall', recall_score(label, pred, average='weighted'))
        print('Accuracy Score', accuracy_score(label, pred))
        print('Balanced Accuracy Score', balanced_accuracy_score(label, pred))

    def display_confusion_matrix(self, cm, ):
        num_classes = len(self.get_labels())
        print(cm.shape, num_classes)

        figsize = min(max(0.75 * num_classes, 8), 20)

        fontsize = min(max(100 / num_classes, 10), 18)

        plt.figure(figsize=(figsize, figsize))
        ax = sns.heatmap(cm, annot=True, fmt=".2f", cmap="Greens", square=True, cbar=False, linewidth=0.5,
                         annot_kws={"fontsize": fontsize})
        ax.set_xticklabels(self.get_labels(), fontsize=fontsize, rotation=90, ha="center")
        ax.set_yticklabels(self.get_labels(), fontsize=fontsize, rotation=0, va="center")
        plt.xlabel("Predicted Label", fontsize=fontsize + 4)
        plt.ylabel("True Label", fontsize=fontsize + 4)
        plt.xticks(fontsize=fontsize)
        plt.yticks(fontsize=fontsize)
        plt.title(f"Confusion Matrix - {self.column} - {self.type_}", fontsize=fontsize + 6)
        plt.show()

    def generate_label_encoding(self, data):
        pass
