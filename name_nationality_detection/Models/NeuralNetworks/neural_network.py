import gc
import os
import random

import numpy as np
import optuna
import torch
from optuna.pruners import MedianPruner
from optuna.samplers import TPESampler
from optuna.trial import TrialState
from torch import nn
from torch.optim import AdamW
from torch.optim.lr_scheduler import ReduceLROnPlateau
from tqdm import tqdm
from Models.NeuralNetworks.utilities import EarlyStopping, CheckpointSaving
from Models.models import Model
from torch.cuda.amp import autocast, GradScaler
from sklearn.metrics import accuracy_score, f1_score, precision_score, recall_score
import time, datetime



class NeuralModel(Model):
    def __init__(self, config, model, name, column, optimizer=None):
        super().__init__()

        self.name = name
        self.file_path = os.path.dirname(__file__)
        self.column = column



        self.label_encoding = {}


        torch.cuda.amp.autocast(enabled=True)
        torch.amp.autocast('cuda', enabled=True)
        torch.backends.cudnn.benchmark = True
        self.device = torch.device("cuda" if torch.cuda.is_available() else "cpu")

        self.config = config
        self.model = model

        if optimizer is None:
            self.optimizer = AdamW(self.model.parameters(), lr=self.config.lr, weight_decay=self.config.weight_decay, amsgrad=True)
        else:
            self.optimizer = optimizer

        self.training_stats = []
        self.valid_stats = []

    def validating(self, dataloader):

        if self.model is None:
            raise Exception('Model is not trained yet!')
        self.model.eval()

        total_valid_accuracy = 0
        total_valid_loss = 0
        total_valid_f1 = 0
        total_valid_recall = 0
        total_valid_precision = 0

        valid_dataloader = tqdm(dataloader, desc="Validation Progress", total=len(dataloader))
        for batch in valid_dataloader:
            b_input_ids = batch[0].to(self.device)
            b_labels = batch[1].to(self.device)

            with torch.no_grad():
                logits = self.model(b_input_ids)
                loss = self.criterion(logits, b_labels.view(-1))
                _, predicted = torch.max(logits, 1)

            total_valid_loss += loss.item()

            predicted = predicted.detach().cpu().numpy()
            y_true = b_labels.detach().cpu().numpy()

            total_valid_f1 += f1_score(predicted, y_true,
                                       average='weighted',
                                       labels=np.unique(predicted))

            total_valid_accuracy += accuracy_score(predicted, y_true)

            total_valid_precision += precision_score(predicted, y_true,
                                                     average='weighted',
                                                     labels=np.unique(predicted))

            total_valid_recall += recall_score(predicted, y_true,
                                               average='weighted',
                                               labels=np.unique(predicted))

        avg_accuracy = total_valid_accuracy / len(dataloader)

        avg_val_f1 = total_valid_f1 / len(dataloader)

        avg_precision = total_valid_precision / len(dataloader)

        avg_recall = total_valid_recall / len(dataloader)

        avg_val_loss = total_valid_loss / len(dataloader)

        return avg_val_loss, avg_accuracy, avg_precision, avg_recall, avg_val_f1


    def training(self, dataloader, optimizer, scaler):

        train_total_loss = 0
        total_train_f1 = 0
        self.model.train()

        train_dataloader = tqdm(dataloader, desc="Train Progress", total=len(dataloader))
        for step, batch in enumerate(train_dataloader):

            b_input_ids = batch[0].to(self.device)
            b_labels = batch[1].to(self.device)

            optimizer.zero_grad(set_to_none=True)

            with autocast():
                logits = self.model(b_input_ids)
                loss = self.criterion(logits, b_labels.view(-1))

            train_total_loss += loss.item()

            scaler.scale(loss).backward()
            # loss.backward()
            # scaler.unscale_(optimizer)

            torch.nn.utils.clip_grad_norm_(self.model.parameters(), self.config.max_grad_norm)

            scaler.step(optimizer)
            # optimizer.step()
            scaler.update()

            _, predicted = torch.max(logits, 1)

            predicted = predicted.detach().cpu().numpy()
            y_true = b_labels.detach().cpu().numpy()

            total_train_f1 += f1_score(predicted, y_true,
                                       average='weighted',
                                       labels=np.unique(predicted))

        avg_train_loss = train_total_loss / len(dataloader)

        avg_train_f1 = total_train_f1 / len(dataloader)

        return avg_train_loss, avg_train_f1

    def init_weights(self, m):
        if isinstance(m, nn.LSTM) or isinstance(m, nn.RNN):
            for name, param in m.named_parameters():
                if 'weight' in name:
                    nn.init.xavier_uniform_(param)
                elif 'bias' in name:
                    nn.init.zeros_(param)
    def learner(self, train_data_loaders, validation_data_loader, trial=None):
        torch.cuda.empty_cache()
        number = ''
        if trial is not None:
            self.config.optuna(trial)
            number = str(trial.number)
            print('Trial', trial.number, 'parameters', trial.params)

        torch.manual_seed(27)
        random.seed(27)
        np.random.seed(27)
        best_valid_f1 = 0

        torch.autograd.set_detect_anomaly(False)
        torch.autograd.profiler.profile(False)
        torch.autograd.profiler.emit_nvtx(False)

        self.model = self.model.to(self.device)
        self.model.apply(self.init_weights)

        self.criterion = nn.CrossEntropyLoss()

        epochs = self.config.epoch_number



        early_stopping = EarlyStopping(patience=10, verbose=False)
        # scheduler = ReduceLROnPlateau(optimizer, factor=0.8, patience=10, verbose=True)
        checkpoint_path = os.path.join(self.get_model_path(), self.column + '_' + number + '_checkpoint.pt')
        checkpoint_saving = CheckpointSaving(best_score=0.70, verbose=True, path=checkpoint_path)

        scaler = GradScaler()

        for epoch in range(epochs):
            gc.collect()
            total_t0 = time.time()
            print('======== Epoch {:} / {:} ========'.format(epoch + 1, epochs))

            avg_train_loss, avg_train_f1 = self.training(train_data_loaders, self.optimizer, scaler)

            self.training_stats.append(
                {
                    'Train Loss': avg_train_loss,
                    'Train F1': avg_train_f1
                }
            )

            training_time = format_time(time.time() - total_t0)

            print("Train", end="\t")
            print(f"epoch: {epoch + 1:5d} | trn loss: {avg_train_loss:.5f} | trn f1: {avg_train_f1:.5f} | trn time: {training_time:} ")

            torch.cuda.empty_cache()

            ### validating

            total_t0 = time.time()

            avg_val_loss, avg_accuracy, avg_precision, avg_recall, avg_val_f1 = self.validating(validation_data_loader)

            self.valid_stats.append(
                {
                    'Val Loss': avg_val_loss,
                    'Val accuracy': avg_accuracy,
                    'Val precision': avg_precision,
                    'Val recall': avg_recall,
                    'Val F1': avg_val_f1
                }
            )

            training_time = format_time(time.time() - total_t0)


            print("Validation", end='\t')
            print(f"epoch: {epoch + 1:5d} | val loss: {avg_val_loss:.5f} | val f1: {avg_val_f1:.5f} | val time: {training_time:}")

            torch.cuda.empty_cache()

            if avg_val_f1 >= best_valid_f1:
                best_valid_f1 = avg_val_f1

            # scheduler.step(avg_val_loss)
            early_stopping(avg_val_loss)
            checkpoint_saving(avg_val_f1, self.model)
            if early_stopping.early_stop:
                print('Early stopping ...')
                return best_valid_f1

        return best_valid_f1

    def testing(self, dataloader):
        print("")
        print("Running Testing...")

        if self.model is None:
            raise Exception('Model is not trained yet!')

        self.model.eval()

        total_test_accuracy = 0
        total_test_loss = 0
        total_test_f1 = 0
        total_test_recall = 0
        total_test_precision = 0

        predicteds = None
        y_trues = None

        for step, batch in enumerate(dataloader):
            b_input_ids = batch[0].cuda().long()
            b_labels = batch[1].cuda().long()

            with torch.no_grad():

                logits = self.model(b_input_ids)

                loss = self.criterion(logits, b_labels.view(-1))

                _, predicted = torch.max(logits, 1)

                total_test_loss += loss.item()

            predicted = predicted.detach().cpu().numpy()
            y_true = b_labels.detach().cpu().numpy()

            if predicteds is None:
                predicteds = predicted
            else:
                predicteds = np.concatenate((predicteds, predicted))

            if y_trues is None:
                y_trues = y_true
            else:
                y_trues = np.concatenate((y_trues, y_true))

            total_test_f1 += f1_score(predicted, y_true,
                                      average='weighted',
                                      labels=np.unique(predicted))

            total_test_accuracy += accuracy_score(predicted, y_true)

            total_test_precision += precision_score(predicted, y_true,
                                                    average='weighted',
                                                    labels=np.unique(predicted))

            total_test_recall += recall_score(predicted, y_true,
                                              average='weighted',
                                              labels=np.unique(predicted))

        avg_accuracy = total_test_accuracy / len(dataloader)

        avg_test_f1 = total_test_f1 / len(dataloader)

        avg_precision = total_test_precision / len(dataloader)

        avg_recall = total_test_recall / len(dataloader)

        avg_test_loss = total_test_loss / len(dataloader)

        print("Test", end='\t')
        print(f"test loss: {avg_test_loss:.5f} | test accuracy: {avg_accuracy:.5f} | test precision: {avg_precision:.5f} | test recall: {avg_recall:.5f} | test F1: {avg_test_f1:.5f} ")

        return predicteds, y_trues

    def optuna_learner(self, train_data_loaders, validation_data_loader):
        study = optuna.create_study(
            study_name=self.name + ' ' +self.column,
            sampler=TPESampler(),
            storage='sqlite:///optuna.db',
            load_if_exists=True,
            direction="maximize",
            pruner=MedianPruner(n_startup_trials=5, n_warmup_steps=3)
        )
        study.optimize(lambda trial: self.learner(train_data_loaders, validation_data_loader, trial=trial), n_trials=20)


        pruned_trials = study.get_trials(deepcopy=False, states=[TrialState.PRUNED])
        complete_trials = study.get_trials(deepcopy=False, states=[TrialState.COMPLETE])

        print("\nStudy statistics: ")
        print("  Number of finished trials: ", len(study.trials))
        print("  Number of pruned trials: ", len(pruned_trials))
        print("  Number of complete trials: ", len(complete_trials))

        trial = study.best_trial
        print("Best trial:")
        print("  Value: ", trial.value)
        print("  Params: ")
        for key, value in trial.params.items():
            print("    {}: {}".format(key, value))

        most_important_parameters = optuna.importance.get_param_importances(study)

        print('\nMost important hyperparameters:')
        for key, value in most_important_parameters.items():
            print('  {}:{}{:.2f}%'.format(key, (15 - len(key)) * ' ', value * 100))

def format_time(elapsed):
    elapsed_rounded = int(round((elapsed)))
    return str(datetime.timedelta(seconds=elapsed_rounded))


