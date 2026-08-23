import numpy as np
import torch


class CheckpointSaving:

    def __init__(self, best_score=np.inf, path='models/checkpoint.pt', verbose=True, trace_func=print):
        self.best_score = best_score
        self.val_f1_max = 0
        self.path = path
        self.verbose = verbose
        self.trace_func = trace_func

    def __call__(self, val_f1, model):
        if self.best_score is None:
            self.best_score = val_f1
            self.save_checkpoint(val_f1, model)
        elif val_f1 > self.best_score:
            self.best_score = val_f1
            self.save_checkpoint(val_f1, model)
        return

    def save_checkpoint(self, val_f1, model):
        if self.verbose:
            self.trace_func(
                f'Validation f1 score increased ({self.val_f1_max:.6f} --> {val_f1:.6f}). Model saved ...')
        torch.save(model.state_dict(), self.path)
        self.val_f1_max = val_f1


class EarlyStopping:

    def __init__(self, patience=10, verbose=False, delta=0.001, path='checkpoint.pt', trace_func=print):
        self.patience = patience
        self.verbose = verbose
        self.counter = 0
        self.best_score = None
        self.early_stop = False
        self.val_loss_min = np.Inf
        self.delta = delta
        self.path = path
        self.trace_func = trace_func

    def __call__(self, val_loss):

        score = -val_loss

        if self.best_score is None:
            self.best_score = score
            if self.verbose:
                self.trace_func(f'Validation loss decreased ({self.val_loss_min:.6f} --> {val_loss:.6f}).')
            self.val_loss_min = val_loss
            # self.save_checkpoint(val_loss, model)
        elif score < self.best_score + self.delta or np.isnan(score):
            self.counter += 1
            self.trace_func(f'EarlyStopping counter: {self.counter} out of {self.patience}')
            if self.counter >= self.patience or np.isnan(score):
                self.early_stop = True
        else:
            self.best_score = score
            if self.verbose:
                self.trace_func(f'Validation loss decreased ({self.val_loss_min:.6f} --> {val_loss:.6f}).')
            # self.save_checkpoint(val_loss, model)
            self.val_loss_min = val_loss
            self.counter = 0