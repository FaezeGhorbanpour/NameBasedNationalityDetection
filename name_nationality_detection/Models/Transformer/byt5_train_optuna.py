import argparse

from Models.Transformer.ByT5 import *
from Datasets.datasets import MyDataset
import os
import sys
from transformers.optimization import Adafactor
from datasets import tqdm

import optuna
from optuna.pruners import MedianPruner
from optuna.trial import TrialState

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-l", "--level", required=True, type=str)
    parser.add_argument("-m", "--model", required=True, type=str)
    parser.add_argument("-g", "--gpu", required=True, type=str)
    parser.add_argument("-e", "--epoch", required=False, default=10, type=int)
    parser.add_argument("-b", "--batch", required=False, default=128, type=int)
    args = parser.parse_args()
    config = vars(args)

    model_name = config['model']
    level = config['level']
    gpu = config['gpu']
    max_epochs = config['epoch']
    batch_size = config['batch']

    os.environ['CUDA_VISIBLE_DEVICES'] = gpu

    model_dir = os.path.join(os.getcwd(), 'models', f"T5/{model_name}-{level}-{max_epochs}/")
    data_dir = os.path.join(os.getcwd(), 'datasets', f"T5/{model_name}-{level}-{max_epochs}/")
    dt = MyDataset()
    train = dt.get_train()
    test = dt.get_test()
    validation = dt.get_validation()
    train_validation = dt.get_train_validation()
    labels = list(train[level].unique())

    train_ds = T5Dataset(train, level)
    checkpoint = "google/byt5-" + model_name
    model = model_init(checkpoint)

    tokenizer = AutoTokenizer.from_pretrained(checkpoint)
    data_collator = DataCollator(tokenizer)
    train_loader = DataLoader(train_ds, batch_size=batch_size, collate_fn=data_collator)

    val_ds = T5Dataset(validation, level)
    val_loader = DataLoader(val_ds, batch_size=batch_size, collate_fn=data_collator)


    def objective(trial):
        learning_rate = trial.suggest_float('learning_rate', 1e-6, 1e-1, log=True)
        weight_decay = trial.suggest_float('weight_decay', 1e-7, 1e-3, log=True)
        opt = trial.suggest_categorical('optimizer', ['AdamW', 'Adafactor'])

        if opt == 'AdamW':
            optimizer = torch.optim.AdamW(model.parameters(), lr=learning_rate, weight_decay=weight_decay)
        else:
            optimizer = Adafactor(model.parameters(), lr=learning_rate, weight_decay=weight_decay, relative_step=False)

        _, eval_metric = training(model, tokenizer, train_loader, val_loader, max_epochs, model_dir, optimizer)

        print(str(trial.number), opt, str(learning_rate), str(weight_decay), str(eval_metric))
        return eval_metric


    optune_default_path = os.path.join(os.getcwd(), 'optuna')
    os.makedirs(optune_default_path, exist_ok=True)

    study = optuna.create_study(study_name=f"ByT5-{model_name}-{level}",
                                sampler=optuna.samplers.TPESampler(),
                                storage=f'sqlite:///{optune_default_path + "/optuna.db"}',
                                load_if_exists=True,
                                direction="minimize",
                                # pruner=MedianPruner(n_startup_trials=1, n_warmup_steps=2)
                                )
    study.optimize(objective, n_trials=1)

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
