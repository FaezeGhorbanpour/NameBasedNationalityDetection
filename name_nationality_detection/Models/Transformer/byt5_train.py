import argparse

from torch.utils.data import DataLoader
from transformers import Adafactor

from Models.Transformer.ByT5 import *
from Datasets.datasets import MyDataset
import os
import sys

from datasets import tqdm



if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-l", "--level", required=True, type=str)
    parser.add_argument("-m", "--model", required=True, type=str)
    parser.add_argument("-g", "--gpu", required=True, type=str)
    parser.add_argument("-e", "--epoch", required=False, default=10, type=int)
    parser.add_argument("-b", "--batch", required=False, default=128, type=int)
    parser.add_argument("-lr", "--learning_rate", required=False, default=1e-4, type=float)
    parser.add_argument("-wd", "--weight_decay", required=False, default=1e-6, type=float)
    parser.add_argument("-o", "--optimizer", required=False, default='Adafactor', type=str)
    args = parser.parse_args()
    config = vars(args)

    model_name = config['model']
    level = config['level']
    gpu = config['gpu']
    max_epochs = config['epoch']
    batch_size = config['batch']
    opt = config['optimizer']
    learning_rate = config['learning_rate']
    weight_decay = config['weight_decay']

    os.environ['CUDA_VISIBLE_DEVICES'] = gpu


    model_dir = os.path.join(os.getcwd(), 'models', f"ByT5/{model_name}-{level}/")
    data_dir = os.path.join(os.getcwd(), 'datasets', f"ByT5/{model_name}-{level}/")
    dt = MyDataset()
    train = dt.get_train()
    test = dt.get_test()
    validation = dt.get_validation()
    train_validation = dt.get_train_validation()
    labels = list(train[level].unique())

    train_ds = T5Dataset(train_validation, level)
    checkpoint = "google/byt5-" + model_name
    model = model_init(checkpoint)

    tokenizer = AutoTokenizer.from_pretrained(checkpoint)
    data_collator = DataCollator(tokenizer)
    train_loader = DataLoader(train_ds, batch_size=batch_size, collate_fn=data_collator)

    val_ds = T5Dataset(test, level)
    val_loader = DataLoader(val_ds, batch_size=batch_size, collate_fn=data_collator)

    if opt == 'AdamW':
        optimizer = torch.optim.AdamW(model.parameters(), lr=learning_rate, weight_decay=weight_decay)
    else:
        optimizer = Adafactor(model.parameters(), lr=learning_rate, weight_decay=weight_decay, relative_step=False)

    trained_model, _ = training(model, tokenizer, train_loader, val_loader, max_epochs, model_dir, optimizer)

    pipeline = Pipeline(trained_model, device="cuda")

    outputs = []
    stream = tqdm(test["name"])
    for text in stream:
        output = pipeline.generate(text)
        # stream.set_description_str(output)
        if output in labels:
            outputs.append(output)
        else:
            outputs.append('None')

    test["pred"] = outputs
    os.makedirs(data_dir, exist_ok=True)
    test.to_csv(data_dir+'test.csv', index=False)

    from Models.models import Model
    m = Model()
    m.print_classification_metrics(test[level], test['pred'])
    true = [labels.index(i) for i in test[level]]
    labels.append('None')
    pred = [labels.index(i) for i in test['pred']]
    m.label_encoding = {i: labels.index(i) for i in labels}
    m.print_classification_metrics(true, pred)
    m.print_classification_report(true, pred, labels=list(m.label_encoding.values()))