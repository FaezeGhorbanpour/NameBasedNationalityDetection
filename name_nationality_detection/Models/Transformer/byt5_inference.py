import argparse

from torch.utils.data import DataLoader

from Models.Transformer.ByT5 import *
from Datasets.datasets import MyDataset
import os
import sys

from datasets import tqdm



if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-l", "--level", required=True, type=str)
    parser.add_argument("-m", "--model", required=True, type=str)
    parser.add_argument("-p", "--data_path", required=True, default='', type=str)
    parser.add_argument("-g", "--gpu", required=True, default=0, type=str)
    args = parser.parse_args()
    config = vars(args)

    model = config['model']
    level = config['level']
    data_path = config['data_path']
    gpu = config['gpu']

    # model_path = config['model_path']
    os.environ['CUDA_VISIBLE_DEVICES'] = gpu

    model_dir = f"name-nationality-models/name-nationality-prediction-ByT5-{model}-{level}"
    checkpoint = "google/byt5-" + model

    tokenizer = AutoTokenizer.from_pretrained(checkpoint, padding = "longest", return_tensors = "pt")

    test = pd.read_csv(data_path)[:100]
    test_ds = T5Dataset(test, level)

    data_collator = DataCollator(tokenizer)
    test_loader = DataLoader(test_ds, batch_size=1024, collate_fn=data_collator)

    pipeline = Pipeline(model_dir=model_dir, device="cuda")

    outputs = []
    confidences = []
    stream = tqdm(test_loader)
    for text in stream:
        output, confidence = pipeline.generate_with_confidence(text)
        # stream.set_description_str(output)
        outputs += output
        confidences += confidence
        # print(output)

    test[f"{level} pred"] = outputs
    test[f"{level} conf"] = confidences
    test.to_csv(data_path.split(".")[0] + f'_with_{level}_pred.csv', index=False)

    # m = pipeline.get_model()
    # m.push_to_hub(f"name-nationality-models/name-nationality-prediction-ByT5-{model}-{level}", )

    from Models.models import Model
    m = Model()
    labels = list(test[level].unique())
    m.print_classification_metrics(test[level], test[f"{level} pred"])
    true = [labels.index(i) for i in test[level]]
    labels.append('None')
    pred = [labels.index(i) for i in test[f"{level} pred"]]
    m.label_encoding = {i: labels.index(i) for i in labels}
    m.print_classification_metrics(true, pred)
    m.print_classification_report(true, pred, labels=list(m.label_encoding.values()))
