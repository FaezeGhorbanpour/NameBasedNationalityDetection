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

    test = pd.read_csv(data_path)
    test_ds = T5Dataset(test, level)

    # model_path = config['model_path']
    os.environ['CUDA_VISIBLE_DEVICES'] = gpu

    for level in ['level_1', 'level_2', 'level_3']:
        model_dir = f"name-nationality-models/name-nationality-prediction-ByT5-{model}-{level}"
        checkpoint = "google/byt5-" + model

        tokenizer = AutoTokenizer.from_pretrained(checkpoint, padding = "longest", return_tensors = "pt")


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
    test.to_csv(data_path.split(".")[0] + f'_with_preds.csv', index=False)
    # m = pipeline.get_model()
    # m.push_to_hub(f"name-nationality-models/name-nationality-prediction-ByT5-{model}-{level}", )
