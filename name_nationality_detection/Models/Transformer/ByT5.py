import math
import os
from typing import Dict, List, Tuple
import torch.nn.functional as F
import numpy as np
import pandas as pd
import torch

# from nltk.metrics.distance import edit_distance
from torch.utils.data import Dataset
from tqdm.auto import tqdm
from transformers import AutoTokenizer, T5ForConditionalGeneration


class T5Dataset(Dataset):
    def __init__(self, df, level):
        self.df = pd.DataFrame({'input': df['name'], 'label': df[level]})

    def __len__(self):
        return len(self.df)

    def __getitem__(self, idx):
        row = self.df.iloc[idx]
        return row.input, row.label


class DataCollator:
    def __init__(self, tokenizer):
        self.tokenizer = tokenizer

    def __call__(self, batch: List[Tuple[str, str]]) -> Dict[str, torch.Tensor]:
        inputs = [input if isinstance(input, str) else '' for input, _ in batch]
        print([input for input, _ in batch if not isinstance(input, str) ])
        labels = [label for _, label in batch]
        encodings = self.tokenizer(inputs, padding="longest", return_tensors="pt")
        label_encodings = self.tokenizer(labels, padding="longest", return_tensors="pt")

        input_ids = encodings.input_ids
        attention_mask = encodings.attention_mask
        labels = label_encodings.input_ids
        labels[labels == self.tokenizer.pad_token_id] = -100
        return {
            "input_ids": input_ids,
            "attention_mask": attention_mask,
            "labels": labels,
        }

def model_init(checkpoint):
    model = T5ForConditionalGeneration.from_pretrained(checkpoint)
    return model.cuda()




class Pipeline:
    def __init__(self, model=None, tokenizer=None, model_dir="", tokenizer_name="google/byt5-base", ged_token="$", device="cpu"):
        # self.model = T5ForConditionalGeneration.from_pretrained("google/byt5-base").to(
        #     device
        # )
        # state_dict = torch.load(model_path)["model"]
        # self.model.load_state_dict(state_dict)
        if model is None:
            self.model = T5ForConditionalGeneration.from_pretrained(model_dir).to(device)
        else:
            self.model = model
        if tokenizer is None:
            self.tokenizer = AutoTokenizer.from_pretrained(tokenizer_name)
        else:
            self.tokenizer = tokenizer
        self.bos_token_id = self.tokenizer.pad_token_id
        self.eos_token_id = self.tokenizer.eos_token_id
        self.ged_token_id = self.tokenizer.encode(ged_token)[0]
        self.device = device

    def get_model(self):
        return self.model
    @torch.no_grad()
    def generate(self, batch, max_tokens=50):
        device = self.device
        batch = dict_to_cuda(batch, device) #input_ids.to(device)

        # input_ids = self.tokenizer.encode(text, return_tensors="pt").to(device)
        output_ids = self.model.generate(batch['input_ids'], max_length=max_tokens)#[0].tolist()
        output_strings = []
        for output_id in output_ids:
            output_id = output_id.tolist()

            output_ids_list = []
            start_token = 0
            sentinel_token = 258
            while sentinel_token in output_id:
                split_idx = output_id.index(sentinel_token)
                output_ids_list.append(output_id[start_token:split_idx])
                start_token = split_idx
                sentinel_token -= 1

            output_ids_list.append(output_id[start_token:])
            output_string = self.tokenizer.batch_decode(output_ids_list)
            output_string = "".join(output_string).replace('<pad>', '').replace('</s>', '').strip()
            output_strings.append(output_string)
        return output_strings

    import torch
    import torch.nn.functional as F

    def generate_with_confidence(self, batch, max_tokens=50):
        device = self.device
        batch = dict_to_cuda(batch, device)

        # Generate tokens with the model and return logits
        output = self.model.generate(
            batch['input_ids'],
            max_length=max_tokens,
            return_dict_in_generate=True,
            output_scores=True
        )

        output_ids = output.sequences
        scores = output.scores  # List of logits for each generated token
        output_strings = []
        confidences = []

        for i, output_id in enumerate(output_ids):
            output_id = output_id.tolist()

            # Decode the output sequence
            output_string = self.tokenizer.decode(output_id, skip_special_tokens=True).strip()
            output_strings.append(output_string)

            # Calculate confidence scores for the predicted label sequence
            token_probs = []
            for j in range(min(len(scores), len(output_string))):  # Limit to the generated tokens
                token_logits = scores[j][i]  # Logits for the j-th token in the sequence
                token_prob = F.softmax(token_logits, dim=-1)  # Convert logits to probabilities
                predicted_token_id = output_id[j + 1]  # Token IDs start after the input sequence
                token_probs.append(token_prob[predicted_token_id].item())

            # Combine probabilities for the label tokens
            if token_probs:  # Avoid empty list
                avg_confidence = (sum(token_probs) / len(token_probs)) * 10 % 1  # Average confidence for the sequence
            else:
                avg_confidence = 0.0  # Default if no tokens are generated

            confidences.append(avg_confidence)

        return output_strings, confidences

def dict_to_cuda(batch, device):
    return {k: v.cuda().to(device) for k, v in batch.items()}

@torch.no_grad()
def eval_model(model, val_loader):
    model.eval()
    total = 0
    count = 0
    stream = tqdm(val_loader)
    for batch in stream:
        batch = dict_to_cuda(batch, model.device)
        loss = model(**batch).loss
        total += loss.item()
        count += 1
        stream.set_description_str(desc=f"val_edit_dist {total / count:.3f}")

    model.train()
    return total / count


def training(model, tokenizer, train_loader, val_loader, max_epochs, model_dir, optimizer):

    best = float("inf")
    trigger = 0
    NUM_ACCUMULATION_STEPS = 1
    eval_metric = math.inf
    for epoch in range(max_epochs):
        stream = tqdm(train_loader)
        total = 0
        count = 0
        for idx, batch in enumerate(stream):
            batch = dict_to_cuda(batch, model.device)
            loss = model(**batch).loss

            loss = loss / NUM_ACCUMULATION_STEPS
            loss.backward()
            if ((idx + 1) % NUM_ACCUMULATION_STEPS == 0) or (idx + 1 == len(train_loader)):
                optimizer.step()
                optimizer.zero_grad()

            total += loss.item()
            count += 1
            stream.set_description_str(
                f"Epoch {epoch}/{max_epochs} | Loss {total / count : .3f}"
            )
        loss = total / count
        eval_metric = eval_model(model, val_loader, tokenizer)

        print(
            f"Epoch {epoch}/{max_epochs} | Loss {loss: .3f} | Val Loss {eval_metric: .4f}"
        )

        trigger += 1

        if eval_metric < best:
            model.save_pretrained(model_dir)
            best = eval_metric
            trigger = 0

        if trigger > 5:
            break

    return model, eval_metric
