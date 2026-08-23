from collections import Counter
import torch

from torch.utils.data import DataLoader, Dataset
from torchtext.vocab import build_vocab_from_iterator
from torchtext.data.utils import get_tokenizer

from Datasets.datasets import MyDataset

class CharLevelTextDataset(Dataset, MyDataset):
    def __init__(self, texts, labels, vocab):
        super().__init__()
        self.texts = texts
        self.labels = labels
        self.vocab = vocab

    def __len__(self):
        return len(self.texts)

    def __getitem__(self, idx):
        text = self.texts[idx]
        label = self.labels[idx]
        vectorized_text = torch.tensor([self.vocab[char] for char in text], dtype=torch.long)
        return vectorized_text, torch.tensor(label, dtype=torch.long)

class DatasetLoader:
    def __init__(self, data, column, config):

        self.column = column
        self.config = config

        self.max_length = config.max_length
        self.vector_size = config.vector_size

        self.character_to_int = None

        self.label_encoding = {}
        self.generate_label_encoding(data[self.column])

        self.config.class_number = len(self.label_encoding)
        self.vocab = None
        self.tokenizer = get_tokenizer("basic_english")


    def generate_label_encoding(self, column):
        labels = column.value_counts().index
        self.label_encoding = {label: i for i, label in enumerate(labels)}

    def build_char_vocab(self, texts):
        def yield_chars(texts):
            for text in texts:
                yield list(text)

        vocab = build_vocab_from_iterator(yield_chars(texts))
        # vocab.set_default_index(vocab["<unk>"])
        return vocab

    # Custom collate function to pad/truncate sequences to a fixed length of 60
    def collate_fn(self, batch):
        texts, labels = zip(*batch)
        padded_texts = []

        for text in texts:
            if len(text) > self.config.max_length:
                # Truncate sequences longer than max_length
                padded_text = text[:self.config.max_length]
            else:
                # Pad sequences shorter than max_length
                padding = [0] * (self.config.max_length - len(text))  # Use 0 as padding token
                padded_text = torch.cat((text, torch.tensor(padding, dtype=torch.long)))

            padded_texts.append(padded_text)

        padded_texts = torch.stack(padded_texts, dim=0)
        labels = torch.stack(labels)
        lengths = torch.tensor([min(len(text), self.config.max_length) for text in texts])  # Store original or truncated lengths

        return padded_texts, labels, lengths


    def get_data_loader(self, data, shuffle=False):
        texts = data.name
        labels = data[self.column].apply(lambda x: self.label_encoding[x]).values
        # Build Vocabulary
        vocab = self.build_char_vocab(texts)

        # Create Dataset and DataLoader
        dataset = CharLevelTextDataset(texts, labels, vocab)
        dataloader = DataLoader(dataset, batch_size=self.config.batch_size, shuffle=shuffle, collate_fn=self.collate_fn)
        return dataloader
