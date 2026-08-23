# Name-Based Nationality Detection

This folder holds the name→nationality detection model code for the paper (see the [repository root README](../README.md) for the full abstract, authors, and citation). It's one of the two replication components: this folder trains/serves the nationality-detection models themselves, while [`../migration_analysis`](../migration_analysis) contains Thiago Zordan Malaguth's R/Targets workflow that consumes their predictions to reproduce the paper's migration-analysis figures.

## What this is

Migration research often relies on a scholar's *current* affiliation history to infer where they're from, which systematically misses people who moved before they were ever observed in the data (the "left-censoring" problem). This code instead predicts a person's likely country/region of origin **directly from their full name**, using character-level machine learning models trained on 2.6 million name–nationality pairs collected from Wikipedia.

The resulting models let you estimate name-of-origin independently of career or affiliation history, which is what makes it possible to distinguish genuine **emigration** from **return migration** — e.g., a scholar leaving the USA who originally came from India is a return migrant, not a "brain drain" loss, even though a naive affiliation-based method would count both the same way. Applied to 8+ million scholars in Scopus, this method found that roughly 48% of apparent emigration from the USA was actually return migration.

Predictions are made at three levels of granularity:
- **Level 1** — broad regional groupings (e.g. "African", "East Asian")
- **Level 2** — sub-regions (e.g. "East Africa")
- **Level 3 / country** — individual country, the most granular and hardest classification target

Reported performance (weighted F1): **84%** at the broadest level, **67%** at the country level.

## Pretrained models (HuggingFace)

The ByT5 models described in the paper are published on the [`name-nationality-models`](https://huggingface.co/name-nationality-models) HuggingFace organization:

| Model | Size | Granularity |
|---|---|---|
| [name-nationality-prediction-ByT5-small-level_1](https://huggingface.co/name-nationality-models/name-nationality-prediction-ByT5-small-level_1) | 0.3B | Broad region |
| [name-nationality-prediction-ByT5-base-level_1](https://huggingface.co/name-nationality-models/name-nationality-prediction-ByT5-base-level_1) | 0.6B | Broad region |
| [name-nationality-prediction-ByT5-large-level_1](https://huggingface.co/name-nationality-models/name-nationality-prediction-ByT5-large-level_1) | 1B | Broad region |
| [name-nationality-prediction-ByT5-small-level_2](https://huggingface.co/name-nationality-models/name-nationality-prediction-ByT5-small-level_2) | 0.3B | Sub-region |
| [name-nationality-prediction-ByT5-base-level_2](https://huggingface.co/name-nationality-models/name-nationality-prediction-ByT5-base-level_2) | 0.6B | Sub-region |
| [name-nationality-prediction-ByT5-large-level_2](https://huggingface.co/name-nationality-models/name-nationality-prediction-ByT5-large-level_2) | 1B | Sub-region |
| [name-nationality-prediction-ByT5-small-level_3](https://huggingface.co/name-nationality-models/name-nationality-prediction-ByT5-small-level_3) | 0.3B | Country |
| [name-nationality-prediction-ByT5-base-level_3](https://huggingface.co/name-nationality-models/name-nationality-prediction-ByT5-base-level_3) | 0.6B | Country |
| [name-nationality-prediction-ByT5-large-level_3](https://huggingface.co/name-nationality-models/name-nationality-prediction-ByT5-large-level_3) | 1B | Country |

These are the models actually used for the paper's downstream migration analysis. The fastText, classical ML (XGBoost/Naive Bayes/Logistic Regression), and character-CNN/LSTM implementations in this repo were trained for comparison but are not published as ready-made checkpoints — train them locally if you want to reproduce that part of the study.

### Using a pretrained model directly

No need to clone this repo just to run inference — the models are plain `T5ForConditionalGeneration` checkpoints:

```python
from transformers import AutoTokenizer, T5ForConditionalGeneration

model_id = "name-nationality-models/name-nationality-prediction-ByT5-base-level_3"
tokenizer = AutoTokenizer.from_pretrained("google/byt5-base")
model = T5ForConditionalGeneration.from_pretrained(model_id)

names = ["Alexander Petrov", "Fatima Al-Sayed"]
inputs = tokenizer(names, return_tensors="pt", padding="longest")
outputs = model.generate(**inputs, max_length=50)
print(tokenizer.batch_decode(outputs, skip_special_tokens=True))
```

If you want batch inference with confidence scores instead, use this repo's own pipeline:

```bash
python -m Models.Transformer.byt5_inference_all \
    -m base -p path/to/names.csv -g 0
```

which runs all three levels and writes `names_with_preds.csv` (`data_path` must be a CSV with a `name` column).

## Repository structure

```
Datasets/
  mapping.py            # country <-> ISO code <-> nationality <-> region lookups
  mappings/              # small public reference tables (ISO-3166, region groupings)
  datasets.py            # MyDataset: assembles/splits the training corpus
  preprocessing.py       # name-string cleaning used before training
  utilities.py           # DBpedia/SPARQL lookups used while building some sources
  wiki_bio/ wiki_people/ wiki_person/ wiki_notable/
  wiki_nationality_estimate/ wikidata/ kaggle/  # per-source dataset loaders
  athletes/ politicians/                        # external evaluation / extra training source

Models/
  models.py                     # shared Model base class (metrics, plotting)
  classical_methods/            # TF-IDF + XGBoost / Naive Bayes / Logistic Regression
  fast_text/                    # fastText baseline
  NeuralNetworks/                # char-CNN and char-LSTM models + training/optuna scripts
  Transformer/                   # ByT5 fine-tuning, inference, and optuna hyperparameter search
```

Each dataset subfolder has an `EDA.ipynb` documenting exploration of that source, and each model family has a corresponding notebook showing training/evaluation for each label level.

## Requirements

```bash
pip install -r requirements.txt
```

Python 3.10 was used during development. A CUDA GPU is recommended for training the neural network, ByT5, and fastText models but is not required for using the pretrained HuggingFace checkpoints for inference.

## Setting up the data

This repo does not redistribute the underlying datasets (Wikipedia dumps, Kaggle name lists, etc. are large and/or have their own licenses) — you supply them yourself and point the code at them via an environment variable:

```bash
export NationalityDatasets=/path/to/your/data/
```

Each subfolder under `Datasets/` expects its own raw source file under `$NationalityDatasets/<name>/...` (see each loader's `raw_data_path` for the exact expected filename) and produces a standardized `data.csv` with columns `id, name, level_1, level_2, level_3, country`. `Datasets/datasets.py` then concatenates all sources listed in `train_datasets` (and evaluates against `test_datasets`) and creates the train/validation/test split.

The small reference tables needed for the `level_1`/`level_2`/country label hierarchy (ISO-3166 codes, nationality↔country, country↔region mappings) are bundled in `Datasets/mappings/` and work out of the box — no separate download needed for those.

## Training from scratch

```bash
# fastText baseline
python -m Models.fast_text.fast_text

# classical ML baseline (TF-IDF + XGBoost/NB/LogReg)
# the __main__ block ships with a toy example -- swap in Datasets.datasets.MyDataset
# for real training, the same way Models/fast_text/fast_text.py does
python -m Models.classical_methods.classical_methods

# character-level CNN or LSTM
python -m Models.NeuralNetworks.trainer -l level_1 -m cnn -g 0
python -m Models.NeuralNetworks.trainer -l level_1 -m lstm -g 0

# ByT5 fine-tuning
python -m Models.Transformer.byt5_train -l level_1 -m small -g 0
```

`-l` selects the label column (`level_1`, `level_2`, `level_3`/`country`), `-m` selects model size/type, `-g` selects the GPU device index. See each script's `argparse` block for the full set of hyperparameter flags (learning rate, batch size, epochs, weight decay, optimizer). `Models/NeuralNetworks/optuna_trainer.py` and `Models/Transformer/byt5_train_optuna.py` run the same training loops under Optuna hyperparameter search.

## Citation

See the [repository root README](../README.md) for the paper's full citation and Zenodo replication-package DOI.

## License

Released under the [MIT License](LICENSE).
