# Scripts, data, and replication materials for "Differentiating Emigration from Return Migration of Scholars Using Name-Based Nationality Detection Models"

This repository includes replication materials, including data, Python, and R scripts to replicate the article's analysis and figures with the following metadata.

**Script authors/maintainers**: [Faeze Ghorbanpour](https://github.com/FaezeGhorbanpour), [Thiago Zordan Malaguth](https://github.com/thiagomalaguth)

**Contact**: faeze.ghorbanpour@tum.de; akbaritabar@demogr.mpg.de

**Article title**: Differentiating Emigration from Return Migration of Scholars Using Name-Based Nationality Detection Models

**Published in**: ICWSM (Proceedings of the International AAAI Conference on Web and Social Media, June 23-26, 2025, Copenhagen, Denmark)

**Manuscript authors**: [Faeze Ghorbanpour](https://github.com/FaezeGhorbanpour), [Thiago Zordan Malaguth](https://github.com/thiagomalaguth), [Aliakbar Akbaritabar](https://github.com/akbaritabar)

**Article DOI**: `TO-BE-ADDED`

**Replication package DOI on Zenodo**: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15103505.svg)](https://doi.org/10.5281/zenodo.15103505)

**Abstract**:
Most web and digital trace data do not include information about an individual's nationality due to privacy concerns. The lack of data on nationality can create challenges for migration research. It can lead to a left-censoring issue since we are uncertain about the migrant's country of origin. Once we observe an emigration event, if we know the nationality, we can differentiate it from return migration. We propose methods to detect the nationality with the least available data, i.e., full names. We use the detected nationality in comparison with the country of academic origin, which is a common approach in studying the migration of researchers. We gathered 2.6 million unique name-nationality pairs from Wikipedia and categorized them into families of nationalities with three granularity levels to use as our training data. Using a character-based machine learning model, we achieved a weighted F1 score of 84% for the broadest- and 67%, for the most granular, country-level categorization. In our empirical study, we used the trained and tested model to assign nationality to 8+ million scholars' full names in Scopus data. Our results show that using the country of first publication as a proxy for nationality underestimates the size of return flows, especially for countries with a more diverse academic workforce, such as the USA, Australia, and Canada. We found that around 48% of emigration from the USA was return migration once we used the country of name origin in contrast to 33% based on academic origin. In the most recent period, 79% of scholars whose affiliation has consistently changed from the USA to China, and are considered emigrants, have Chinese names in contrast to 41% with a Chinese academic origin. Our proposed methods in addressing left-censoring issues are beneficial for other research that uses digital trace data to study migration.

## How to replicate the analysis
To replicate paper's figures, see the `Targets` workflow under `migration analysis" folder which includes R scripts, input data, and output figures written by [Thiago Zordan Malaguth](https://github.com/thiagomalaguth).