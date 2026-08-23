import os

import pandas as pd

_default_mappings_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'mappings')
os.environ.setdefault('NationalityMappings', _default_mappings_path)
datasets_path = os.environ['NationalityMappings']

countries_with_regional_codes = os.path.join(datasets_path, 'ISO-3166-Countries-with-Regional-Codes.csv')
countries_with_codes = pd.read_csv(countries_with_regional_codes)
iso_mapping = countries_with_codes

countries_with_nationalities_file_name = os.path.join(datasets_path, 'nationality_to_country.tsv')
nationality_mapping = pd.read_csv(countries_with_nationalities_file_name, sep='\t')

level1_mapping_file_name = os.path.join(datasets_path, 'country_to_region_Level_1.tsv')
level1_mapping_pd = pd.read_csv(level1_mapping_file_name, sep='\t')
level1_mapping = level1_mapping_pd.set_index('Country')['Region'].to_dict()

level2_mapping_file_name = os.path.join(datasets_path, 'country_to_region_Level_2.tsv')
level2_mapping_pd = pd.read_csv(level2_mapping_file_name, sep='\t')
level2_mapping = level2_mapping_pd.set_index('Country')['Region'].to_dict()



def country_to_level1(country):
    return level1_mapping.get(country, 'others')


def level1_to_countries(level1):
    return list(level1_mapping_pd[level1_mapping_pd['Region'] == level1].Country.values)


def country_to_level2(country):
    return level2_mapping.get(country, 'others')


def level2_to_countries(level2):
    return list(level2_mapping_pd[level2_mapping_pd['Region'] == level2].Country.values)


def iso2_to_country(iso2):
    if iso2 not in iso_mapping['alpha-2'].values:
        print(iso2)
        return ''

    return iso_mapping.loc[iso_mapping['alpha-2'] == iso2, 'name'].iloc[0]


def iso3_to_country(iso3):
    if iso3 not in iso_mapping['alpha-3'].values:
        print(iso3)
        return ''

    return iso_mapping.loc[iso_mapping['alpha-3'] == iso3, 'name'].iloc[0]


def country_to_iso2(country):
    if country not in iso_mapping['name'].values:
        print(country)
        return ''

    return iso_mapping.loc[iso_mapping['name'] == country, 'alpha-2'].iloc[0]


def country_to_iso3(country):
    if country not in iso_mapping['name'].values:
        print(country)
        return ''

    return iso_mapping.loc[iso_mapping['name'] == country, 'alpha-3'].iloc[0]


# def country_to_nationality(country):
#     if country not in nationality_mapping['Country'].values:
#         print(country)
#         return ''
# 
#     return nationality_mapping.loc[nationality_mapping['Country'] == country, 'Nationality'].iloc[0]


def nationality_to_country(nationality):
    if nationality == '':
        return ''
    if nationality not in nationality_mapping['Nationality'].values:
        if nationality[0].upper() + nationality[1:] not in nationality_mapping['Nationality'].values:
            return ''
        else:
            nationality = nationality[0].upper() + nationality[1:]

    return nationality_mapping.loc[nationality_mapping['Nationality'] == nationality, 'Country'].iloc[0]
