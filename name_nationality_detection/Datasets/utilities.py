import time

import country_converter as coco
from SPARQLWrapper import SPARQLWrapper, JSON


def get_person_nationality_with_dbpedia_link(link):
    link = link[1:-1]
    for type in ['dbp', 'dbo']:
        tags = ['nationality', 'birthPlace', 'citizenship']
        print(link)
        for tag in tags:
            # Create a SPARQLWrapper object
            try:
                sparql = SPARQLWrapper("http://dbpedia.org/sparql")

                # Create the SPARQL query
                query = f"""
                SELECT DISTINCT ?info WHERE {{
                    {{
                        <{link}> {type}:{tag} ?info .
                    }}
                }}
                """

                # Set the SPARQL query and return format
                sparql.setQuery(query)
                sparql.setReturnFormat(JSON)

                # Execute the SPARQL query
                results = sparql.query().convert()

                bindings = results["results"]["bindings"]

                # Retrieve the first available information (nationality, birth place, or citizenship)
                if bindings:
                    print(bindings[0]["info"]["value"])
                    return bindings[0]["info"]["value"]
            except:
                import time
                time.sleep(5)
                continue
    print('nothing')
    return ''




country_standardized_names = dict()


def country_name_standardizer(text):
    if text.strip() == '':
        return ''

    if text in country_standardized_names:
        return country_standardized_names[text]

    standard_name = coco.convert(names=text, to='name_short')

    if isinstance(standard_name, list) or standard_name == 'not found':
        standard_name = ''

    country_standardized_names[text] = standard_name

    return standard_name


if __name__ == '__main__':
    # Example usage with the given DBpedia links
    links = ['http://dbpedia.org/resource/Jos%C3%A9_Manuel_Mart%C3%ADn',
    'http://dbpedia.org/resource/Jeff_Gordon',
    'http://dbpedia.org/resource/Simon_de_Waal',
    "http://dbpedia.org/resource/Tom_Sawyer,_Baron_Sawyer",
    'http://dbpedia.org/resource/Herv%C3%A9_This',
    'http://dbpedia.org/resource/Martin_Roberts_(rugby_union)',
    'http://dbpedia.org/resource/Irene_Obera',
    'http://dbpedia.org/resource/Andy_Kessler_(author)',
    'http://dbpedia.org/resource/Jenny_Logan',
    'http://dbpedia.org/resource/Roland_Muldoon',
    ]

    for link in links:
        info = get_person_nationality_with_dbpedia_link(link)
        print(f"For {link}, the retrieved information is: {info}")

