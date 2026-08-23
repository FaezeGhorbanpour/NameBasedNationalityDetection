import argparse
import re
import ftfy
import unicodedata

def remove_links_and_urls(string):
    pattern = r'http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+'
    modified_string = re.sub(pattern, '', string)
    return modified_string
def remove_word(string, word):
    pattern = rf'(?i)\b{re.escape(word)}\b'
    modified_string = re.sub(pattern, '', string)
    return modified_string

def remove_word_and_following_words(string, word):
    pattern = rf'(?i)\b{re.escape(word)}\b.*'
    modified_string = re.sub(pattern, '', string)
    return modified_string
def process_name(name):

    # get rid of nicknames in quotes and remove content between forward slashes, usually name pronunciations
    name = re.sub(r'\([^()]*\)', '', name)
    name = re.sub(r'\([^()]*', '', name)
    name = re.sub(r'\([^()]*', '', name)
    name = re.sub(r'（[^()]*）', '', name)
    name = re.sub('\".*\"', " ", name)
    name = re.sub(' /.*/ ', " ", name)




    # almost always a name previously seen
    name = name.split(".com")[1] if ".com" in name else name
    name = name.split("-lrb-")[0]
    name = name.split("-lsb-")[0]
    name = name.split(',')[0]
    name = name.split(':')[0]
    name = name.split('s/o')[0]
    name = name.split('a/l')[0]
    name = name.split('/')[0]
    name = name.split('&')[0]


    # remove periods
    name = re.sub("\.", " ", name)
    name = re.sub(",", " ", name)
    name = name.replace("'", ' ')
    name = name.replace("`", ' ')
    name = name.replace('"', ' ')
    name = name.replace("”", ' ')
    name = name.replace("“", ' ')
    name = name.replace(";", ' ')
    name = name.replace("-", ' ')
    name = name.replace("_", ' ')

    # remove non-name parts
    name = remove_word_and_following_words(name, "is")
    name = remove_word_and_following_words(name, "are")
    name = remove_word_and_following_words(name, "who")
    name = remove_word_and_following_words(name, "of")
    name = remove_word_and_following_words(name, "or")
    name = remove_word_and_following_words(name, "the")
    name = remove_word_and_following_words(name, "born")
    name = remove_word_and_following_words(name, "in")
    name = remove_word_and_following_words(name, "also")
    name = remove_word_and_following_words(name, "sometimes")
    name = remove_word_and_following_words(name, "commonly")
    name = remove_word_and_following_words(name, "best")
    name = remove_word_and_following_words(name, "better")
    name = remove_word_and_following_words(name, "known")
    name = remove_word_and_following_words(name, "unknown")
    name = remove_word_and_following_words(name, "full")
    name = remove_word_and_following_words(name, "pronunciation")
    name = remove_word_and_following_words(name, "\ U200d")

    # TODO: look for more suffixes
    # remove common suffixes
    name = remove_word(name, "Jr")
    name = remove_word(name, "Sr")
    name = remove_word(name, "I")
    name = remove_word(name, "II")
    name = remove_word(name, "III")
    name = remove_word(name, "IV")
    name = remove_word(name, "V")
    name = remove_word(name, "VI")
    name = remove_word(name, "VII")
    name = remove_word(name, "VIII")
    name = remove_word(name, "IX")
    name = remove_word(name, "X")
    name = remove_word(name, "XI")
    name = remove_word(name, "XII")
    name = remove_word(name, "XIII")
    name = remove_word(name, "CBE")
    name = remove_word(name, "OBE")
    name = remove_word(name, "MBE")

    # remove common prefixes
    name = remove_word(name, "Doctor")
    name = remove_word(name, "Univ")
    name = remove_word(name, "Ing")
    name = remove_word(name, "Professor")
    name = remove_word(name, "Sir")
    name = remove_word(name, "Mr")
    name = remove_word(name, "Msc")
    name = remove_word(name, "Mis")
    name = remove_word(name, "Bsc")
    name = remove_word(name, "Dame")
    name = remove_word(name, "Lord")
    name = remove_word(name, "Dr")
    name = remove_word(name, "Prof")
    name = remove_word(name, "Princess")
    name = remove_word(name, "Prince")
    name = remove_word(name, "Countess")
    name = remove_word(name, "Count")


    name = ftfy.fix_text(name)
    name = unicodedata.normalize('NFKD', name).encode('ASCII', 'ignore').decode('utf-8')

    # throw out any name containing one or more of these characters
    bad_chars = ['!', '"', '%', '&', '\(', '\)', '\/', ':', ';', '=', '\?', '@', '\]', '\[', '_', '`', '{', '\|', '}',
                 '~', '[0-9]']
    for char in bad_chars:
        if re.search(char, name) is not None:
            return ''

    return " ".join(name.split()).lower().strip()





if __name__ == '__main__':
    some_names = [
    'Gazi HÃ¼seyin Pasha', 'JoaquÃ­n JosÃ© MorÃ³n Hidalgo', 'Luis Javier ArgÃ¼ello GarcÃ­a'
        'Louis Combes (homme politique)', 'Hans Andersson (arkeolog)', 'Abuda (footballer', 'Mark Clark (baseball)',
                  'Brian MacLeod (U.S. musician)',
                  "    lao chew yen（ 刘秋雁 ）",
"André LiohnAndré Liohn in Misuratah Libya April 2012BornAndré L. Garcia de Oliveira9 December 1974Botucatu", "R SathyanarayanaBorn9 May 1927Mysore",
        "John Stuebner commonly known by his ring name ...", "unknown . name assigned by beazley after a the..."
                  ]

    for i in some_names:
        j = process_name(i)
        print(i, '->', j)
        print()



