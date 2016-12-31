# khscrape.py

import urllib
import json
import contextlib
from pprint import pprint

url = 'http://api.kuroganehammer.com/api/smashattributetypes/25/characterattributes'

with contextlib.closing(urllib.urlopen(url)) as x :
    js = json.load(x)

# view json structure with
len(js)
len(js[0])
pprint(js[0])

# access character name and weight values with
js[0]['characterName']
js[0]['rawValues'][0]['value']