import json
from pprint import pprint

class node():
    def __init__(self, *args, **kwargs):
        self.value = kwargs.get("value", 0)
        self.year = kwargs.get("year", 1990)
        self.link = kwargs.get('link', '/')
        self.name = kwargs.get('name', '')
        self.dependency = kwargs.get("dependency", [])

    def __str__(self):
        return str(self.name) + " with " + str(self.link)

    def __repr__(self):
        return str(self.name) + " with " + str(self.link)

    def add_dependency(self, dependency):
        self.dependency = dependency

    def to_json(self, outfile):
        return json.dump(self.__dict__, outfile)

    def load_json(self, filename):
        with open(filename) as f:
            try:
                obj = json.load(f)
                self.value = obj['value'] or 0
                self.year = obj['year'] or 1990
                self.dependency = obj['dependency'] or []
                self.link = obj['link'] or '/'
                self.name = obj['name'] or ''
            except Exception as e:
                print(e)
                return None
        return self
