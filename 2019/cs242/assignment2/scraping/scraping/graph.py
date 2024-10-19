import os
from scraping.scraping.node import *
import unittest


class graphTest(unittest.TestCase):
    def testActorList(self):
        g = graph()
        self.assertTrue(
            g.read_actor('/home/max/junior/cs242/sp19_cs242_assignment2/scraping/scraping/actor') is not None)

    def testFilmList(self):
        g = graph()
        self.assertTrue(g.read_film('/home/max/junior/cs242/sp19_cs242_assignment2/scraping/scraping/film') is not None)

    def testBuild(self):
        g = graph()
        self.assertTrue(
            g.read_actor('/home/max/junior/cs242/sp19_cs242_assignment2/scraping/scraping/actor') is not None)
        self.assertTrue(g.read_film('/home/max/junior/cs242/sp19_cs242_assignment2/scraping/scraping/film') is not None)

        g.build_graph()
        self.assertTrue(g.table is not None)

    def testCall(self):
        g = graph()
        self.assertTrue(
            g.read_actor('/home/max/junior/cs242/sp19_cs242_assignment2/scraping/scraping/actor') is not None)
        self.assertTrue(g.read_film('/home/max/junior/cs242/sp19_cs242_assignment2/scraping/scraping/film') is not None)
        print(g.get_box_office('50_to_1'))
        print(g.get_actor_by_film('50_to_1'))
        print(g.get_author_by_year(80))
        print(g.get_movie_by_year(1999))
        print(g.get_top_actor_by_age(3))
        print(g.get_top_actor_by_value(3))


class graph():
    actor = []
    film = []
    table = {}

    def __init__(self):
        self.actor = []
        self.film = []
        self.table = {}

    def read_actor(self, directory):
        result = []
        for filename in os.listdir(directory):
            n = node()
            result.append(n.load_json(os.path.join(directory, filename)))
        print(result)
        self.actor = list(filter(lambda a: a is not None, result))
        return self.actor

    def read_film(self, directory):
        result = []
        for filename in os.listdir(directory):
            n = node()
            result.append(n.load_json(os.path.join(directory, filename)))
        print(result)
        self.film = list(filter(lambda a: a is not None, result))
        return self.film

    def build_graph(self):
        result = []
        for actor in self.actor:
            dependency = actor.dependency
            new_dep = []
            new_val = 0
            for dep in dependency:
                dep_film = next((x for x in self.film if x.link == dep), None)
                if dep_film is None:
                    continue
                else:
                    new_val += float(dep_film.value)
                    new_dep.append(dep_film.name)
            result.append(node(value=new_val, dependency=new_dep, year=actor.year, link=actor.link, name=actor.name))
        self.actor = result

        result = []
        for film in self.film:
            dependency = film.dependency
            new_dep = []
            for dep in dependency:
                dep_actor = next((x for x in self.actor if x.link == dep), None)
                if dep_actor is None:
                    continue
                else:
                    new_dep.append(dep_actor.name)
            result.append(node(value=film.value, dependency=new_dep, year=film.year, link=film.link, name=film.name))
        self.film = result

        actor_dict = dict((actor.name, actor) for actor in self.actor)

        film_dict = dict((film.name, film) for film in self.film)

        self.table = {**actor_dict, **film_dict}

    def get_box_office(self, film_name):
        result = list(filter(lambda film: film.name == film_name, self.film))
        if len(result) > 0:
            return result[0]
        return None

    def get_actor_by_film(self, film_name):
        film = next((x for x in self.film if x.name == film_name), None)
        if film is None:
            return []
        return film.dependency

    def get_movie_by_year(self, year):
        return list(filter(lambda film: film.year == year, self.film))

    def get_author_by_year(self, year):
        return list(filter(lambda actor: actor.year == year, self.actor))

    def get_top_actor_by_value(self, n):
        try:
            return self.actor.sort(key=lambda x: x.value, reverse=True)[:n]
        except Exception as e:
            print(e)
            return []

    def get_top_actor_by_age(self, n):
        try:
            return self.actor.sort(key=lambda x: x.year, reverse=True)
        except Exception as e:
            print(e)
            return []


if __name__ == '__main__':
    unittest.main()
