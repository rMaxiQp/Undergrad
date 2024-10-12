import json

from node import *


class JSONGraph:
    def __init__(self):
        self.actor = {}
        self.movie = {}
        self.edge = []

    # @param String path of the file
    # read data from file, create actor and movie nodes, then generate edges
    def load_json(self, path):

        actor_dict, movie_dict = {}, {}

        with open(path) as f:
            data = json.load(f)
            actors, movies = data[0], data[1]

            for actor in actors:
                actor_dict[actor] = ActorNode(actors[actor])

            for movie in movies:
                movie_dict[movie] = MovieNode(movies[movie])

        self.actor = actor_dict
        self.movie = movie_dict
        self.generate_edge()

    # generate edges via actor nodes and movie nodes
    def generate_edge(self):
        edges = []
        actor_names = self.actor.keys()
        movie_names = self.movie.keys()

        for actor, actor_node in self.actor.items():
            actor_info = actor_node.get_node()
            for movie in actor_info[-1]:
                if movie in movie_names:
                    edges.append((actor, movie))

        for movie, movie_node in self.movie.items():
            movie_info = movie_node.get_node()
            for actor in movie_info[-1]:
                if (actor in actor_names) and ((actor, movie) not in edges):
                    edges.append((movie, actor))

        self.edge = edges

    # getter
    def get_edge(self):
        return self.edge

    def get_actor(self):
        return self.actor

    def get_movie(self):
        return self.movie
