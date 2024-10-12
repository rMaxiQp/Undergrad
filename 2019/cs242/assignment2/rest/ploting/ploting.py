import matplotlib.pyplot as plt
import networkx as nx

from itertools import groupby, permutations

from jsongraph import JSONGraph


def count_actor_connection(g):
    actors = g.get_actor()
    movies = g.get_movie()
    movie_keys = movies.keys()
    result = []
    # traverse every actor
    for actor in actors:
        actor_set = set()
        actor_info = actors[actor].get_node()
        actor_movies = actor_info[-1]

        # traverse every movie that this actor is involved in
        for actor_movie in actor_movies:
            if actor_movie in movie_keys:
                movie_info = movies[actor_movie].get_node()
                actor_set.update(movie_info[-1])
        actors[actor].set_connections(actor_set)
        result.append((actor, len(actor_set)))
    return result


def group_actor_age(g):
    actors = g.get_actor()
    result = []

    # traverse every actor
    for actor in actors:
        actor_info = actors[actor].get_node()
        actor_gross = actor_info[1]
        actor_age = actor_info[2]
        result.append((actor_age, actor_gross))
    return result


def group_movie_year(g):
    movies = g.get_movie()
    result = []

    # traverse every actor
    for movie in movies:
        movie_info = movies[movie].get_node()
        movie_box_office = movie_info[1]
        movie_year = movie_info[2]
        result.append((movie_year, movie_box_office))
    return result


def plot_hub_actor(g):
    actor_counts = count_actor_connection(g)
    top_10 = sorted(actor_counts, key=lambda count: count[1])[-10::][::-1]

    labels = [x[0] for x in top_10]
    counts = [x[1] for x in top_10]

    fig, ax1 = plt.subplots()
    ax1.set_title('Hub Actor')
    ax1.bar(labels, counts)
    fig.autofmt_xdate()

    plt.show()


def plot_actor_age(g):
    actor_ages = group_actor_age(g)
    actor_ages_group = groupby(sorted(actor_ages, key=lambda i: i[0]), lambda i: i[0])
    actor_age_labels, actor_age_gross = [], []

    for actor_age, actor_gross in actor_ages_group:
        acc = 0
        if actor_age > 0:
            actor_age_labels.append(actor_age)
            actor_age_gross.append(sum(i[1] for i in actor_gross))
            acc += 1
        if acc != 0:
            actor_age_gross[-1] /= acc

    plt.title('Actor Age vs Actor Gross')
    plt.plot(actor_age_labels, actor_age_gross)
    plt.show()


def plot_movie_year(g):
    movies_year = group_movie_year(g)
    movies_year_group = groupby(sorted(movies_year, key=lambda i: i[0]), lambda i: i[0])
    movies_year_labels, movies_year_box = [], []
    for key, group in movies_year_group:
        acc = 0
        if key > 1900:
            movies_year_labels.append(key)
            movies_year_box.append(sum(i[1] for i in group))
            acc += 1
        if acc != 0:
            movies_year_box[-1] /= acc

    plt.title('Movie Year vs Movie Box Office')
    plt.plot(movies_year_labels, movies_year_box)
    plt.show()


def plot_network(g):
    G = nx.Graph()
    G.add_edges_from(g.get_edge())
    plt.subplots(figsize=(100,100))
    nx.draw(G)
    plt.show()


# a simple demo to show a similar plot
def plot_demo_network():
    edge = []
    for movie in [u'Death Weekend', u'Rock the Kasbah', u'The Young Philadelphians']:
        for actor in [u'Michael Wincott', u'Nick Nolte']:
            edge.append((actor, movie))

    k = nx.Graph()
    k.add_edges_from(edge)
    nx.draw_shell(k, with_labels=True)
    plt.show()


if __name__ == '__main__':
    graph = JSONGraph()
    graph.load_json('../data.json')

    plot_actor_age(graph)
    plot_hub_actor(graph)
    plot_movie_year(graph)
    plot_demo_network()
    # plot_network(graph)
