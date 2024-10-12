class Node():
    def __init__(self):
        self.name = None

    def get_node(self):
        pass

    def __eq__(self, other):
        return self.name == other.name

    def __hash__(self):
        return hash(self.name)


class ActorNode(Node):
    def __init__(self, obj):
        super().__init__()
        self.gross = obj['total_gross']
        self.age = obj['age']
        self.name = obj['name']
        self.movies = obj['movies']
        self.actor_set = set()

    def get_node(self):
        return ['Actor', self.gross, self.age, self.movies]

    def set_connections(self, actor_set):
        self.actor_set = actor_set


class MovieNode(Node):
    def __init__(self, obj):
        super().__init__()
        self.year = obj['year']
        self.name = obj['name']
        self.box_office = obj['box_office']
        self.actors = obj['actors']

    def get_node(self):
        return ['Movie', self.box_office, self.year, self.actors]
