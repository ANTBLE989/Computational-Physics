class Graph(dic):
    def __init__(self,vs=[],es=[]):
        """Create a new graph. vs is a list of vertices;
        (es is a list of edges. """

        for v in vs:
            self.add_vertex(v)

        for e in es:
            self.add_edge(e)


    def add_vertex(self,v):
        """add (v) to the graph"""
        self[v] = {}

    def add_edge(self,e):
        """add (e) to the graph by adding an entry in both directions.
        If there is already an edge connecting these Vertices, the new
        edge replaces it."""

        v, w = e
        self[v][w] = e
        self[w][v] = e
