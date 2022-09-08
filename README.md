# TriangleProblem

Challenge: determine the number of triangles in the given graph

![drawsvg](https://user-images.githubusercontent.com/10800878/189033572-09428f3d-e8f1-42b8-8beb-4cfa9220ac63.svg)

This code currently runs through a brute-force approach, a breadth-first search through all possible sets of three of the major lines composing the graph, eliminating ones which do not intersect in the form of a triangle within the given area.

I model the graph as a list of major lines, labeling each intersection with an alphabetic index from a-x, in (left -> right) -> (top -> bottom) order.

I use a few type synonyms for ease of mental manipulation: Vertex = Char, Line = List of Vertex (therefore string), Graph = List of Lines

I use a data type called LineTrace which essentially allows us to track the state of the process of choosing a potential triangle.

The algorithm iterates through the graph (line by line), choosing a set of three lines and determining whether it forms a valid triangle inside the given graph. Once we 
have three lines which form a valid triangle, we find the intersections and return the resulting set of three vertices.
