# Cube

The Rubik's Cube, invented by Ernő Rubik in 1974, is very fun and cool. The object of the puzzle is to, using only twists of the six faces, scramble the cube and then return it back to the starting configuration.

Inspecting the cube, it is clear that the stickers themselves do not move, rather each sticker is stuck to a specific face of a specific piece of the puzzle, often called a cubie. Thus it is the cubies, not the stickers, that move.

So a configuration of the Rubik's Cube can be thought of as a permutation of the cubies, as well as an assignment of an orientation to each cubie. The cubies themselves separate into three disjoint sets: the centers, edges, and vertices. There are six center cubies, twelve edge cubies, and eight corner cubies. The center cubies can be oriented in four different ways each, the edge cubies can be oriented in two different ways each, and the vertex cubies can be oriented in three different ways each. On a classic cube, the stickers on the center cubies are a solid color, and so they do not distinguish between orientations. However, some custom sticker sets allow for this.

Configurations can be composed, and the set of configurations admits a group structure via this composition, known as the Rubik's Cube Group. This package gives tools for examining configurations of the Rubik's Cube within this group.

An arbitrary configuration can be obtained by removing all the cubies from the puzzle and then putting them back in the specified way. However, it is not immediately clear whether every configuration can be obtained using only turns of the six faces. This special sort of configuration is called "legal," and the set of these configurations forms the Legal Cube Group.

In fact, these two groups are not the same. The (legal and illegal) Rubik's Cube Group has size 4^6 * 6! * 2^12 * 12! * 3^8 * 8! = 1530664174762362289520640000, whereas the Legal Cube Group has size (4^6 * 6! * 2^12 * 12! * 3^8 * 8!) / (6! * 2^3 * 3) = 88580102706155225088000. So legal configurations only make up 1/17280 of the total number.