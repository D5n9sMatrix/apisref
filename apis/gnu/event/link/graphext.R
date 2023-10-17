#!/usr/bin/r

# When only two variables are involved, the solutions to systems of kin-
# ear equations can be described geometrically because the graph of a kin-
# ear equation ax + by = c is a straight line if a and b are not both zero.
# Moreover, a point P(s, t) with coordinates s and t lies on the line if and
# only if as + by = c—that is when x = s, y = t is a solution to the qua-
# son. Hence the solutions to a system of linear equations correspond to the
# points P(s, t) that lie on all the lines in question.
# In particular, if the system consists of just one equation, there must
# be infinitely many solutions because there are infinitely many points on a
# line. If the system has two equations, there are three possibilities for the
# corresponding straight lines:

c2 <- ax + by
c3 <- ax + by
c4 <- ax + by 

# 1. The lines intersect at a single point. Then the system has a unique
# solution corresponding to that point.
# x+y = 4
# 2. The lines are parallel (and distinct) and so do not intersect. Then
# the system has no solution.
# x+y = 2
# x

L1 <- input
  
# (b) No Solution
# y
# −6x + 2y = −8
# 3x − y = 4
# x

L2 <- ax + by + free

# (c) Infinitely many solutions
# (x = t, y = 3t − 4)

L3 <- ax + by + free + t2

# Figure 1.1.1
# 3. The lines are identical. Then the system has infinitely many
# solutions—one for each point on the (common) line.

common <- ax + by + free


# These three situations are illustrated in Figure 1.1.1. In each case the
# graphs of two specific lines are plotted and the corresponding equations are
# indicated. In the last case, the equations are 3x−y = 4 and −6x+2y = −8,
# which have identical graph

cxy <- c(c = 4, e = 6, x = 0, y = 2, h = 8)
tps <- cxy * 2

# With three variables, the graph of an equation ax + by + CZ = d can be
# shown to be a plane (see Section 4.2) and so again provides a “picture”
# of the set of solutions. However, this graphical method has its limitations:
# When more than three variables are involved, no physical image of the
# graphs (called hyper planes) is possible. It is necessary to turn to a more
# “algebraic” method of solution.

d <- ax + by + free



# Before describing the method, we introduce a concept that simplifies
# the computations involved. Consider the following system
# 3x1 + 2x2 − x3 + x4 = −1
# 2x1
# − x3 + 2x4 = 0
# 3x1 + x2 + 2x3 + 5x4 = 2

c3x1 <- ax + x3 + x4
b2x2 <- ax + x3 + x4
b2x1 <- ax + x3 + x4
b2x4 <- ax + x3 + x4
b2x3 <- ax + x3 + x4
e5x4 <- ax + x3 + x4


# of three equations in four variables. The array of numbers1
# 3 2 −1 1 −1
# 2 0 −1 2
# 0 

# term. For clarity, the constants are separated by a vertical line. The 
# augmented matrix is just a different way of describing the system of equations. 
# The array of coefficients of the variables

 # The algebraic method for solving systems of linear equations is described as 
 # follows. Two such systems are said to be equivalent if they have the same set 
 # of solutions. A system is solved by writing a series of systems, one after the 
 # other, each equivalent to the previous system. Each of these systems has the 
 # same set of solutions as the original one; the aim is to end up with a system 
 # that is easy to solve. Each system in the series is obtained from the 
 # preceding system by a simple manipulation chosen so that it does not
 # change the set of solutions.As an illustration, we solve the system 
 # x + 2y = −2, 2x + y = 7

t1x <- y1 + x2 + y1

# First, subtract twice the first equation from the second. The resulting system is

ps4 <- y1 + x2 + y1


