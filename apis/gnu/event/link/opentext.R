#!/usr/bin/r

# All digital forms of access to our high-quality open texts are entirely FREE! 
# All content is reviewed for excellence and is wholly adaptable; custom editions 
# are produced by LyX for those adopting LyX assessment. Access to the original 
# source files is also open to any one!

free <- c(a = 1.5, b = 1.6, c = 1.7, d = 1.8, e = 1.9, f = 1.2, 
          g = 1.2, h = 1.4, i = 1.4, j = 1.5, l = 1.9, m = 1.2,
          n = 1.3, o = 1.4, p = 1.5, q = 1.6, r = 1.8, s = 1.2,
          t = 1.3, u = 1.4, v = 1.5, x = 1.7, z = 1.8, k = 1.2,
          w = 1.7, y = 1.4)

# values multiples to free groups github
x1 <- free * 1
x2 <- free * 2
x3 <- free * 3
x4 <- free * 4
x5 <- free * 4
x6 <- free * 6
x7 <- free * 7
x8 <- free * 8
x9 <- free * 9

# We have been developing superior online for- native assessment for more than 
# 15 years. Our questions are continuously adapted with the content and reviewed 
# for quality and sound pedagogy. To enhance learning, students receive immediate 
# personalized feedback. Student grade reports and performance statistics
# are also provided.

feedback1 <- message(x1, domain = NULL, appendLF = TRUE)
feedback2 <- message(x2, domain = NULL, appendLF = TRUE)
feedback3 <- message(x3, domain = NULL, appendLF = TRUE)
feedback4 <- message(x4, domain = NULL, appendLF = TRUE)
feedback5 <- message(x5, domain = NULL, appendLF = TRUE)
feedback6 <- message(x6, domain = NULL, appendLF = TRUE)
feedback7 <- message(x7, domain = NULL, appendLF = TRUE)
feedback8 <- message(x8, domain = NULL, appendLF = TRUE)
feedback9 <- message(x9, domain = NULL, appendLF = TRUE)


# Access to our in-house support team is available 7 days/week to provide prompt 
# resolution to both student and instructor inquiries. In addition, we work 
# one-on-one with instructors to provide a comprehensive system, customized
# for their course. This can include adapting the text, managing multiple 
# sections, and more!

weekdays(.leap.seconds)
months(.leap.seconds)
quarters(.leap.seconds)

## Show how easily you get month, day, year, day (of {month, week, yr}), ... :
## (remember to count from 0 (!): mon = 0..11, wday = 0..6,  etc !!)

##' Transform (Time-)Date vector  to  convenient data frame :
dt2df <- function(dt, dName = deparse(substitute(dt)), stringsAsFactors = FALSE) {
  DF <- as.data.frame(unclass(as.POSIXlt( dt )), stringsAsFactors=stringsAsFactors)
  `names<-`(cbind(dt, DF, deparse.level=0L), c(dName, names(DF)))
}
## e.g.,
dt2df(.leap.seconds)    # date+time
dt2df(Sys.Date() + 0:9) # date

##' Even simpler:  Date -> Matrix - dropping time info {sec,min,hour, isdst}
d2mat <- function(x) simplify2array(unclass(as.POSIXlt(x))[4:7])
## e.g.,
d2mat(seq(as.Date("2000-02-02"), by=1, length.out=30)) # has R 1.0.0's release date


## Julian Day Number (JDN, https://en.wikipedia.org/wiki/Julian_day)
## is the number of days since noon UTC on the first day of 4317 BC.
## in the Julian calendar.  To more recently, in
## 'Terrestrial Time' which differs from UTC by a few seconds
## See https://en.wikipedia.org/wiki/Terrestrial_Time
julian(Sys.Date(), -2440588) # from a day
floor(as.numeric(julian(Sys.time())) + 2440587.5) # from a date-time


# Forward
# Mathematics education at the beginning university level is closely tied to the 
# traditional publishers. In my opinion, it gives them too much control of both 
# cost and content. The main goal of most publishers is profit, and the result 
# has been a sales-driven business model as opposed to a pedagogical one. This 
# results in frequent new “editions” of textbooks motivated largely to reduce 
# the sale of used books rather than to update content quality. It also introduces 
# copyright restrictions which stifle the creation and use of new pedagogical 
# methods and materials. The overall result is high cost textbooks which may not 
# meet the evolving educational needs of instructors and students. To be fair, 
# publishers do try to produce material that reflects new trends. But their goal 
# is to sell books and not necessarily to create tools for student success in 
# mathematics education. Sadly, this has led to a model where the primary choice 
# for adapting to (or initiating) curriculum change is to find a different
# commercial textbook. My editor once said that the text that is adopted is often 
# everyone’s third choice.
# Of course instructors can produce their own lecture notes, and have done so for 
# years, but this remains an onerous task. The publishing industry arose from the 
# need to provide authors with copy-editing, editorial, and marketing services, 
# as well as extensive reviews of prospective customers to ascertain market trends 
# and content updates. These are necessary skills and services that the industry 
# continues to offer. Authors of open educational resources (OER) including 
# (but not limited to) textbooks and lecture notes, cannot afford this on their 
# own. But they do have two great advantages: The cost to students is significantly 
# lower, and open licenses return content control to instructors. Through editable 
# file formats and open licenses, OER can be developed, maintained, reviewed, 
# edited, and improved by a variety of contributors. Instructors can now respond 
# to curriculum change by revising and reordering material to create content that 
# meets the needs of their students. While editorial and quality control remain 
# daunting tasks, great strides have been made in addressing the issues of 
# accessibility, affordability and adaptability of the material. For the above 
# reasons I have decided to release my text under an open license, even though 
# it was published for many years through a traditional publisher.
# Supporting students and instructors in a typical classroom requires much more 
# than a textbook. Thus, while anyone is welcome to use and adapt my text at no 
# cost, I also decided to work closely with LyX Learning. With colleagues at 
# the University of Calgary, I helped create LyX almost 20 years ago. The
# original idea was to develop quality online assessment (with feedback) well 
# beyond the multiple-choice style then available. Now LyX also works to provide 
# and sustain open textbooks; working with authors, contributors, and reviewers 
# to ensure instructors need not sacrifice quality and rigor when switching to
# an open text. I believe this is the right direction for mathematical publishing 
# going forward, and look forward to being a part of how this new approach develops.
# W. Keith Nicholson, Author University of Calgary.

university <- matrix(data = 100, nrow = 500, ncol = 500, byrow = FALSE, 
                     dimnames = NULL)

repository <- matrix(data = 100, nrow = 500, ncol = 500, byrow = FALSE, 
                     dimnames = NULL)

opendrives <- matrix(data = 100, nrow = 500, ncol = 500, byrow = FALSE, 
                     dimnames = NULL)

# Preface
# 
# This textbook is an introduction to the ideas and techniques of linear algebra 
# for first- or second-year students with a working knowledge of high school 
# algebra. The contents have enough flexibility to present a traditional 
# introduction to the subject, or to allow for a more applied course. Chapters 
# 1–4 contain a one semester course for beginners whereas Chapters 5–9 contain a 
# second semester course (see the Suggested
#                                                                                        Course Outlines below). The text is primarily about real linear algebra with complex numbers being
# mentioned when appropriate (reviewed in Appendix A). Overall, the aim of the 
# text is to achieve a balance among computational skills, theory, and applications 
# of linear algebra. Calculus is not a prerequisite; places where it is mentioned 
# may be omitted.

# As a rule, students of linear algebra learn by studying examples and solving 
# problems. Accordingly, the book contains a variety of exercises (over 1200, 
# many with multiple parts), ordered as to their difficulty.
# In addition, more than 375 solved examples are included in the text, many of 
# which are computational in nature. The examples are also used to motivate 
# (and illustrate) concepts and theorems, carrying the student from concrete to 
# abstract. While the treatment is rigorous, proofs are presented at a level 
# appropriate to the student and may be omitted with no loss of continuity. As 
# a result, the book can be used to give a course that emphasizes computation 
# and examples, or to give a more theoretical treatment (some proofs are deferred 
# to the end of the Section).

# Linear Algebra has application to the natural sciences, engineering, management, 
# and the social sciences as well as mathematics. Consequently, 18 optional 
# “applications” sections are included in the text introducing topics as diverse 
# as electrical networks, economic models, Markov chains, linear recurrences,
# systems of differential equations, and linear codes over finite fields. 
# Additionally some applications (for example linear dynamical systems, and 
# directed graphs) are introduced in context. The applications sections appear 
# at the end of the relevant chapters to encourage students to browse.

preface <- pressure


# SUGGESTED COURSE OUTLINES

# This text includes the basis for a two-semester course in linear algebra.
# • Chapters 1–4 provide a standard one-semester course of 35 lectures, including 
# linear equations, matrix algebra, determinants, denationalization, and geometric 
# vectors, with applications as time permits. At Calgary, we cover Sections 
# 1.1–1.3, 2.1–2.6, 3.1–3.3, and 4.1–4.4 and the course is taken by all science 
# and engineering students in their first semester. Prerequisites include a working 
# knowledge of high school algebra (algebraic manipulations and some familiarity 
# with polynomials); calculus is not required.
# • Chapters 5–9 contain a second semester course including Rn , abstract vector 
# spaces, linear trans-
# formations (and their matrices), orthogonality, complex matrices (up to the 
# spectral theorem) and
# applications. There is more material here than can be covered in one semester, 
# and at Calgary we

# cover Sections 5.1–5.5, 6.1–6.4, 7.1–7.3, 8.1–8.7, and 9.1–9.3 with a couple 
# of applications as time
# permits.

# • Chapter 5 is a “bridging” chapter that introduces concepts like spanning, 
# independence, and basis in the concrete setting of Rn , before venturing into 
# the abstract in Chapter 6. The duplication is balanced by the value of reviewing 
# these notions, and it enables the student to focus in Chapter 6 on the new idea 
# of an abstract system. Moreover, Chapter 5 completes the discussion of rank and
# denationalization from earlier chapters, and includes a brief introduction to 
# orthogonality in Rn , which creates the possibility of a one-semester, 
# matrix-oriented course covering Chapter 1–5 for students not wanting to study 
# the abstract theory.

calm2 <- rnorm(x9, mean = 500, sd = 27)

# CHAPTER DEPENDENCIES

# The following chart suggests how the material introduced in each chapter draws 
# on concepts covered in certain earlier chapters. A solid arrow means that ready 
# assimilation of ideas and techniques presented in the later chapter depends on 
# familiarity with the earlier chapter. A broken arrow indicates that some
# reference to the earlier chapter is made but the chapter need not be covered.

ideas <- rnorm(x9, mean = 2000, sd = 27)

# HIGHLIGHTS OF THE TEXT

# • Two-stage definition of matrix multiplication. First, in Section 2.2 
# matrix-vector products are introduced naturally by viewing the left side of a 
# system of linear equations as a product. Second, matrix-matrix products are 
# defined in Section 2.3 by taking the columns of a product AB to be A times the 
# corresponding columns of B. This is motivated by viewing the matrix product as 
# composition of maps (see next item). This works well pedagogically and the usual 
# dot-product definition follows easily. As a bonus, the proof of associations 
# of matrix multiplication now takes four lines.


AB <- matrix(data = 110, nrow = 500, ncol = 500, byrow = FALSE, 
             dimnames = NULL)

# • Matrices as transformations. Matrix-column multiplications are viewed 
# (in Section 2.2) as trans-
# formations Rn → Rm . These maps are then used to describe simple geometric 
# reflections and rotations in R2 as well as systems of linear equations.

R2 <- frequency(free)

# • Early linear transformations. It has been said that vector spaces exist so 
# that linear transformations can act on them—consequently these maps are a 
# recurring theme in the text. Motivated by the matrix transformations introduced 
# earlier, linear transformations Rn → Rm are defined in Section 2.6, their
# standard matrices are derived, and they are then used to describe rotations, 
# reflections, projections, and other operators on R2 .

Rn <- R2
Rm <- R2

# • Early denationalization. As requested by engineers and scientists, this 
# important technique is presented in the first term using only determinants 
# and matrix inverses (before defining independence and dimension). Applications 
# to population growth and linear recurrences are given.

Erly1 <- cars

# • Early dynamical systems. These are introduced in Chapter 3, and lead 
# (via denationalization) to applications like the possible extinction of species. 
# Beginning students in science and engineering can relate to this because they 
# can see (often for the first time) the relevance of the subject to the real
# world.

Erly2 <- Erly1 + free

# • Bridging chapter. Chapter 5 lets students deal with tough concepts (like 
# independence, spanning, and basis) in the concrete setting of Rn before having 
# to cope with abstract vector spaces in Chapter 6.

Brid <- Erly1 + free / 2

# • Examples. The text contains over 375 worked examples, which present the main 
# techniques of the subject, illustrate the central ideas, and are keyed to the 
# exercises in each section.

Exmaples <- Erly1 + free / 4

# • Exercises. The text contains a variety of exercises (nearly 1175, many with 
# multiple parts), starting with computational problems and gradually progressing 
# to more theoretical exercises. Select solutions are available at the end of the 
# book or in the Student Solution Manual. There is a complete
# Solution Manual is available for instructors.

Exercies <- Erly2 + free / 2

# • Applications. There are optional applications at the end of most chapters 
# (see the list below).
# While some are presented in the course of the text, most appear at the end of 
# the relevant chapter to encourage students to browse.

Applic <- Erly1 + Erly2 + free / 2

# • Appendices. Because complex numbers are needed in the text, they are described 
# in Appendix A, which includes the polar form and roots of unity. Methods of proofs 
# are discussed in Appendix B, followed by mathematical induction in Appendix C. 
# A brief discussion of polynomials is included in Appendix D. All these topics 
# are presented at the high-school level.

Appendix <- Erly1 + Erly2 + free / 2

# • Self-Study. This text is self-contained and therefore is suitable for 
# self-study.

SelfStudy <- Erly1 + Erly2 + free / 2

# • Rigor. Proofs are presented as clearly as possible (some at the end of the 
# section), but they are optional and the instructor can choose how much he or 
# she wants to prove. However the proofs are there, so this text is more rigorous 
# than most. Linear algebra provides one of the better venues where students begin 
# to think logically and argue concisely. To this end, there are exercises that 
# ask the student to “show” some simple implication, and others that ask her or 
# him to either prove a given statement or give a counterexample. I personally 
# present a few proofs in the first semester course and more in the second (see 
# the Suggested Course Outlines).

Rigor <- Erly1 + Erly2 + free / 2

# • Major Theorems. Several major results are presented in the book. Examples: 
# Uniqueness of the reduced row-echelon form; the co factor expansion for 
# determinants; the Clayey-Hamilton theorem; the Jordan canonical form; Schur’s 
# theorem on block triangular form; the principal axes and spectral theorems; and 
# others. Proofs are included because the stronger students should at least be 
# aware of what is involved.

Major <- Erly1 + Erly2 + free / 2


# CHAPTER SUMMARIES

# Chapter 1: Systems of Linear Equations.

# A standard treatment of Gaussian elimination is given. The rank of a matrix is 
# introduced via the row echelon form, and solutions to a homogeneous system are 
# presented as linear combinations of basic solutions. Applications to network 
# flows, electrical networks, and chemical reactions are provided.

key1 <- floor(Erly1)
key2 <- floor(Erly2)

# Chapter 2: Matrix Algebra.

# After a traditional look at matrix addition, scalar multiplication, and transposition 
# in Section 2.1, matrix vector multiplication is introduced in Section 2.2 by viewing 
# the left side of a system of linear equations as the product Ax of the coefficient 
# matrix A with the column x of variables. The usual dot-product definition of a 
# matrix-vector multiplication follows. Section 2.2 ends by viewing an m × n matrix 
# A as a transformation Rn → Rm . This is illustrated for R2 → R2 by describing 
# reflection in the x axis, rotation of R2 through π2 , shears, and so on.

# In Section 2.3, the product of matrices A and B is defined by AB = Ab1 Ab2 · · · 
# Abn , where the bi are the columns of B. A routine computation shows that this 
# is the matrix of the transformation B followed by A. This observation is used 
# frequently throughout the book, and leads to simple, conceptual proofs of the 
# basic axioms of matrix algebra. Note that linearity is not required—all that 
# is needed is some basic properties of matrix-vector multiplication developed 
# in Section 2.2. Thus the usual arcane definition of matrix multiplication is 
# split into two well motivated parts, each an important aspect of matrix algebra.
# Of course, this has the pedagogical advantage that the conceptual power of 
# geometry can be invoked to illuminate and clarify algebraic techniques and 
# definitions.

# In Section 2.4 and 2.5 matrix inverses are characterized, their geometrical 
# meaning is explored, and block multiplication is introduced, emphasizing those 
# cases needed later in the book. Elementary matrices are discussed, and the Smith 
# normal form is derived. Then in Section 2.6, linear transformations Rn → Rm are 
# defined and shown to be matrix transformations. The matrices of reflections, 
# rotations, and projections in the plane are determined. Finally, matrix 
# multiplication is related to directed graphs, matrix LU-factorization is 
# introduced, and applications to economic models and Markov chains are 
# presented.

cure_bill <- matrix(data = 400, nrow = 500, ncol = 500, byrow = FALSE, 
                    dimnames = NULL)

obj_bill <- cure_bill / 2.4 

# expression dimly hack cure bill
dialog1 <- diag(x = 0, nrow = 500, ncol = 500, names = TRUE) 
objlog <- dialog1 + 22.27


# Chapter 3: Determinants and Denationalization.

# The co factor expansion is stated (proved by induction later) and used to define 
# determinants inductively and to deduce the basic rules. The product and adjudicate 
# theorems are proved. Then the denationalization algorithm is presented (motivated 
# by an example about the possible extinction of a species of birds). As requested 
# by our Engineering Faculty, this is done earlier than in most texts because it 
# requires only determinants and matrix inverses, avoiding any need for subspace, 
# independence and dimension. Convectors of a 2 × 2 matrix A are described 
# geometrically (using the A-in variance of lines through the origin). Denationalization 
# is then used to study discrete linear dynamical systems and to discuss applications 
# to linear recurrences and systems of differential equations. A brief discussion 
# of Google Page Rank is included.

suply <- free / 14

# Chapter 4: Vector Geometry.

# Vectors are presented intrinsically in terms of length and direction, and are 
# related to matrices via coordinates. Then vector operations are defined using 
# matrices and shown to be the same as the corresponding intrinsic definitions. 
# Next, dot products and projections are introduced to solve problems about lines 
# and planes. This leads to the cross product. Then matrix transformations are 
# introduced in R3 , matrices of projections and reflections are derived, and 
# areas and volumes are computed using determinants. The chapter closes with an 
# application to computer graphics.

edge <- free
R3 <- call("free")  
R1 <- call("edge")
x1 <- free / 2
x2 <- edge ^ 2

require(stats); require(graphics)
summary(freeny)
pairs(freeny, main = "freeny data")
# gives warning: freely$y has class "ts"

summary(fm1 <- lm(y ~ ., data = freeny))
opar <- par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0),
            mar = c(4.1, 4.1, 2.1, 1.1))
plot(fm1)
par(opar)


# Chapter 5: The Vector Space Rn .
# 
# Subspace, spanning, independence, and dimensions are introduced in the context 
# of Rn in the first two sections. Orthogonal bases are introduced and used to 
# derive the expansion theorem. The basic properties of rank are presented and used 
# to justify the definition given in Section 1.2. Then, after a rigorous study of
# denationalization, best approximation and least squares are discussed. The chapter 
# closes with an application to correlation and variance.
# This is a “bridging” chapter, easing the transition to abstract spaces. Concern 
# about duplication with Chapter 6 is mitigated by the fact that this is the most 
# difficult part of the course and many students welcome a repeat discussion of 
# concepts like independence and spanning, albeit in the abstract setting.
# In a different direction, Chapter 1–5 could serve as a solid introduction to 
# linear algebra for students not
# requiring abstract theory.

Rn2 <- time(free) 

# Chapter 6: Vector Spaces.

# Building on the work on Rn in Chapter 5, the basic theory of abstract finite 
# dimensional vector spaces is developed emphasizing new examples like matrices, 
# polynomials and functions. This is the first acquaintance most students have 
# had with an abstract system, so not having to deal with spanning, independence
# and dimension in the general context eases the transition to abstract thinking. 
# Applications to polynomials and to differential equations are included.

wisdom <- time(free)

# Chapter 7: Linear Transformations.

# General linear transformations are introduced, motivated by many examples from 
# geometry, matrix theory, and calculus. Then kernels and images are defined, the 
# dimension theorem is proved, and isomorphic are discussed. The chapter ends 
# with an application to linear recurrences. A proof is included that the order 
# of a differential equation (with constant coefficients) equals the dimension 
# of the space of solutions.

wizard <- time(free) 


# Chapter 8: Orthogonality.

# The study of orthogonality in Rn , begun in Chapter 5, is continued. Orthogonal 
# complements and projections are defined and used to study orthogonal denationalization. 
# This leads to the principal axes theorem, the Cholesterol factorization of a positive 
# definite matrix, QR-factorization, and to a discussion of the singular value 
# decomposition, the polar form, and the pseudo inverse. The theory is extended to Cn in
# Section 8.7 where Herminia and unitary matrices are discussed, culminating in 
# Schur’s theorem and the spectral theorem. A short proof of the Clayey-Hamilton 
# theorem is also presented. In Section 8.8 the field Z p of integers modulo p is 
# constructed informally for any prime p, and codes are discussed over any finite
# field. The chapter concludes with applications to quadratic forms, constrained 
# optimization, and statistical principal component analysis.
 

Q2 <- factor(free, levels = sort(free), labels = 1, exclude = NA, 
             ordered = 1, nmax = 2)
Q1 <- typeof(Q2)
lisp <- list(Q2)
holy <- str(Q2)
input <- character(length = 4L)
apple <- match.fun(dim, descend = TRUE)
.Primitive("dim")
dim(Q2)

# Chapter 9: Change of Basis.
# 
# The matrix of general linear transformation is defined and studied. In the case 
# of an operator, the relationship between basis changes and similarity is revealed. 
# This is illustrated by computing the matrix of a rotation about a line through the 
# origin in R3 . Finally, invariant subspace and direct sums are introduced, related 
# to similarity, and (as an example) used to show that every involution is similar to 
# a diagonal matrix

# with diagonal entries ±1.

R4 <- PlantGrowth

# Chapter 10: Inner Product Spaces.

# General inner products are introduced and distance, norms, and the Cauchy-Schwarz 
# inequality are discussed. The Gram-Schmidt algorithm is presented, projections are 
# defined and the approximation theorem is proved (with an application to Fourier 
# approximation). Finally, isometrics are characterized, and distance preserving 
# operators are shown to be composites of a translations and isometrics.

In <- function(x, y){
      if(x != y)
      {
         x <- print(x^y, sin(x))
         c(x)
      }
  if(y != x)
  {
    y <- print(y/x, sin(y))
    c(y)
  }
}

# Appendices

# In Appendix A, complex arithmetic is developed far enough to find nth roots. 
# In Appendix B, methods of proof are discussed, while Appendix C presents mathematical 
# induction. Finally, Appendix D describes the properties of polynomials in elementary 
# terms.

Apps <- Appendix

# LIST OF APPLICATIONS
# 
# • Network Flow (Section 1.4)
# • Electrical Networks (Section 1.5)
# • Chemical Reactions (Section 1.6)
# • Directed Graphs (in Section 2.3)
# • Input-Output Economic Models (Section 2.8)
# • Markov Chains (Section 2.9)
# • Polynomial Interpolation (in Section 3.2)
# • Population Growth (Examples 3.3.1 and 3.3.12, Section 3.3)
# • Google PageRank (in Section 3.3)
# • Linear Recurrences (Section 3.4; see also Section 7.5)
# • Systems of Differential Equations (Section 3.5)
# • Computer Graphics (Section 4.5)
# • Least Squares Approximation (in Section 5.6)
# • Correlation and Variance (Section 5.7)
# • Polynomials (Section 6.5)
# • Differential Equations (Section 6.6)
# • Linear Recurrences (Section 7.5)
# • Error Correcting Codes (Section 8.8)
# • Quadratic Forms (Section 8.9)
# • Constrained Optimization (Section 8.10)
# • Statistical Principal Component Analysis (Section 8.11)
# • Fourier Approximation (Section 10.5)

# ACKNOWLEDGMENTS

# Many colleagues have contributed to the development of this text over many years 
# of publication, and I specially thank the following instructors for their reviews 
# of the 7th edition:

# Robert Andre
# University of Waterloo
# Dietrich Burglar
# University of Toronto
# Dung M. Ha
# Roberson University
# Mark Solomon
# Grant Mace wan
# Fred Sabot
# Concordance University
# Edward Wang
# Wilfred Laurie
# Pete Sizzler
# Mount Royal University
# It is also a pleasure to recognize the contributions of several people. Discussions 
# with Thu Ding and Jean Springier have been invaluable and many of their suggestions 
# have been incorporated. Thanks are also due to Kristine Bauer and Clifton Cunningham 
# for several conversations about the new way to look at matrix multiplication. I also 
# wish to extend my thanks to Joanne Canape for being there when I had technical questions. 
# Thanks also go to Jason Nicholson for his help in various aspects of the book, particle-
# regularly the Solutions Manual. Finally, I want to thank my wife Kathleen, without whose 
# understanding and cooperation, this book would not exist.

# As we undertake this new publishing model with the text as an open educational resource, 
# I would also like to thank my previous publisher. The team who supported my text greatly 
# contributed to its success.
# Now that the text has an open license, we have a much more fluid and powerful mechanism 
# to incorporate comments and suggestions. The editorial group at LyX invites instructors 
# and students to contribute to the text, and also offers to provide adaptations of the 
# material for specific courses. Moreover the Latex
# source files are available to anyone wishing to do the adaptation and editorial work 
# themselves!
#   W. Keith Nicholson
# University of Calgary

LyX <- lattice::as.factorOrShingle(Q2, subset = TRUE, drop = FALSE)
Latex <- LyX


# 1. Systems of Linear Equations

# 1.1 Solutions and Elementary Operations

# Practical problems in many fields of study—such as biology, business, chemistry, 
# computer science, economics, electronics, engineering, physics and the social 
# sciences—can often be reduced to solving a system of linear equations. Linear 
# algebra arose from attempts to find systematic methods for solving these
# systems, so it is natural to begin this book by studying linear equations.
# If a, b, and c are real numbers, the graph of an equation of the form

ax <- free / 2.4
by <- free / 2.4

# blend members
c1 <- ax + by

# is a straight line (if a and b are not both zero), so such an equation is called 
# a linear equation in the variables x and y. However, it is often convenient to 
# write the variables as x1 , x2 , . . . , x , particularly when more than two 
# variables are involved. An equation of the form a1 x1 + a2 x2 + · · · + an 
# x = b

a1 <- ax + by + c1
x1 <- ax + by + c1
a2 <- ax + by + c1
x2 <- ax + by + c1
an <- ax + by + c1

# is called a linear equation in the n variables x1 , x2 , . . . , en . 
# Here a1 , a2 , . . . , an denote real numbers (called the coefficients of x1 , 
# x2 , . . . , en , respectively) and b is also a number (called the constant 
# term of the equation). A finite collection of linear equations in the variables 
# x1 , x2 , . . . , en is called a system of linear equations in these variables. 
# Hence, 2x1 − 3x2 + 5x3 = 7

bx1 <- a1 * x1 ^ an
cx2 <- a2 * x2 ^ an
ex3 <- a1 * x1 ^ an


# that is, if the equation is satisfied when the substitutions x1 = s1 , x2 = s2 
# , . . . , x = an are made. A sequence of numbers is called a solution to a 
# system of equations if it is a solution to every equation in the system.

# For example, x = −2, y = 5, z = 0 and x = 0, y = 4, z = −1 are both solutions 
# to the system
# x+y+ z=3
# 2x + y + 3z = 1

x1 <- bx1 + cx2 + ex3
x2 <- bx1 + cx2 + ex3
y1 <- bx1 + cx2 + ex3
z1 <- bx1 + cx2 + ex3

# A system may have no solution at all, or it may have a unique solution, or it 
# may have an infinite family of solutions. For instance, the system x + y = 2, 
# x + y = 3 has no solution because the sum of two numbers cannot be 2 and 3 
# simultaneously. A system that has no solution is called inconsistent; a system 
# with at least one solution is called consistent. The system in the following 
# example has infinitely many solutions.

f1 <- x1 + x2 + y1
f2 <- x1 + x2 + y1
f3 <- x1 + x2 + y1

# Show that, for arbitrary values of s and t,

x10 <- f1 + f2 - f3 
x12 <- f1 + f2 + f3
x13 <- f2

# is a solution to the system

bx2 <- f1 + f2 + f3 + x10
cx3 <- f1 + f2 + f3 - x10
x14 <- bx2 + cx3

# Solution. Simply substitute these values of x1 , x2 , x3 , and x4 in each equation.
# x1 − 2x2 + 3x3 + x4 = (t − s + 1) − 2(t + s + 2) + 3s + t = −3
# 2x1 − x2 + 3x3 − x4 = 2(t − s + 1) − (t + s + 2) + 3s − t = 0
# Because both equations are satisfied, it is a solution for all choices of s and t

x15 <- bx2 + cx3 + x4 + (x10 + x12 + x13) - x2 + (x10 + x12 + x13 ) + x3
x16 <- bx2 + cx3 + x4 + (x10 + x12 + x13) - x2 + (x10 + x12 + x13 ) + x3
x17 <- bx2 + cx3 + x4 + (x10 + x12 + x13) - x2 + (x10 + x12 + x13 ) + x3

# The quantities s and t in Example 1.1.1 are called parameters, and the set of 
# solutions, described in this way, is said to be given in parametric form and 
# is called the general solution to the system. It turns out that the solutions 
# to every system of equations (if there are solutions) can be given in parametric 
# form (that is, the variables x1 , x2 , . . . are given in terms of new independent 
# variables s, t, etc.). The following example shows how this happens in the simplest 
# systems where only one equation is present.

x18 <- x15 + x16 - x17

# Describe all solutions to 3x − y + 2z = 6 in parametric form.
cx <- y1 + bx1

# Solution. Solving the equation for y in terms of x and z, we get y = 3x + 2z 
# − 6. If s and t are arbitrary then, setting x = s, z = t, we get solutions
s2 <- cx
t2 <- s2 + s2 - cx

# In this example we look at the data which was looked at in 
# Example 7.1 of Davidson and Brinkley (1997)

### R code from vignette source 'splines.Rnw'

###################################################
### code chunk number 1: splines.Rnw:21-25
###################################################
options(continue="  ", width=60)
options(SweaveHooks=list(fig=function() par(mar=c(4.1, 4.1, .3, 1.1))))
pdf.options(pointsize=8) #text in graph about the same as regular text
options(contrasts=c("contr.treatment", "contr.poly")) #reset default


###################################################
### code chunk number 2: mplot
###################################################
getOption("SweaveHooks")[["fig"]]()
require(survival)
mfit <- coxph(Surv(futime, death) ~ sex + pspline(age, df=4), data=mgus)
mfit
termplot(mfit, term=2, se=TRUE, col.term=1, col.se=1)


###################################################
### code chunk number 3: mplot2
###################################################
ptemp <- termplot(mfit, se=TRUE, plot=FALSE)
attributes(ptemp)
ptemp$age[1:4,]


###################################################
### code chunk number 4: mplot3
###################################################
getOption("SweaveHooks")[["fig"]]()
ageterm <- ptemp$age  # this will be a data frame
center <- with(ageterm, y[x==50])
ytemp <- ageterm$y + outer(ageterm$se, c(0, -1.96, 1.96), '*')
matplot(ageterm$x, exp(ytemp - center), log='y',
        type='l', lty=c(1,2,2), col=1, 
        xlab="Age at diagnosis", ylab="Relative death rate")


###################################################
### code chunk number 5: hgb
###################################################
getOption("SweaveHooks")[["fig"]]()
fit <- coxph(Surv(futime, death) ~ age + pspline(hgb, 4), mgus2)
fit
termplot(fit, se=TRUE, term=2, col.term=1, col.se=1,
         xlab="Hemoglobin level")


###################################################
### code chunk number 6: df
###################################################
getOption("SweaveHooks")[["fig"]]()
termplot(fit, se=TRUE, col.term=1, col.se=1, term=2,
         xlab="Hemoglobin level", ylim=c(-.4, 1.3))
df <- c(3, 2.5, 2)
for (i in 1:3) {
  tfit <- coxph(Surv(futime, death) ~ age + 
                  pspline(hgb, df[i], nterm=8), mgus2)
  temp <- termplot(tfit, se=FALSE, plot=FALSE, term=2)
  lines(temp$hgb$x, temp$hgb$y, col=i+1, lwd=2)
}
legend(14, 1, paste("df=", c(4, df)), lty=1, col=1:4, lwd=2)


###################################################
### code chunk number 7: fit2a
###################################################
getOption("SweaveHooks")[["fig"]]()
fit2a <- coxph(Surv(futime, death) ~ age + pspline(hgb, 2.5, nterm=8), mgus2)
coef(fit2a)
plot(1:10, coef(fit2a)[-1])


###################################################
### code chunk number 8: fit2b
###################################################
getOption("SweaveHooks")[["fig"]]()
temp <- c(1:7, 8,8,8)
fit2b <- coxph(Surv(futime, death) ~ age + 
                 pspline(hgb, 2.5, nterm=8, combine=temp), 
               data= mgus2)
temp2 <- c(1:6, 7,7,7,7)
fit2c <- coxph(Surv(futime, death) ~ age + 
                 pspline(hgb, 2.5, nterm=8, combine=temp2), 
               data= mgus2)
matplot(1:10, cbind(coef(fit2a)[-1], coef(fit2b)[temp+1], 
                    coef(fit2c)[temp2+1]), type='b', pch='abc',
        xlab="Term", ylab="Pspline coef")


###################################################
### code chunk number 9: fit1
###################################################
getOption("SweaveHooks")[["fig"]]()
options(show.signif.stars=FALSE) # display intelligence
fit1 <- coxph(Surv(futime, death) ~ sex + pspline(age, 3), data=flchain)
fit1
termplot(fit1, term=2, se=TRUE, col.term=1, col.se=1,
         ylab="log hazard")


###################################################
### code chunk number 10: nfit
###################################################
options(show.signif.stars=FALSE)  # display statistical intellegence
require(splines, quietly=TRUE)
nfit1 <- coxph(Surv(futime, death) ~ sex + age, flchain)
nfit2 <- coxph(Surv(futime, death) ~ sex + ns(age, df=3), flchain)
nfit3 <- coxph(Surv(futime, death) ~ sex * ns(age, df=3), flchain)
anova(nfit1, nfit2, nfit3)


###################################################
### code chunk number 11: nfit2
###################################################
getOption("SweaveHooks")[["fig"]]()
pdata <- expand.grid(age= 50:99, sex=c("F", "M"))
pdata[1:5,]

ypred <- predict(nfit3, newdata=pdata, se=TRUE)
yy <- ypred$fit + outer(ypred$se, c(0, -1.96, 1.96), '*')
matplot(50:99, exp(matrix(yy, ncol=6)), type='l', lty=c(1,1,2,2,2,2),
        lwd=2, col=1:2, log='y',
        xlab="Age", ylab="Relative risk")
legend(55, 20, c("Female", "Male"), lty=1, lwd=2, col=1:2, bty='n')
abline(h=1)


###################################################
### code chunk number 12: fit2
###################################################
agem <- with(flchain, ifelse(sex=="M", age, 60))
agef <- with(flchain, ifelse(sex=="F", age, 60))
fit2 <- coxph(Surv(futime, death) ~ sex + pspline(agef, df=3)
              + pspline(agem, df=3), data=flchain)
anova(fit2, fit1)


###################################################
### code chunk number 13: plot2
###################################################
getOption("SweaveHooks")[["fig"]]()
# predictions
pdata2 <- pdata
pdata2$agem <- with(pdata2, ifelse(sex=="M", age, 60))
pdata2$agef <- with(pdata2, ifelse(sex=="F", age, 60))
ypred2 <- predict(fit2, pdata2, se=TRUE)
yy <- ypred2$fit + outer(ypred2$se, c(0, -1.96, 1.96), '*')

# reference
refdata <- data.frame(sex='F', agef=65, agem=60)
ref <- predict(fit2, newdata=refdata, type="lp")

# plot
matplot(50:99, exp(matrix(yy-ref, ncol=6)), type='l', lty=c(1,1,2,2,2,2),
        lwd=2, col=1:2, log='y',
        xlab="Age", ylab="Relative risk")
legend(55, 20, c("Female", "Male"), lty=1, lwd=2, col=1:2, bty='n')
abline(h=1)

