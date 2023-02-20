First we prepare necessary definitions and rigorously define a sofa.

> **Definition \[area\].** For any measurable set
> \\(X \subseteq \mathbb{R}^2\\), denote its *area* as \\(|X|\\), the
> Lebesgue measure of \\(X\\). ^def-area

> **Definition \[shape\].** A *shape* is a nonempty compact subset of
> \\(\mathbb{R}^2\\). ^def-shape

> **Definition \[hallway\].** The standard *hallway* \\(L\\) of unit
> width that rotates 90 degrees is the union \\(L = L_H \cup L_V\\) of
> its *horizontal side* \\(L_H = (-\infty, 1\] \times \[0, 1\]\\) and
> *vertical side* \\(L_V = \[0, 1\] \times (-\infty, 1\]\\)
> respectively. ^def-hallway

To define a sofa, we need to describe its rigid motion. A rigid motion
is described by a continuous path of rigid transformations.

> **Definition \[rigid-motion\].** For any
> \\(\theta \in S^1 \simeq \mathbb{R}/2\pi\mathbb{Z}\\), define the map
> \\(R\_\theta : \mathbb{R}^2 \to \mathbb{R}^2\\) as the rotation of
> \\(\mathbb{R}^2\\) around the origin by a counterclockwise angle of
> \\(\theta\\). For any \\(\mathbf{v} \in \mathbb{R}^2\\), define the
> map \\(T\_\mathbf{v} : \mathbb{R}^2 \to \mathbb{R}^2\\) as the
> translation of \\(\mathbb{R}^2\\) by the vector \\(\mathbf{v}\\).
>
> A *proper rigid transformation* is the composition
> \\(T_v \circ R\_\theta\\) for any choice of \\(\theta \in S^1\\) and
> \\(\mathbf{v} \in \mathbb{R}^2\\). The *set of all proper rigid
> transformation* \\(\mathcal{T}\\) can be identified with
> \\(S^1 \times \mathbb{R}^2\\) by the bijective map
> \\((\theta, \mathbf{v}) \mapsto T\_\mathbf{v} \circ R\_\theta\\) and
> inherits the topology of \\(S^1 \times \mathbb{R}^2\\).
>
> A *rigid motion* is a continuous path
> \\(f\_\* : \[a, b\] \to \mathcal{T}\\) in the set of all proper rigid
> transformations \\(\mathcal{T}\\). We put the time
> \\(t \in \[a, b\]\\) as a subscript of \\(f\\) to denote the
> transformation \\(f_t\\) at time \\(t\\). ^def-rigid-motion

Now we are ready to define what is a sofa.

> **Definition \[sofa\].** A shape \\(S\\) is a *sofa* if it is
> connected and there exists a rigid motion
> \\(f\_- : \[a, b\] \to \mathcal{T}\\) such that the followings hold.
>
> - \\(f_a(S) \subseteq L_H\\). That is, the sofa starts in the
>   horizontal side \\(L_V\\) of the hallway.
> - \\(f_t(S) \subseteq L\\) for all \\(t \in \[a, b\]\\). That is, the
>   sofa stays inside the hallway \\(L\\) at all the time.
> - \\(f_b(S) \subseteq L_V\\). That is, the sofa ends in the vertical
>   side \\(L_H\\) of the hallway. Call such a rigid motion a *movement*
>   of the sofa \\(S\\) ^def-sofa

> **Remark \[sofa-def\]**. One may question why we restrict a sofa to a
> shape. That is, a bounded and closed set. The rationale behind it is
> this:
>
> - If an arbitrary set \\(S\\) which might not be closed admits the
>   same movement \\(f\\) inside the hallway \\(L\\), then its closure
>   \\(\overline{S}\\) also admits the same movement \\(f\\) as the
>   hallway parts \\(L, L_H, L_V\\) are closed. So it does not hurt
>   generality to assume that \\(S\\) is closed.
> - Even if there is an unbounded sofa \\(S\\), its area can be
>   approximated as much as we want by the intersection of \\(S\\) with
>   a disk of sufficiently large radius \\(R\\) centered at the origin.
>   So the supremum of the area of sofa \\(S\\) is the same even if we
>   restrict to bounded \\(S\\). We will eventually prove that the
>   Gerver's sofa \\(S\_\text{Gerver}\\), which is bounded, attains the
>   supremum of the sofa area. So this suffice to cover even the
>   unbounded case if desired. ^rem-sofa-def

> **Remark \[sofa-connected\].** We depend on the connectedness of the
> sofa throughly in this document. ^rem-sofa-connected

Given a movement \\(f\\) of a sofa \\(S\\), we can define a *rotation
angle* of \\(S\\).

> **Definition \[rotation-angle\].** For any rigid motion
> \\(f : \[a, b\] \to \mathcal{T}\\), its *rotation angle* \\(\omega\\)
> is defined as the following. As the topology space \\(\mathcal{T}\\)
> is identified with \\(S^1 \times \mathbb{R}^2\\) by the bijective map
> \\((\theta, \mathbf{v}) \mapsto T\_\mathbf{v} \circ R\_\theta\\), the
> continuous path \\(f_t\\) of \\(\mathcal{T}\\) is equal to
> \\(f_t = T\_{\mathbf{v}(t)} \circ R\_{\theta(t)}\\) for continuous
> functions \\(\theta(t) \in S^1\\) and
> \\(\mathbf{v}(t) \in \mathbb{R}^2\\) of \\(t\\). Let the map
> \\(\hat{\theta} : \[a, b\] \to \mathbb{R}\\) be any lift of the
> continous map \\(\theta : \[a, b\] \to S^1\\) with respect to the
> covering \\(\mathbb{R} \to S^1 \simeq \mathbb{R}/2\pi\mathbb{Z}\\).
> Then the difference \\(\hat{\theta}(b) - \hat{\theta}(a)\\) is
> independent of the choice of \\(\hat{\theta}\\). Define \\(\omega\\)
> as \\(\omega = \hat{\theta}(a) - \hat{\theta}(b)\\) (note that the
> sign is reversed).
>
> For any sofa \\(S\\), say that \\(\omega\\) is a *rotation angle* of
> \\(S\\) if there is a movement \\(f\\) of \\(S\\) with the rotation
> angle \\(\omega\\). ^def-rotation-angle

Note that this definition measures the angle a sofa \\(S\\) moves
*clockwise* from \\(L_H\\) to \\(L_V\\) inside \\(L\\).

TODO: the definition of rotation angle is more verbose and
counterintuitive than what it actually means.

TODO: mention that we can assume \\(\omega \in \[0, \pi/2\]\\) and
provide sufficient argument. Also mention existing works that narrows
the value of \\(\omega\\).

