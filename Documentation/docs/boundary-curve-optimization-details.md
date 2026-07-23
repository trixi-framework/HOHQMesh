# Technical Details of Boundary Curve Optimization

Herein provides details on the assumptions, mathematics, and algorithms
behind the error based, adaptive high order mesh generation option in HOHQMesh.

## Introduction: The Accurate Approximation of Boundary Curves

The goal is to have an accurate approximation of a boundary curve when
written out to the mesh file. Formally, the problem to solve is the following:

!!! note "Boundary curve optimization"
    Find the best piecewise polynomial approximation of degree $N$ of a
    curve $\vec{\Gamma}(t)$, $t\in[0,1]$ such that the error is less than a
    user-provided tolerance.

In the figure below, $\vec{\Gamma}(t)$ (blue solid line) is the curve to be approximated
and the polynomial approximations $\vec{P}_i$ (black dashed line) that span intervals
$h_i = t_i - t_{i-1}$ along the curve (red solid lines).

<figure id="fig:Goal" data-latex-placement="htbp">
<img src="https://github.com/user-attachments/assets/320981d4-11c2-4687-bd68-c246585241f4" style="width:6in" />
<figcaption>The piecewise approximation of a curve by
polynomials</figcaption>
</figure>

HOHQMesh actually solves two problems:

1. Once a mesh with number of segments $K$ along a curve and segment
   lengths $h_{k}$ is generated, it can find and write to the mesh file
   the best piecewise polynomial approximation $\vec{P}_{k}$ , and

2. Before generating a mesh, it can find the best number of segments,
   $K$, intervals $h_{k}$ and polynomials $\vec{P}_{k}$ so the error is
   less than a user acceptable tolerance to use to choose the local
   element size.

To begin, we need to define "best". To include both the boundary error
and boundary derivative errors, we define "best" as meaning the
approximation with the smallest error in the $\mathbb H^0$ or $\mathbb H^{1}$ norm as
chosen by the user, where, for $K$ segments,

$$
\mathrm{error} = \sum_{i=1}^K \left |\left| \vec{\Gamma}-\vec{P}_i\right|\right|_{\mathbb H^k}^2, \quad k = 0,1.
$$

The $\mathbb H^1$ norm includes the error in the
position and the error in the derivative. Over an interval
$[t_{i-1},t_i]$, that norm is

$$
\left |\left| \vec{\Gamma}-\vec{P}_i\right|\right|_{\mathbb H^1\left(t_{i-1},t_i \right)}^2 = \int\limits_{t_{i-1}}^{t_i} |\vec{\Gamma}-\vec{P}_i|^2 + |\vec{\Gamma}'-\vec{P}'_i|^{2} \,\mathrm{d}\ell
$$

The $\mathbb L^{2} = \mathbb H^{0}$ norm drops the derivative term.

There are also constraints on the approximation. The first three ensure
a watertight mesh.

1.  The first polynomial must match the curve at the left end point.

2.  The last polynomial must match the curve at the right end point.

3.  At the interior ends of the segments the polynomials must match.

4.  Additionally it may be desirable that the derivatives of the
    polynomials match up to some order $p$ at the interior ends of the
    segments.

## Global $\mathbb H^1$ Optimization of a Curve

We proceed now to show how to compute the best approximation in steps of
increasing complexity. We derive the equations for the $\mathbb H^{1}$
norm; the $\mathbb L^{2}$ norm just drops the terms associated with the
derivatives. We first derive the systems of equations to solve without
constraints, then add endpoint constraints, and finally, add smoothness
constraints to ensure that the approximations are continuous up to a
user specified derivative.

### Optimization without constraints

We start with a single polynomial to approximate the curve, without the
endpoint constraints. Therefore, we want to find a polynomial

$$
\vec{P}= \sum_{j=0}^N \vec{b}_j \phi_j(s(t)), \quad \vec{b}_j = [b^x_j \; b_j^y]^T
$$

that best approximates a curve
$\vec{\Gamma}(t) = X(t)\hat x + Y(t)\hat y$ in the $\mathbb H^1$ norm.
Here, $\phi_j$ can be any basis, but we will assume it is an orthogonal
polynomial, e.g. Legendre parametrized in arc length along the curve.
The choice of an orthogonal basis will keep the condition number of the
system to be solved low. The parameter $t\in[t_{k-1},t_{k}]$ and
$s(t)\in [-1,1]$ are related through the affine transformation

$$
t = t_{k-1}\frac{(1-s)}{2} + t_{k}\frac{(s+1)}{2}.
$$

We start with the unconstrained minimization of the
$\mathbb H^1$ error, which is: *find $b_j$ that minimizes the functional*

$$
\mathcal Q = \frac{1}{2}\int\limits_{t_{k-1}}^{t_{k}} \left| \vec{\Gamma}(t) - \sum_{j=0}^N \vec{b}_j \phi_j(s(t)) \right|^2 \,\mathrm{d}t+ \frac{1}{2}\int\limits_{t_{k-1}}^{t_{k}} \left|\vec{\Gamma}'(t) - \sum_{j=0}^N \vec{b}_j \phi'_j(s(t))\frac{ds}{dt}\right|^2 \,\mathrm{d}t.
$$

Let's convert the integral with $\mathrm{d}t = \frac{1}{2}h\,\mathrm{d}s$,
where $h = t_{k} - t_{k-1}$. Then $ds/dt = 2/h$. Written out,

$$
\begin{split}
\mathcal Q &= \mathcal Q_x + \mathcal Q_y \\& =
\frac{1}{2}\frac{h}{2}\int\limits \,\mathrm{d}s +  \frac{1}{2}\frac{h}{2}\int\limits_{-1}^1 \left( X'(t(s)) - \sum_{j=0}^N  b^x_j \phi'_j(s)\frac{ds}{dt}\right)^{\!2} \,\mathrm{d}s
\\&
+\frac{1}{2}\frac{h}{2}\int\limits_{-1}^1 \left(  Y(t(s)) - \sum_{j=0}^N  b^y_j \phi_j(s) \right)^{\!2} \,\mathrm{d}s +  \frac{1}{2}\frac{h}{2}\int\limits_{-1}^1 \left( Y'(t(s)) - \sum_{j=0}^N  b^y_j \phi'_j(t) \frac{ds}{dt}\right)^{\!2} \,\mathrm{d}s.
\end{split}
$$

The objective function is minimized where the gradient
with respect to the coefficients $(\vec{b}^x,\vec{b}^y)$ is zero,
with some abuse of notation. With respect to the first set,

$$
\begin{aligned}
\frac{\partial \mathcal Q}{\partial b^x_i} = 0 &= -\frac{h}{2}\left\{\int\limits_{-1}^1 \left(  X(s) - \sum_{j=0}^N  b^x_j \phi_j(s) \right)\phi_i \,\mathrm{d}s \right.\\[0.1cm]
& \qquad\qquad \left. + \int\limits_{-1}^1 \left( X'(s) - \sum_{j=0}^N  b^x_j \phi'_j(s)\frac{ds}{dt}\right)\phi'_i\frac{ds}{dt} \,\mathrm{d}s\right\}.
\end{aligned}
$$

Gathering knowns and unknowns,

$$
\frac{h}{2}\sum_{j=0}^N  b^x_j \int\limits_{-1}^1\left( \phi_j\phi_i +\phi'_j\phi'_i\left(\frac{ds}{dt}\right)^{\!2}\right) \,\mathrm{d}s = \frac{h}{2}\int\limits_{-1}^1\left( X\phi_i +X'\phi'_i\frac{ds}{dt}\right) \,\mathrm{d}s
$$

This can be written in matrix-vector form as

$$
\underline{M} \mathbf b^x = \mathbf r
$$

where

$$
\frac{h}{2}M_{ij} = \int\limits_{-1}^1\left( \phi_j\phi_i +\phi'_j\phi'_i\left(\frac{ds}{dt}\right)^2\right) \,\mathrm{d}s, \quad r_i = \frac{h}{2}\int\limits_{-1}^1\left( X\phi_i +X'\phi'_i\frac{ds}{dt}\right) \,\mathrm{d}s
$$

and

$$
\mathbf b = \left[ b_0\; b_1\; \ldots b_N\right]^T.
$$

The same formula holds true for the coefficients, $\mathbf b^y$.

!!! note "Remark 1"
    The integrals in the matrix $\underline{\smash{M}}$ can be computed exactly
    with a Gauss quadrature of order $N$. The first part is just
    orthogonality, so is $2/(2j+1)$ along the diagonal for the Legendre
    basis. A recursion formula is used to compute the derivatives of the
    Legendre polynomials and from that the integrals.

### $\mathbb H^1$ Optimization with endpoint constraints

To ensure the corners of an element are fixed by the geometry, we add
two constraints. Based on the previous analysis, we can do the $x$ and
$y$ components separately. So let $Z$ be one of $X$ or $Y$ and $b$ refer
to either $b^x$ or $b^y$. Then for either the $x$ or $y$ components,

$$
\sum_{j=0}^N  b_j \phi_j(-1) =  Z(t_0),\quad \sum_{j=0}^N  b_j \phi_j(1) =  Z(t_{max}).
$$

This can also be written in matrix-vector form

$$
\underline{C}^T\mathbf b = \mathbf d,
$$

where is the $2\times (N+1)$ matrix

$$
\underline{C}^T = \left[\begin{array}{cccc}\phi_0(-1) & \phi_1(-1) & \ldots & \phi_N(-1) \\\phi_0(1) & \phi_1(1) & \ldots & \phi_N(1)\end{array}\right]\equiv \left[\begin{array}{c}\mathbf C^T_- \\\mathbf C^T_+\end{array}\right]
$$

and

$$
\mathbf d = [Z(t_0)\;Z(t_{max})]^T.
$$

The constraints are imposed with a penalty term and a Lagrange multiplier by adding

$$
\pmb{\lambda}^T\left( \mathbf d - \underline{C}^T\mathbf b\right)
$$

to $\mathcal Q$ . Then we define the new objective function with the linear constraints as

$$
\begin{aligned}
\mathcal Q_c &= \frac{1}{2}\frac{h}{2}\int\limits_{-1}^1 \left|  Z(s) - \sum_{j=0}^N  b_j \phi_j(s) \right|^2 \,\mathrm{d}s+  \frac{1}{2}\frac{h}{2}\int\limits_{-1}^1 \left| Z'(s) - \sum_{j=0}^N  b_j \phi'_j(s)\frac{ds}{dt}\right|^2 \,\mathrm{d}s\\[0.1cm]
&\qquad + \sum_{n=1}^2 \lambda_n\left( d_n - \sum_{j=0}^N C^T_{nj} b_j\right) ,
\end{aligned}
$$

which we then minimize with respect to the $b_i$ and $\pmb{\lambda}$. For the first,

$$
\frac{\partial \mathcal Q_c}{\partial b_i} =\frac{\partial \mathcal Q}{\partial b_i}  - \sum_{n=1}^2 \lambda_n C^T_{ni} = 0.
$$

For the second,

$$
\frac{\partial \mathcal Q_c}{\partial \lambda_i} = d_i - \sum_{j=0}^N C^T_{ij} b_j = 0
$$

In matrix-vector form, this becomes

$$
\left[\begin{array}{cc}\frac{h}{2}\underline{M} & \underline{C} \\\underline{C}^T & 0\end{array}\right]
\left[\begin{array}{c}\mathbf b \\\pmb{\lambda}\end{array}\right]=\left[\begin{array}{c}\frac{h}{2}\mathbf r \\\mathbf d \end{array}\right].
$$

The matrices $\underline{M}$ and $\underline{C}$ do not change between the $x$ and $y$ components.
Only $\mathbf r$ and $\mathbf d$ do.

### Multiple segment $\mathbb H^1$ optimization

Now the curve $\vec{\Gamma}$ is subdivided into $K$ subintervals,
$[t_{k-1},t_k]$, with length $h^{(k)}=t_k - t_{k-1}$ where
$t\in[t_0,t_{max}]$ and the best polynomial is sought. With $s\in[-1,1]$
we can write $t = t_{k-1} + \frac{1}{2}(s+1)h^{(k)}$. To ensure a
watertight mesh we set continuity conditions at the endpoints. We will
step through several cases here. As above, we can consider the $x$ and
$y$ components separately.

We can have three constraint conditions applied:

1.  Specified value at endpoints,

    $$
    \sum_{j=0}^N  b^{(1)}_j \phi_j(-1) =  Z(t_0),\quad \sum_{j=0}^N  b^{(K)}_j \phi_j(1) =  Z(t_{max})
    $$

2.  Continuity at interior interfaces,

    $$
    \sum_{j=0}^N   b^{(k-1)}_j \phi_j(1) =  \sum_{j=0}^N  b^{(k)}_j \phi_j(-1),
    $$

    where $k = 2, 3, \ldots, K$

3.  Smoothness at interfaces,

    $$
    \sum_{j=0}^N   b^{(k-1)}_j \frac{d^m \phi_j(1)}{ds^m}\left(\frac{ds}{dt}\right)^{\! m} =  \sum_{j=0}^N  b^{(k)}_j \frac{d^m \phi_j(-1)}{ds^m}\left(\frac{ds}{dt}\right)^{\! m},
    $$

    where $m = 0, 1,\ldots, p < N$ and $k = 2, 3, \ldots, K$

4.  Periodic conditions at endpoints,

    $$
    \sum_{j=0}^N   b^{(K)}_j \frac{d^m \phi_j(1)}{ds^m}\left(\frac{ds}{dt}\right)^{\! m} =  \sum_{j=0}^N  b^{(1)}_j \frac{d^m \phi_j(-1)}{ds^m}\left(\frac{ds}{dt}\right)^{\! m},
    $$

    where $m = 0, 1,\ldots, p < N$.

As above, let us define the two following vectors

$$
\mathbf C_- = \left[\phi_0(-1)\; \phi_1(-1)\;\ldots\;\phi_N(-1) \right]^T,\quad \mathbf C_+ = \left[\phi_0(1)\; \phi_1(1)\;\ldots\;\phi_N(1) \right]^T.
$$

Then the fixed value constraints are

$$
\mathbf C^T_-\cdot \mathbf b^{(1)}=Z(t_0),\quad \mathbf C^T_+\cdot \mathbf b^{(K)}=Z(t_{max}).
$$

and continuity constraints are

$$
\mathbf C^T_-\cdot \mathbf b^{(k)}- \mathbf C^T_+\cdot \mathbf b^{(k-1)}=0, \quad k = 2,3,\ldots,K.
$$

Finally, periodic constraints are

$$
\mathbf C^T_-\cdot \mathbf b^{(1)}- \mathbf C^T_+\cdot \mathbf b^{(K)}=0.
$$

For any of these, we can write the constraints as a matrix-vector system,

$$
\underline{C}^T\mathbf b = \mathbf d,
$$

where the global state vector is

$$
\mathbf b = \left[  \mathbf b^{(1),T}\;\mathbf b^{(2),T}\;\ldots \;\mathbf b^{(K),T}\right]^T.
$$

The matrix $\underline{C}$ and $\mathbf d$ will depend on the constraints chosen. Options are

1.  Fixed endpoint values:

    $$
    \underline{C}^T = \left[\begin{array}{ccccc}\mathbf C_-^T & \mathbf 0 & \mathbf 0 & \ldots & \mathbf 0 \\-\mathbf C_+^T & \mathbf C_-^T & \mathbf 0 &  &  \\0 & -\mathbf C_+^T & \mathbf C_-^T &   &   \\\vdots & \ddots & \ddots &   &   \\\mathbf 0 & \mathbf 0 & \mathbf 0 & \ldots & \mathbf C_+^T\end{array}\right],\quad \mathbf d = \left[\begin{array}{c}Z(t_0) \\\mathbf 0 \\\mathbf 0 \\\vdots \\Z(t_{max})\end{array}\right].
    $$

2.  Periodic constraints:

    $$
    \underline{C}^T = \left[\begin{array}{ccccc}
    \mathbf C_-^T & \mathbf 0 & \mathbf 0 & \ldots &-\mathbf C_+^T \\
    -\mathbf C_+^T & \mathbf C_-^T & \mathbf 0 &  &  \\
    0 & -\mathbf C_+^T & \mathbf C_-^T &   &   \\
    \vdots & \ddots & \ddots &   &   \\
    -\mathbf C_-^T & \mathbf 0 & \mathbf 0 & \ldots & \mathbf C_+^T\end{array}\right]
    ,\quad \mathbf d = \mathbf 0.
    $$

    Note that the matrix $\underline{C}^T$ is singular, since the first and
    last rows are the same to within a factor of $-1$. Since this shows up
    as giving the same two Lagrange multipliers, we just delete the last row

    $$
    \underline{C}^T = \left[\begin{array}{ccccc}
    \mathbf C_-^T & \mathbf 0 & \mathbf 0 & \ldots &-\mathbf C_+^T \\
    -\mathbf C_+^T & \mathbf C_-^T & \mathbf 0 &  &  \\
    0 & -\mathbf C_+^T & \mathbf C_-^T &   &   \\
    \vdots & \ddots & \ddots &   &   \\
    \mathbf 0 & \mathbf 0 &  \ldots& -\mathbf C_+^T& \mathbf C_-^T\end{array}\right]
    ,\quad \mathbf d = \mathbf 0.
    $$

### Fixed element segments and polynomial order

Given a subdivision of the parametrization, we find the best piecewise
polynomial approximation by splitting the objective function into
pieces. The unconstrained objective function is

$$
\begin{split}
\mathcal Q_z =\sum_{k=1}^{K}\mathcal Q_z^{(k)}
=\frac{1}{2}\sum_{k=1}^{K}\frac{h^{(k)}}{2} \int\limits_{-1}^{1}&\left\{ \left|  Z\left(t_{k-1} + \frac{1}{2}(s+1)h^{(k)} \right) - \sum_{j=0}^N  b^{(k)}_j \phi_j\left(s\right) \right|^2 \right.
\\&
\left. + \left| Z'\left( t_{k-1} + \frac{1}{2}(s+1)h^{(k)}\right) - \sum_{j=0}^N  b^{(k)}_j \phi'_j\left(s\right)\frac{ds}{dt}\right|^2\right\} \,\mathrm{d}s.
\end{split}
$$

Taking the gradient with respect to $b^{(n)}_i$, gives a local system of equations in component form,

$$
\frac{h^{(n)}}{2}\sum_{j=0}^N M_{ij} b_j = \frac{h^{(n)}}{2}  r_i
$$

where, now,

$$
r_i = \int\limits_{-1}^1\left( Z\left( t_{k-1} + \frac{1}{2}(s+1)h^{(k)}\right)\phi_i +Z'\left( t_{k-1} + \frac{1}{2}(s+1)h^{(k)}\right)\phi'_i\frac{ds}{dt}\right) \,\mathrm{d}s.
$$

The constraints are in canonical form, so we write with the Lagrange multiplier

$$
\mathcal Q_c =  \mathcal Q  +\pmb{\lambda}^T\left(\mathbf d - \underline{C}^T\mathbf b\right),
$$

again, in canonical form, which leads to the system

$$
\left[\begin{array}{ccc|ccc}\frac{h^{(1)}}{2}\underline{M}^{(1)} &  &   &   &   &   \\
 & \ddots  &   &   & \underline{C} &   \\  &   &\frac{h^{(K)}}{2} \underline{M}^{(K)} &   &   &   \\
 \hline
   &   &   &   &   &   \\ &  \underline{C}^T &   &   & \underline{0} &
      \\  &   &   &   &   &  \end{array}\right]
      \left[\begin{array}{c} \mathbf b^{(1)} \\\vdots \\\mathbf b^{(K)} \\\hline
      \lambda^{(0)} \\ \vdots \\\lambda^{(K)}\end{array}\right]
      = \left[\begin{array}{c}\frac{h^{(1)}}{2}  \mathbf r^{(1)} \\\vdots \\\frac{h^{(k)}}{2} \mathbf r^{(K)} \\[0.1cm]
      \hline
      Z(t_0) \\\mathbf 0 \\ Z(t_{max}) \end{array}\right]
      \tag{1}
$$

to determine the polynomial coefficients when fixed endpoints are specified.

For periodic constraints the lower part of the right hand side vector is
zero for all components and there is one fewer Lagrange multiplier.

Each of the $\underline{M}$ matrices is of size $(N+1)\times (N+1)$. The
matrix $\underline{C}$ is of size $K(N+1)\times (K+1)$, while
$\underline{C}^T$ is $(K+1)\times K(N+1)$. Therefore, the full system
matrix is of size $K(N+1) + (K+1)\times K(N+1) + (K+1)$. The upper left
block is of size $K(N+1)\times K(N+1)$.

!!! note "Remark 2"
    An approximation like this has a name when applied to
    data and the $\mathbb L^{2}$ norm is used: Least squares spline.

### Higher order constraints

In some applications, especially transition to turbulence problems, the
boundary must be very smooth, up to even the fourth derivative. In that
case, we want to specify additional smoothness constraints, e.g.

$$
\sum_{j=0}^N   b^{(k-1)}_j \frac{d^m \phi_j(1)}{ds^m}\left(\frac{ds}{dt}\right)^{\! m} =  \sum_{j=0}^N  b^{(k)}_j \frac{d^m \phi_j(-1)}{ds^m}\left(\frac{ds}{dt}\right)^{\! m}
$$

with $m = 0, 1,\ldots, p < N$, $k = 2, 3, \ldots, K$, and
where, presumably, $p<N$. This will increase the size of the constraint matrix,
with entries that depend on the derivatives of the basis functions.

As above, let us define the two following vectors

$$
\begin{aligned}
\mathbf C^m_- &= \left[\phi^{(m)}_0(-1)\; \phi^{(m)}_1(-1)\;\ldots\;\phi^{(m)}_N(-1) \right]^T,\\[0.1cm]
\mathbf C^m_+ &= \left[\phi^{(m)}_0(1)\; \phi^{(m)}_1(1)\;\ldots\;\phi^{(m)}_N(1) \right]^T.
\end{aligned}
$$

Then the continuity constraints are

$$
\left(\frac{ds}{dt}^{(k)}\right)^{\! m}\mathbf C^{m,T}_-\cdot \mathbf b^{(k)}- \left(\frac{ds}{dt}^{(k-1)}\right)^{\! m}\mathbf C^{m,T}_+\cdot \mathbf b^{(k-1)}=0,
$$

with $k = 2,3,\ldots,K$, $m = 0,1,\ldots$, and $p < N$.
Note, we have explicitly noted the fact that the transformation
Jacobian changes from segment to segment. Therefore, we
need to construct an array of derivative quantities
$\left\{\phi_j^{(m)}\right\}_{j=0,m=0}^{N,p}$ evaluated at $\pm 1$.

Then the constraint matrix for fixed endpoints is

$$
\underline{C}^T = \left[\begin{array}{ccccc}
\mathbf C_-^T & \mathbf 0 & \mathbf 0 & \ldots & \mathbf 0 \\
-\mathbf C_+^T & \mathbf C_-^T & \mathbf 0 &  &  \\
-\sigma\mathbf C_+^{1,T} & \sigma\mathbf C_-^{1,T} & \mathbf 0 &  &  \\
\vdots & \vdots & \mathbf 0 &   &   \\
-\sigma^p\mathbf C_+^{p,T} & \sigma^p\mathbf C_-^{p,T} & \mathbf 0 &  &  \\
\mathbf 0 & -\mathbf C_+^T & \mathbf C_-^T &   &   \\
\mathbf 0 & -\sigma\mathbf C_+^{1,T} & \sigma\mathbf C_-^{1,T} &   &   \\
\mathbf 0 & \vdots & \vdots &   &   \\
\mathbf 0 & -\sigma^p\mathbf C_+^{p,T} & \sigma^p\mathbf C_-^{p,T} &   &   \\
\vdots & \ddots & \ddots &   &   \\
\mathbf 0 & \mathbf 0 & \mathbf 0 & \ldots & \mathbf C_+^T\end{array}\right],
\quad \mathbf d = \left[\begin{array}{c}Z(t_0) \\\mathbf 0  \\\mathbf 0 \\\vdots  \\\mathbf 0\\\mathbf 0 \\\mathbf 0  \\\vdots \\\mathbf 0 \\\vdots \\Z(t_{max})\end{array}\right]
$$

Since we are using the Legendre basis,

$$
\begin{gathered}
L'_{k} = (2k-1)L_{k-1} + L'_{k-2}\\
L'_k({\pm 1}) = \frac{1}{2}(\pm 1)^{k+1}k(k+1),
\end{gathered}
$$

from the well-known formulas. Hence for any other derivative,

$$
L^{(m)}_{k} = (2k-1)L^{(m-1)}_{k-1} + L^{(m)}_{k-2}.
$$

For starting conditions, we know that

$$
L^{(m)}_k = 0,\quad k \le m-1.
$$

Then we compute the derivatives recursively using the following algorithm.
<figure id="fig:alg1" data-latex-placement="htbp">
<img src="https://github.com/user-attachments/assets/4dcaf571-4990-421d-b288-7de3e1af218f" style="width:7in" />
</figure>

### Implementation

This section is implemented in the module
`ConstrainedMultiH1Optimization` contained in the
file
[`Source/BoundaryOptimization/ConstrainedMultiH1Optimization.f90`](https://github.com/trixi-framework/HOHQMesh/blob/BoundaryOptimization/Source/BoundaryOptimization/CurveOptimization.f90).

- The linear systems from Eq. (1) are solved with an LU factorization,
  implemented in `LUFactorization` and `ForwardBackwardSolve` defined in that file.

- Setup is done in the procedure `SUBROUTINE ConstructCMH1O`, whereas the actual optimization
  is performed in `SUBROUTINE Optimize`.

### Practical Notes

Some observations to pay attention to:

- For a good approximation, the number of smoothness constraints needs
  to be smaller than the polynomial order. One cannot have fourth order
  smoothness with a piecewise second order polynomial. The user should
  ensure there are enough modal coefficients to account for continuity
  and accuracy.

- The systems in Eq. (1) appear to be ill-conditioned for high
  $p$, limiting how much smoothness one can get. It appears that only
  first or second order continuity is possible this way. Some sort of
  preconditioning or other amelioration should be studied.

## Optimal intervals for fixed polynomial order

As part of the adaptive mesh generation procedure, HOHQMesh finds the
optimal subdivision for a given polynomial order before meshing to get
the best element distribution. In this case, the number of subdivisions
is computed as part of the optimization process so that the error on
each segment, as measured the objective function, satisfies
$\sqrt{\mathcal Q^k} = \sqrt{\mathcal Q_x^k+ \mathcal Q_y^k} \le \tau$,
where $\tau$ is a user specified tolerance.

To find the optimal segmentation of a boundary, HOHQMesh implements a
marching scheme where the global error is bounded by a tolerance, and
each step along the way is chosen so it contributes a prorated amount
along the curve. Note that the true best approximation couples the
segments, and marching independently does not.

We can set the tolerance in several ways. For example, each section
could contribute an amount of error which we bound in terms of the local
error tolerance $\tau$ as

$$
 e_k =\sqrt{\frac{h_{k}}{2}\int\limits_{-1}^{1}\left\{ \left| \vec{\Gamma}(t_{k}+h_{k}(s+1)/2 ) - \sum_{j=0}^N \vec{b}^{(k)}_j \phi_j(s) \right|^2 + \left|\vec{\Gamma}'(t_{k}+h_{k}(s+1)/2 ) - \sum_{j=0}^N \vec{b}^{(k)}_j \frac{ds}{dt}\phi'_j(s) \right|^2 \right\} \,\mathrm{d}s}
\le \tau,
\tag{2}
$$

where $h_k = t_k - t_{k-1}$ is the length of the interval.
Then the problem becomes: For each $k$ find $h_{k}$ such that

$$
f(h_{k}) = e_{k} - \gamma\tau = 0.
\tag{3}
$$

The desired approximation is global, but the marching scheme is local.
HOHQMesh modifies the problem by constraining the approximation to match
the curve values at the end points. To account for this later, it
includes a safety factor $\gamma \le 1$ so that it can go back and
compute the best approximation given the computed distribution of
segments that the full global error with continuity conditions satisfies
the tolerance.

The marching algorithm has a disadvantage that it does not look ahead.
Therefore, it can end with a "leftover" small segment. To ameliorate
this, an estimate of an acceptable segment size is computed and the
smaller of that and halfway to the previous segment start location is
used. The next to the last segment will decrease in size (but still
remain within the tolerance), but the last segment will not have as
small a size to create significant time stepping issues due to the
resulting mesh.

### Implementation

- The procedure `SUBROUTINE OptimizeCurve` is found
  in the file
  [`Source/BoundaryOptimization/CurveOptimization.f90`](https://github.com/trixi-framework/HOHQMesh/blob/BoundaryOptimization/Source/BoundaryOptimization/CurveOptimization.f90).

- The nonlinear solve for (3) is performed in the `FUNCTION iterate`,
- found in the same file, which combines
  bisection (`FUNCTION bisect`) and the Pegasus
  method, (Dowell, M., Jarratt, P. (1972) The "Pegasus" method for
  computing the root of an equation, [DOI: 10.1007/BF01932959](https://doi.org/10.1007/BF01932959))

- The marching function (2) is implemented in `FUNCTION marchingFunction`.

## The Adaptive Mesh Procedure

The mesh adaptation is performed in the procedure `SUBROUTINE GenerateQuadMesh` in the file
[`Source/Mesh/MeshGeneratorMethods.f90`](https://github.com/trixi-framework/HOHQMesh/blob/BoundaryOptimization/Source/Mesh/MeshGeneratorMethods.f90)
The sequence of events is shown in the following algorithm.
<figure id="fig:alg2" data-latex-placement="htbp">
<img src="https://github.com/user-attachments/assets/ee947034-8791-4615-a97b-287d13ff1125" style="width:7in" />
</figure>

The number of tries to generate a mesh with the user's tolerance is `numberOfTries = 3`.

!!! warning "Warning"
    There is no guaranteed convergence of the algorithm to generate a mesh to within the expected error.

However, HOHQMesh does supply the means to print the errors along the
boundaries, which could be used to insert a refinement region to further
refine the mesh in difficult situations.
