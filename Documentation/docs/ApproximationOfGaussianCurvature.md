# Approximation of Gaussian Curvature

The sizing of the bottom topography is done using the Gaussian curvature of the surface using the GaussianCurvature class.

From wikipedia, the Gaussian curvature for a surface $f(x,y) - z = F(x,y,z) = 0$ is

$$
\begin{equation}
K=-\frac{1}{|\nabla F|^4} \left|\begin{array}{cc}H(F) & \nabla F^T \\\nabla F & 0\end{array}\right|=-\frac{1}{|\nabla F|^4}\left|\begin{array}{cccc}F_{xx} & F_{xy} & F_{xz} & F_x \\F_{xy} & F_{yy} & F_{yz} & F_y \\F_{xz} & F_{yz} & F_{zz} & F_z \\F_x & F_y & F_z & 0\end{array}\right|
\end{equation}
$$

where $H$ is the Hessian matrix.

Filling in the derivatives,

$$
\begin{equation}
K=-\frac{1}{|\nabla F|^4}\left|\begin{array}{cccc}f_{xx} & f_{xy} & 0 & f_x \\f_{xy} & f_{yy} & 0 & f_y \\0 & 0 & 0 & -1 \\f_x & f_y & -1 & 0\end{array}\right|
\end{equation}
$$

where
$|\nabla F|^4 = (f_x^2 + f_y^2 + 1)^2$.

Then

$$
\begin{equation}\begin{split}
-{|\nabla F|^4}K &= \left|\begin{array}{cccc}f_{xx} & f_{xy} & 0 & f_x \\f_{xy} & f_{yy} & 0 & f_y \\0 & 0 & 0 & -1 \\f_x & f_y & -1 & 0\end{array}\right|
\\[0.2cm]
&= f_{xx}\left|\begin{array}{ccc}f_{yy} & 0 & f_y \\0 & 0 & -1 \\f_y & -1 & 0\end{array}\right|
- f_{xy}\left|\begin{array}{ccc}f_{xy} & 0 & f_y \\0 & 0 & -1 \\f_x & -1 & 0\end{array}\right|
-f_x\left|\begin{array}{ccc}f_{xy} & f_{yy} & 0 \\0 & 0 & 0 \\f_x & f_y & -1\end{array}\right|
\\[0.2cm]
&= f_{xx}f_{yy}-f^2_{xy}
\end{split}
\end{equation}
$$

So, finally,

$$
\begin{equation}
K = \frac{f_{xx}f_{yy}-f^2_{xy}}{ (f_x^2 + f_y^2 + 1)^2}
\end{equation}
$$

*Example:* For a sphere, $z = \sqrt{1 - (x^2+y^2)}= f(x,y)$, $K=1$.

To approximate $K$, we compute a centered finite difference approximation to the first derivatives

$$
\begin{equation}
\begin{gathered}
f_x\approx \frac{f(x+h,y) - f(x-h,y)}{2h}\\[0.2cm]
f_y\approx \frac{f(x,y+h) - f(x,y-h)}{2h}\\
\end{gathered}
\end{equation}
$$

and similarly for the second derivatives,

$$
\begin{equation}
\begin{gathered}
f_{xx} \approx \frac{f(x+h,y) -2f(x,y) + f(x-h,y)}{h^2}\\[0.2cm]
f_{yy} \approx \frac{f(x,y+h) -2f(x,y) + f(x,y+h)}{h^2}\\[0.2cm]
f_{xy} = f_{yx}\approx \frac{f(x+h,y+h) - f(x-h,y+h)- f(x+h,y-h) + f(x-h,y-h)}{4\Delta x\Delta y}
\end{gathered}
\end{equation}
$$

The step size is taken to be $h = 10^{-4}$.
