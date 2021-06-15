# The Control File

The `MODEL` and the `CONTROL_INPUT` blocks described above are put into a single file called the **control file**, which is finished with a `\end{FILE}` command. An example of a full control file that meshes a model with a circular outer boundary and two inner circular boundaries, and writes out a plot file with spectral element resolution,
is shown below. There is also a template control file in the *Examples* directory.

Just some notes: 

1. Blocks do not have to be specified in any order (e.g. the `MODEL` could come first before the `CONTROL_INPUT`. `INNER_BOUNDARIES` could come before `OUTER_BOUNDARY`. Blocks must be defined withing their appropriate block however, e.g. `OUTER_BOUNDARY` can only be defined in a `MODEL` block.
2. Keywords within a block can be specified in any order. The only ordering that is important is that within a `CHAIN`, the curves must be specified in order, counter-clockwise. 
3. Spaces in keywords **are** significant, but not in any other contexts. For instance equals signs are aligned only for visual formatting.
4. Blank lines or lines starting with % are ignored.

---

	\begin{CONTROL_INPUT}
	
	   \begin{RUN_PARAMETERS}
	      mesh file name         = Circles3Mesh.mesh
	      plot file name         = Circles3Plot.tec
	      statistics file name   = Circles3Stats.txt
	      mesh file format       = ISM
	      polynomial order       = 6
	      plot file format       = sem
	   \end{RUN_PARAMETERS}
	
	   \begin{BACKGROUND_GRID}
	     background grid size = [4.0,4.0]
	   \end{BACKGROUND_GRID}
	
	   \begin{SPRING_SMOOTHER}
	     smoothing type       = LinearAndCrossbarSpring
	     number of iterations = 20
	   \end{SPRING_SMOOTHER}
	
	\end{CONTROL_INPUT}
	
	\begin{MODEL}

		\begin{OUTER_BOUNDARY}
	   		\begin{PARAMETRIC_EQUATION_CURVE}
			 	name = outer
		 		xEqn = x(t) = 14.0*cos(2*pi*t)
		 		yEqn = y(t) = 14.0*sin(2*pi*t)
		 		zEqn = z(t) = 0.0
	  	 	\end{PARAMETRIC_EQUATION_CURVE}
		\end{OUTER_BOUNDARY}

		\begin{INNER_BOUNDARIES}
	
	   		\begin{CHAIN}
		       	name = Boundary 1
	           	\begin{PARAMETRIC_EQUATION_CURVE}
						name = Circle1
						xEqn = f(t) = -10.25 + 0.2*cos(2*pi*t)
						yEqn = f(t) = 3.0 + 0.2*sin(2*pi*t)
						zEqn = z(t) = 0.0
		       	\end{PARAMETRIC_EQUATION_CURVE}
	  		\end{CHAIN}
	   
	   		\begin{CHAIN}
	      	 	name = Boundary 2
	           \begin{PARAMETRIC_EQUATION_CURVE}
					name = Circle2
					xEqn = f(t) = -5.1 + 1.0*cos(2*pi*t)
					yEqn = f(t) = 1.0*sin(2*pi*t) - 4.1
					zEqn = z(t) = 0.0
	      		\end{PARAMETRIC_EQUATION_CURVE}
	   		\end{CHAIN}
	   
	   		\begin{CHAIN}
		       	name = Boundary 3
	           	\begin{PARAMETRIC_EQUATION_CURVE}
					name = Circle3
					xEqn = f(t) = -12.0 + 0.5*cos(2*pi*t)
					yEqn = f(t) = 0.5*sin(2*pi*t) - 0.5
					zEqn = z(t) = 0.0
	      		\end{PARAMETRIC_EQUATION_CURVE}
	   		\end{CHAIN}
		\end{INNER_BOUNDARIES}
	\end{MODEL}
	\end{FILE}
