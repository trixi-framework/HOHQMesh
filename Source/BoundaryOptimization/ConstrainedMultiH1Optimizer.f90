!
!////////////////////////////////////////////////////////////////////////
!
!      ConstrainedMultiH1Optimizer.f90
!      Created: September 29, 2025 at 9:28 AM 
!      By: David Kopriva  
!
!      Solves the least squares problem with constraints for multiple
!      subdivisions of a curve. Solves the system
!
!         S\cdot b = Rhs
!
!      where
!
!      b   = [b_1 b_2 ... b_K | \lambda_0 \lambda_1 ... \lambda_K]^T
!      Rhs = [h_1/2*r_1 h_2/2*r_2 ... h_K/2*r_K | x(t_0) 0 ... 0 x(t_{max})]^T
!      K = # subdivisions
!
!      S = | A   C |
!          | C^T 0 |
!   
!      The main entries are:
!
!      1. Setup
!
!      SUBROUTINE ConstructCMH1O(self, N, M, nSegments, cuts, options)
!
!        ----------------------------------------------------------------
!        N                    = Appoximation order
!        M                    = Quadrature order
!        nSegments            = Number of segments in the total domain
!        cuts(0:nSegments)    = segment points (TODO: Come up with better names)
!         
!        options               =
!         % internalConstraint = smoothness order at internal interfaces. 
!                                0 = continuity, 1 = 1st derivative, etc.
!         % endConstraint      = FIXED_CONSTRAINT .or. PERIODIC_CONSTRAINT
!         % whichnorm          = L2_NORM .OR. H1_NORM
!     ----------------------------------------------------------------
!  
!
!      2. Solve
!       
!      SUBROUTINE Optimize( self, curveFun, optimalCoefficients )  
!         CLASS(MultiH1Optimizer)    :: self
!         REAL(KIND=RP), INTENT(out) :: optimalCoefficients(0:self % N,2,self % nSegments)
!         CLASS(SMCurve), POINTER    :: curveFun
!
!      To use the results, create a MultiSegmentModalCurve from the optimal coefficients.
!
!////////////////////////////////////////////////////////////////////////
!
   Module ConstrainedMultiH1Optimization
      USE ProgramGlobals
      USE LegendreAlgorithms
      USE SMCurveClass
      IMPLICIT NONE
      
      INTEGER, PARAMETER :: NO_CONSTRAINT       = 0
      INTEGER, PARAMETER :: FIXED_CONSTRAINT    = -1
      INTEGER, PARAMETER :: PERIODIC_CONSTRAINT = -2
      INTEGER, PARAMETER :: H1O_OK = 0, H1O_CONVERT = 1
!
!     --------------------------------
!     Utility to use to compute errors
!     --------------------------------
!
      TYPE GaussQuadratureType
         INTEGER                    :: N
         REAL(KIND=RP), ALLOCATABLE :: nodes(:), weights(:)
      END TYPE GaussQuadratureType
      
      TYPE OptimizerOptions
         INTEGER       :: internalConstraint ! Default = 0
         INTEGER       :: endConstraint      ! Default = FIXED_CONSTRAINT
         INTEGER       :: whichNorm          ! DEFAULT = H1_NORM
         REAL(KIND=RP) :: toler              ! DEFAULT = 1.0d-4
         REAL(KIND=RP) :: safetyFactor       ! DEFAULT = 0.9d0
      END TYPE OptimizerOptions
      
      TYPE MultiH1Optimizer
         INTEGER                    :: N ! Approximation order
         INTEGER                    :: M ! Quadrature order
         INTEGER                    :: nSegments
         REAL(KIND=RP), ALLOCATABLE :: cuts(:)
         REAL(KIND=RP), ALLOCATABLE :: dsdt(:)
         REAL(KIND=RP), ALLOCATABLE :: breaks(:)
         REAL(KIND=RP), ALLOCATABLE :: breakIndices(:)
         INTEGER                    :: internalConstraint
         INTEGER                    :: endConstraint
         
         INTEGER                    :: totalStateVectors
         INTEGER                    :: nConstraints
         INTEGER                    :: globalStateLength
         
         TYPE(GaussQuadratureType)  :: GQuadrature
         REAL(KIND=RP), ALLOCATABLE :: L(:,:)  , LPrime(:,:)
         REAL(KIND=RP), ALLOCATABLE :: locMatrix(:,:)
         REAL(KIND=RP), ALLOCATABLE :: CTrans(:,:)
         REAL(KIND=RP), ALLOCATABLE :: SMatrix(:,:)
         REAL(KIND=RP), ALLOCATABLE :: rhs(:,:)
         INTEGER      , ALLOCATABLE :: iPvt(:)
         INTEGER                    :: whichNorm
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE :: construct => ConstructCMH1O
         FINAL     :: DestructCMH10
         PROCEDURE :: Optimize
      END TYPE MultiH1Optimizer
!
!     ========
      CONTAINS  
!     ========
!
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE ConstructGaussQuadrature(self, N)  
      IMPLICIT NONE
      TYPE(GaussQuadratureType) :: self
      INTEGER                   :: N
      
      self % N = N
      ALLOCATE(self % nodes(0:N), self % weights(0:N))
      CALL LegendreLobattoNodesAndWeights( N, self % nodes, self % weights )
 
   END SUBROUTINE ConstructGaussQuadrature
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE SetDefaultOptions(self)  
      IMPLICIT NONE  
      TYPE(OptimizerOptions) :: self
      
      self % internalConstraint = 0
      self % endConstraint      = FIXED_CONSTRAINT
      self % toler              = 1.0d-4
      self % whichNorm          = H1_NORM
      self % safetyFactor       = 0.90_RP
       
   END SUBROUTINE SetDefaultOptions
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE ConstructCMH1O(self, N, M, nSegments, cuts, breakIndices, &
                                options, testModeOptn)
!
!     ----------------------------------------------------------------
!     N                    = Appoximation order
!     M                    = Quadrature order
!     nSegments            = Number of segments in the total domain
!     cuts(0:nSegments)    = segment points (TODO: Come up with better names) 
!     breakIndices         = marks the segments at which breaks are placed
!     options(OptimizerOptions):
!        options % internalConstraint = smoothness order at internal interfaces. 
!                                       0 = continuity, 1 = 1st derivative, etc.
!        options % endConstraint      = FIXED_CONSTRAINT .or. PERIODIC_CONSTRAINT .OR. NO_CONSTRAINT
!        options % whichnorm          = L2_NORM .OR. H1_NORM
!     testMode             = (OPTIONAL) Turns on/off test mode
!     ----------------------------------------------------------------
!
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      CLASS(MultiH1Optimizer) :: self
      TYPE(OptimizerOptions)  :: options            ! parameters for the approximation
      INTEGER                 :: N
      INTEGER                 :: M
      INTEGER                 :: nSegments
      REAL(KIND=RP)           :: cuts(0:nSegments)
      INTEGER                 :: breakIndices(:)
      LOGICAL, OPTIONAL       :: testModeOptn
      LOGICAL                 :: testMode
!
!     ---------------
!     Local variables
!     ---------------
!
      INTEGER                    :: nConstraints, totalStateVectors
      INTEGER                    :: globalStateLength,  constraintCol, constraintRow
      INTEGER                    :: j, k
      INTEGER                    :: nc
      
      self % whichnorm          = options % whichNorm
      self % nSegments          = nSegments
      self % endConstraint      = options % endConstraint
      self % internalConstraint = options %internalConstraint
      
      ALLOCATE( self % cuts(0:nSegments), source = cuts )
      self % breakIndices = breakIndices
      
      self % N = N
      self % M = M
      
      totalStateVectors = (N+1)*nSegments
      
!     Total number of constraints
!     2 endpoints + subdivision*number of derivatives
!     This gives the number of rows in C^T

      IF ( self % endConstraint == NO_CONSTRAINT )     THEN
         nc = 0
      ELSE IF(self % endConstraint == PERIODIC_CONSTRAINT)     THEN
         nc = 1 + self % internalConstraint
      ELSE
         nc = 2 
      END IF 
      nConstraints      = nc + (nSegments-1)*(self % internalConstraint+1)
      
!     Sizes of the transpose of the constraint matrix. C^T b = d
      constraintCol     = totalStateVectors
      constraintRow     = nConstraints
      
!     Size of the global state vector and normal matrix, A , b + \lambda
      globalStateLength = totalStateVectors + nConstraints
!
      self % globalStateLength = globalStateLength
      self % totalStateVectors = totalStateVectors
      self % nConstraints      = nConstraints
!
!     ------------------------------
!     Allocate Local arrays/matrices
!     ------------------------------
!
      CALL ConstructGaussQuadrature(self % GQuadrature, M)
      ALLOCATE(self % L(0:M, 0:N), self % LPrime(0:M, 0:N))
      ALLOCATE(self % locMatrix(0:N,0:N))
!
!     -------------------------------
!     Allocate Global arrays/matrices
!     -------------------------------
!
      ALLOCATE(self % CTrans(constraintRow,constraintCol), source = 0.0_RP)
      ALLOCATE(self % SMatrix(globalStateLength,globalStateLength), source = 0.0_RP)
      ALLOCATE(self % rhs(globalStateLength,2), source = 0.0_RP)
      ALLOCATE(self % iPvt(globalStateLength), source = 0)
!
!     ------
!     Sizing
!     ------
!
      ALLOCATE(self % dsdt(nSegments))
      DO k = 1, nSegments
         self % dsdt(k) = 2.0_RP/(cuts(k) - cuts(k-1)) ! = 2/h
      END DO
!
!     ---------------------
!     Local Basis functions
!     ---------------------
!
      DO j = 0, M 
         CALL LegendrePolysAndDerivatives(N, self % gQuadrature % nodes(j), self % L(j,:), self % LPrime(j,:))
      END DO
      CALL ConstructConstraintMatrix(self)
      CALL ConstructGlobalMatrix(self)
!
!     ------------------------------------------------------------
!     Do LU decomposition on the matrix, unless it's test mode,
!     in which case we don't want to destroy the matrix right away
!     ------------------------------------------------------------
!
      IF ( PRESENT(testModeOptn) )     THEN
         testMode = testModeOptn
      ELSE
         testMode = .FALSE. 
      END IF 
      IF ( .NOT.testMode )     THEN
         CALL LUFactorization(A = self % SMatrix, pivots = self % iPvt,n = self % globalStateLength)
      END IF 
       
      END SUBROUTINE ConstructCMH1O
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE Reconstruct(self, cuts)
!
!        --------------------------------------------------------------
!        For a previously constructed self, readjust matrices for a new
!        cuts distribution. Nothing else can change.
!        --------------------------------------------------------------
!
         IMPLICIT NONE  
         CLASS(MultiH1Optimizer) :: self
         REAL(KIND=RP)           :: cuts(0:self % nSegments)
         INTEGER                 :: k
!
!        ------
!        Sizing
!        ------
!
         DO k = 1, self % nSegments
            self % dsdt(k) = 2.0_RP/(cuts(k) - cuts(k-1)) ! = 2/h
         END DO
!
!        --------------------
!        Reconstruct matrices
!        --------------------
!
         CALL ConstructConstraintMatrix(self)
         CALL ConstructGlobalMatrix(self)
!
         CALL LUFactorization(A = self % SMatrix, pivots = self % iPvt,n = self % globalStateLength)
          
      END SUBROUTINE Reconstruct
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE DestructCMH10(self)  
         IMPLICIT NONE  
         TYPE(MultiH1Optimizer) :: self
         self % N = 0
         self % M = 0
         
         IF(ALLOCATED(self % SMatrix))      DEALLOCATE(self % SMatrix)
         IF(ALLOCATED(self % gQuadrature % nodes))        DEALLOCATE(self % gQuadrature % nodes)
         IF(ALLOCATED(self % gQuadrature % weights))      DEALLOCATE(self % gQuadrature % weights)
         IF(ALLOCATED(self % cuts))         DEALLOCATE(self % cuts)
         IF(ALLOCATED(self % rhs))          DEALLOCATE(self % rhs)
         IF(ALLOCATED(self % L))            DEALLOCATE(self % L)
         IF(ALLOCATED(self % LPrime))       DEALLOCATE(self % LPrime)
         IF(ALLOCATED(self % locMatrix))    DEALLOCATE(self % locMatrix)
         IF(ALLOCATED(self % CTrans))       DEALLOCATE(self % CTrans)
         IF(ALLOCATED(self % iPvt))         DEALLOCATE(self % iPvt)
         
      END SUBROUTINE DestructCMH10
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE ConstructConstraintMatrix(self)  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(MultiH1Optimizer)    :: self
!
!        ---------------
!        Local Variables
!        ---------------
!
         INTEGER                    :: r, c, j, N, cl, k
         REAL(KIND=RP), ALLOCATABLE :: CAPlus(:,:)
         REAL(KIND=RP), ALLOCATABLE :: CAMinus(:,:)
         
         N = self % N
         
         ALLOCATE(CAPlus(0:N,0:self % internalConstraint),CAMinus(0:N,0:self % internalConstraint))
         CALL ConstructContinuityArray(CAPlus ,N,self % internalConstraint, 1.0_RP)
         CALL ConstructContinuityArray(CAMinus,N,self % internalConstraint,-1.0_RP)
!
!        ----------------------------
!        Set up the constraint matrix
!        ----------------------------
!
         IF ( self % endConstraint == FIXED_CONSTRAINT )     THEN
!
!           ------------------
!           First and last row
!           ------------------
!
            r = 1; c = 1
            self % CTrans(r,c:c+N) = CAMinus(:,0)
            r = self % nConstraints ; c = 1 + (self % nSegments-1)*(N+1)
            self % CTrans(r,c:c+N) = CAPlus(:,0)
            r = 2 ! Reset to start the following rows
            
         ELSE IF(self % endConstraint == PERIODIC_CONSTRAINT)     THEN
!
!           ---------------------------------------------
!           nConstraint+1 rows for smoothness constraints
!           ---------------------------------------------
!
            r = 1; c =1 + (self % nSegments-1)*(N+1)
            DO j = 0, self % internalConstraint
               self % CTrans(r,1:1+N) =  CAMinus(:,j)*self % dsdt(1)**j
               self % CTrans(r,c:c+N) = -CAPlus(:,j) *self % dsdt(self % nSegments)**j 
               r = r + 1
            END DO
            
         END IF 
         
         cl = 1; c = cl + N+1 ! & Pick up r above where boundary constraints left off
         DO k = 2, self % nSegments
            DO j = 0, self % internalConstraint
               self % CTrans(r,c:c+N)   =  CAMinus(:,j)*self % dsdt(k)**j
               self % CTrans(r,cl:cl+N) = -CAPlus(:,j) *self % dsdt(k-1)**j 
               r = r + 1
            END DO
            c = c + N+1; cl = cl + N+1
         END DO
      
      END SUBROUTINE ConstructConstraintMatrix
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE ConstructGlobalMatrix(self)  
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(MultiH1Optimizer)    :: self
!
!        ---------------
!        Local Variables
!        ---------------
!
         INTEGER       :: r, c, i, j, k
         INTEGER       :: N, nSegments, constraintRow, constraintCol
         REAL(KIND=RP) :: scal
         REAL(KIND=RP) :: h(1:self % nSegments)
         
         constraintCol = self % totalStateVectors
         constraintRow = self % nConstraints
         N             = self % N
         nSegments     = self % nSegments
         
         DO k = 1, nSegments
            h(k) = self % cuts(k) - self % cuts(k-1)
         END DO
!
!        -----------------------------
!        Construct the global matrix
!        -----------------------------
!
         DO i = 1, nSegments ! Upper left diagonal
            scal = (2.0_RP/h(i))**2
            CALL ConstructLocalNormalMatrix(self, scal)
            j = 1 + (i-1)*(N+1)
            DO r = 0, N
               DO c = 0, N
                  self % SMatrix(j + r,j + c) = 0.5_RP*h(i)*self % locMatrix(r,c)
               END DO  
            END DO 
         END DO
!
!        -----------------------
!        Upper right, constraint
!        -----------------------
!
         r = 0 ; c = nSegments*(N+1)
         DO i = 1, constraintRow 
            DO j = 1, constraintCol 
               self % SMatrix(r+j,c+i) = self % CTrans(i,j)
            END DO  
         END DO 
!
!        ---------------------
!        Lower left constraint
!        ---------------------
!
         r = nSegments*(N+1); c = 0
         DO i = 1, constraintRow 
            DO j = 1, constraintCol 
               self % SMatrix(r+i,c+j) = self % CTrans(i,j)
            END DO  
         END DO
         
      END SUBROUTINE ConstructGlobalMatrix
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE Optimize( self, curveFun, optimalCoefficients )  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(MultiH1Optimizer)    :: self
         REAL(KIND=RP), INTENT(out) :: optimalCoefficients(0:self % N,2,self % nSegments)
         CLASS(SMCurve), POINTER    :: curveFun
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER :: j, k, i
!
!        ----------------------------------------------
!        Compute the RHS for the desired curve function
!        ----------------------------------------------
!
         CALL ConstructRHS(self,curveFun)
!
!        ---------------------------------
!        Solve the global system S b = rhs
!        ---------------------------------
!
         DO i = 1, 2 
            CALL ForwardBackwardSolve(A = self % SMatrix, b = self % rhs(:,i), &
                                      pivots = self % iPvt,n = self % globalStateLength)
!
!           ----------------------------------------------------------
!           Re-package the global coefficient vector into the the form
!           coefs(#modes,[x,y],#segments)
!           ----------------------------------------------------------
!
            DO k = 1, self % nSegments 
               j                           = 1 + (k-1)*(self % N + 1)
               optimalCoefficients(0:,i,k) = self % rhs(j:j+self % N,i)
            END DO
         END DO 
!
      END SUBROUTINE Optimize
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE ConstructLocalNormalMatrix( self, scal )  
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      TYPE(MultiH1Optimizer) :: self
      REAL(KIND=RP)          :: scal ! = (ds/dt)^2
      
      INTEGER :: i, j

      DO j = 0, self % N 
         DO i = 0, self % N 
            self % locMatrix(i,j) = gaussInnerProduct(self % L(:,i), self % L(:,j), &
                                                      self % gQuadrature % weights, self % M)
         END DO  
      END DO
      IF ( self % whichNorm == H1_NORM )     THEN
         DO j = 0, self % N 
            DO i = 0, self % N 
               self % locMatrix(i,j) = self % locMatrix(i,j) + &
                                       scal *gaussInnerProduct(self % LPrime(:,i),self % LPrime(:,j),&
                                                               self % gQuadrature % weights, self % M)
            END DO  
         END DO
      END IF 

   END SUBROUTINE ConstructLocalNormalMatrix
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE ConstructRHS( self, curveFun )
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      TYPE(MultiH1Optimizer)  :: self
      CLASS(SMCurve)          :: curveFun
!
!     ---------------
!     Local variables
!     ---------------
!
      REAL(KIND=RP)  :: f(0:self % M,2), fp(0:self % M,2)
      REAL(KIND=RP)  :: v(3), z
      REAL(KIND=RP)  :: dsdt, hOver2
      INTEGER        :: j, k, i, r
!
       self % rhs = 0.0_RP
!
!      ----------------
!      Curve projection
!      ----------------
!
       DO k = 1, self % nSegments
            i      = 1 + (k-1)*(self % N + 1)
            hOver2 = 0.5_RP*(self % cuts(k) - self % cuts(k-1))    
            dsdt   = self % dsdt(k) !1.0_RP/hOver2
            
            DO j = 0, self % M
               z         = affineMap(t0 = self % cuts(k-1), &
                                     t1 = self % cuts(k),   &
                                     s  = self % gQuadrature % nodes(j))
               v         = curveFun % positionAt(z)
               f(j,1:2)  = v(1:2)
               
               v         = curveFun % derivativeAt(z)
               fp(j,1:2) = v(1:2)
            END DO
            
            DO j = 0, self % N 
               self % rhs(i+j,1) =  hOver2*gaussInnerProduct(f(:,1) , self % L(:,j),self % gQuadrature % weights, self % M)
               self % rhs(i+j,2) =  hOver2*gaussInnerProduct(f(:,2) , self % L(:,j),self % gQuadrature % weights, self % M)
            END DO
            
            IF ( self % whichNorm == H1_NORM )     THEN ! note: dsdt*hOver2 = 1. Leaving for clarity.
               DO j = 0, self % N 
                  self % rhs(i+j,1) =  self % rhs(i+j,1) + &
                                       dsdt*hOver2*gaussInnerProduct(fp(:,1), self % LPrime(:,j), &
                                       self % gQuadrature % weights, self % M)
                  self % rhs(i+j,2) =  self % rhs(i+j,2) + &
                                       dsdt*hOver2*gaussInnerProduct(fp(:,2), self % LPrime(:,j), &
                                       self % gQuadrature % weights, self % M)
               END DO 
       
            END IF 
       END DO 
       IF( self % endConstraint == PERIODIC_CONSTRAINT) RETURN
!
!      ------------------------------------------------
!      Constraint section if the endpoints are included
!      ------------------------------------------------
!
       r = 1 + self % nSegments*(self % N + 1)
       v = curveFun % positionAt( self % cuts(0))
       self % rhs(r,:) = v(1:2)
       
       r = UBOUND(self % rhs,1) 
       v = curveFun % positionAt(self % cuts(self % nSegments))
       self % rhs(r,:) = v(1:2)
       
   END SUBROUTINE ConstructRHS
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE ConstructContinuityArray(C, N, p, one)  
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      INTEGER,       INTENT(IN)       :: N          !     = highest polynomial order
      INTEGER,       INTENT(IN)       :: p          ! < N = highest derivative to match
      REAL(KIND=RP)                   :: C(0:N,0:p) !     = L_k^m
      REAL(KIND=RP), INTENT(IN)       :: one        !     = \pm 1
!
!     ---------------
!     Local variables
!     ---------------
!
      INTEGER :: k, m
      C = 0.0_RP
      DO k = 0, N 
         C(k,0) = one**k
      END DO
      IF ( p == 0 )  RETURN

      DO k = 0, N 
         C(k,1) = 0.5_RP*one**(k+1)*k*(k+1) 
      END DO
      IF(p == 1)     RETURN
      
      DO m = 2, p 
         DO k = m, N 
            C(k,m) = (2*k-1)*C(k-1,m-1) - C(k-2,m) 
         END DO  
      END DO 
      
   END SUBROUTINE ConstructContinuityArray
!
!//////////////////////////////////////////////////////////////////////// 
! 
      REAL(KIND=RP) FUNCTION affineMap(t0,t1,s)  
         IMPLICIT NONE  
         REAL(KIND=RP) :: t0, t1, s !\in [-1,1]
         
         affineMap = t0 + (t1 - t0)*0.5_RP*(1.0_RP + s)
         
      END FUNCTION affineMap
!
!//////////////////////////////////////////////////////////////////////// 
! 
      REAL(KIND=RP) FUNCTION InvAffineMap(t0,t1,t)  
         IMPLICIT NONE  
         REAL(KIND=RP) :: t0, t1, t !\in [t0,t1]
         
         InvAffineMap = 2.0_RP*(t - t0)/(t1 - t0) - 1.0_RP
         
      END FUNCTION InvAffineMap
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE LUFactorization(A, pivots, n)
! Computes the LU factorization of the matrix A with partial (row) pivoting.
! The factorization is done in-place overwriting the input array A.
! The vector pivots stores the row permutations of the partial pivoting
! where pivots(k) is the index of the kth pivot row.
!
! Note, this routine assumes that A is nonsingular as no checks are done.
       IMPLICIT NONE
       INTEGER, INTENT(IN)                          :: n
       REAL(KIND=RP), INTENT(INOUT), DIMENSION(n,n) :: A
       INTEGER, INTENT(OUT), DIMENSION(n)           :: pivots
   
       ! Local variables
       INTEGER :: j, k, pivot_idx
       REAL(KIND=RP), DIMENSION(n) :: pivot_row
   
       ! Fill pivots vector with the default order of the matrix
       pivots = [(j, j=1,n)]
   
       ! Gaussian elimination with row pivoting done in-place
       DO j = 1,n-1
           ! Find the index of the largest pivot element in the current column
           k = MAXLOC(ABS(A(j:n,j)), 1)
   
           ! Adjust to be the "global" pivot index of the matrix
           k = j - 1 + k
   
           ! Swap the rows of the in-place array
           pivot_row = A(k,:)
           A(k,:) = A(j,:)
           A(j,:) = pivot_row
   
           ! Swap the "rows" in the pivot vector
           pivot_idx = pivots(k)
           pivots(k) = pivots(j)
           pivots(j) = pivot_idx
   
           ! Scale the "active" matrix with the pivot element
           A(j+1:n,j) = A(j+1:n,j) / A(j,j)
   
           ! Rank-1 update for elimination in the "active" matrix.
           ! This requires an outer product
           !       A(j+1:n,j) * A(j,j+1:n)
           ! which we achieve using SPREAD
           A(j+1:n,j+1:n) = A(j+1:n,j+1:n) - &
                               SPREAD(A(j+1:n,j), dim=2, ncopies=n-j) * &
                               SPREAD(A(j,j+1:n), dim=1, ncopies=n-j)
       END DO ! j
   
       RETURN
   END SUBROUTINE LUFactorization
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE ForwardBackwardSolve(A, b, pivots, n)
! Solve the linear system A x = b where A contains the in-place LU
! factorization from LUFactorization. The right hand side vector b
! is over-written to contain the solution vector x. The vector pivots
! contains the row pivoting information from LUFactorization.
       IMPLICIT NONE
       INTEGER, INTENT(IN)                         :: n
       REAL(KIND=RP), INTENT(IN), DIMENSION(n,n)   :: A
       REAL(KIND=RP), INTENT(INOUT), DIMENSION(n)  :: b
       INTEGER, INTENT(IN), DIMENSION(n)           :: pivots
   
       ! Local variables
       INTEGER :: j
   
       ! Pivot the right hand side vector
       b = b(pivots)
   
       ! Forward elimination. Note, exploits that the diagonal entries
       ! of the lower triangular matrix from the in-place LU are all one
       ! such that no divisions are needed.
       DO j = 2,n
           b(j) = b(j) - DOT_PRODUCT(A(j,1:j-1), b(1:j-1))
       END DO ! j
   
       ! Backward substitution
       b(n) = b(n) / A(n,n)
       DO j = n-1,1,-1
           b(j) = (b(j) - DOT_PRODUCT(A(j,j+1:n), b(j+1:n))) / A(j,j)
       END DO ! j
   
       RETURN
   END SUBROUTINE ForwardBackwardSolve
!
!//////////////////////////////////////////////////////////////////////// 
! 
   FUNCTION spotCheckSmoothnessConditionsIsOK()  RESULT(r)
      IMPLICIT NONE
      LOGICAL             :: r
      INTEGER, PARAMETER  :: N = 5, p = 3
      REAL(KIND=RP)       :: C(0:5,0:3)
      REAL(KIND=RP)       :: L2Prime = 3.0_RP
      REAL(KIND=RP)       :: L3Prime = 6.0_RP, L3DoublePrime = 15.0_RP
      
      CALL ConstructContinuityArray(C,N,p,1.0_RP)
      
      r = .FALSE.
      
      r = (C(2,1) - L2Prime) == 0.0_RP .AND. (C(3,2)-L3DoublePrime) == 0
      r = r .AND. (C(3,1) - L3Prime)       == 0.0_RP
      r = r .AND. (C(3,3) - L3DoublePrime) == 0.0_RP
    
   END FUNCTION spotCheckSmoothnessConditionsIsOK
   
   END Module ConstrainedMultiH1Optimization
