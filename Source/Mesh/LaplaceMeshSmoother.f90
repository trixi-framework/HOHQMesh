! MIT License
!
! Copyright (c) 2010-present David A. Kopriva and other contributors: AUTHORS.md
!
! Permission is hereby granted, free of charge, to any person obtaining a copy  
! of this software and associated documentation files (the "Software"), to deal  
! in the Software without restriction, including without limitation the rights  
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell  
! copies of the Software, and to permit persons to whom the Software is  
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all  
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR  
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE  
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER  
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  
! SOFTWARE.
! 
! HOHQMesh contains code that, to the best of our knowledge, has been released as
! public domain software:
! * `b3hs_hash_key_jenkins`: originally by Rich Townsend, 
!    https://groups.google.com/forum/#!topic/comp.lang.fortran/RWoHZFt39ng, 2005
! * `fmin`: originally by George Elmer Forsythe, Michael A. Malcolm, Cleve B. Moler, 
!    Computer Methods for Mathematical Computations, 1977
! * `spline`: originally by George Elmer Forsythe, Michael A. Malcolm, Cleve B. Moler, 
!    Computer Methods for Mathematical Computations, 1977
! * `seval`: originally by George Elmer Forsythe, Michael A. Malcolm, Cleve B. Moler, 
!    Computer Methods for Mathematical Computations, 1977
!
! --- End License
!
!////////////////////////////////////////////////////////////////////////
!
!      LaplaceMeshSmoother.f90
!      Created: May 30, 2014 at 11:07 AM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      Module LaplaceMeshSmootherClass
      USE MeshSmootherClass
      USE SMMeshClass
      USE SMModelClass
      USE ConectionsModule
      IMPLICIT NONE 
      PRIVATE 
!
!     ---------------------
!     Class type definition
!     ---------------------
!
      TYPE, EXTENDS(MeshSmoother) :: LaplaceMeshSmoother
         INTEGER :: numSteps
!
!        ========
         CONTAINS
!        ========
!
         PROCEDURE, PUBLIC :: init       => initLaplaceSmoother
         PROCEDURE, PUBLIC :: destruct   => destructLaplaceMeshSmoother
         PROCEDURE, PUBLIC :: smoothMesh => laplaceSmoothMesh
         
      END TYPE LaplaceMeshSmoother
      PUBLIC ::LaplaceMeshSmoother
!
!     ----------------------
!     Other type definitions
!     ----------------------
!
      TYPE LaplaceSmootherParameters
         LOGICAL :: smoothingOn
         INTEGER :: numSteps
      END TYPE LaplaceSmootherParameters
      PUBLIC :: LaplaceSmootherParameters
      
      PUBLIC :: ReadLaplaceSmootherBlock, release
      
!      INTERFACE release
!         MODULE PROCEDURE ::  releaseLaplaceSmoother
!      END INTERFACE  
      
!
!     ========
      CONTAINS 
!     ========
!
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initLaplaceSmoother(self,lpr)  
         IMPLICIT NONE  
         CLASS(LaplaceMeshSmoother)      :: self
         TYPE(LaplaceSmootherParameters) :: lpr
         self % numSteps = lpr % numSteps
      END SUBROUTINE initLaplaceSmoother
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE destructLaplaceMeshSmoother(self)  
         IMPLICIT NONE  
         CLASS(LaplaceMeshSmoother) :: self
      END SUBROUTINE destructLaplaceMeshSmoother
!
!//////////////////////////////////////////////////////////////////////// 
! 
!      SUBROUTINE releaseLaplaceSmoother(self)  
!         IMPLICIT NONE
!         TYPE (LaplaceMeshSmoother), POINTER :: self
!         CLASS(FTObject)           , POINTER :: obj
!         
!         IF(.NOT. ASSOCIATED(self)) RETURN
!         
!         obj => self
!         CALL releaseFTObject(self = obj)
!         IF ( .NOT. ASSOCIATED(obj) )     THEN
!            self => NULL() 
!         END IF      
!      END SUBROUTINE releaseLaplaceSmoother
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE laplaceSmoothMesh( self, mesh, model, errorCode )  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS (LaplaceMeshSmoother)   :: self
         TYPE  (SMMesh)      , POINTER :: mesh
         TYPE  (SMModel)     , POINTER :: model
         INTEGER                       :: errorCode
!
!        ---------------
!        local variables
!        ---------------
!
         INTEGER :: iter
         
         CALL makeNodeToEdgeConnections( mesh )
         
         DO iter = 1, self % numSteps
            CALL relax( mesh ) 
         END DO 
         
         CALL deallocateNodeToEdgeConnections 
         
      END SUBROUTINE laplaceSmoothMesh
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE relax( mesh )  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE  (SMMesh)             , POINTER :: mesh
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                               :: k, id
         CLASS(FTLinkedListIterator), POINTER  :: iterator => NULL()
         CLASS(FTObject)            , POINTER  :: obj      => NULL()
         CLASS(SMNode)              , POINTER  :: node     => NULL()
         CLASS(SMEdge)              , POINTER  :: edge     => NULL()
         REAL(KIND=RP)                         :: x(3)
         
         iterator => mesh % nodesIterator
         
         IF ( ASSOCIATED(iterator) )     THEN
            CALL iterator % setToStart()
            
            DO WHILE(.NOT.iterator % isAtEnd())
               obj  => iterator % object()
               node => nodeFromSMMeshobject(obj)
               
               IF ( ASSOCIATED(node) .AND. .NOT.hasFixedPosition(node) )     THEN
                  id = node % id
                  x  = 0.0_RP
                  DO k = 1, numEdgesForNodes(id)
                     edge => edgesForNodes(k,id) % edge
                     IF ( edge % nodes(1) % node % id == id )     THEN
                        x = x + edge % nodes(2) % node % x
                     ELSE
                        x = x + edge % nodes(1) % node % x
                     END IF
                  END DO
                  node % x = x/numEdgesForNodes(id)
               END IF 
               
               CALL iterator % moveToNext()
            END DO  
         END IF

      END SUBROUTINE relax      
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ReadLaplaceSmootherBlock( fUnit, smp ) 
!
!        Example block is:
!
!         \begin{Smoother}
!            smoothing            = "ON"
!            number of iterations = 25
!         \end{Smoother}
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER                         :: fUnit
         TYPE(LaplaceSmootherParameters) :: smp
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                    :: ios
         CHARACTER(LEN=LINE_LENGTH) :: str
         CHARACTER(LEN=LINE_LENGTH) :: inputLine = " "
         REAL(KIND=RP), EXTERNAL    :: GetRealValue
         INTEGER      , EXTERNAL    :: GetIntValue
         
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         str = GetStringValue( inputLine )
         
         IF( TRIM(str) == "ON" )     THEN
            smp % smoothingON = .true.
         ELSE
            smp % smoothingON = .false.
         END IF
         
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         smp % numSteps = GetIntValue( inputLine )

      END SUBROUTINE ReadLaplaceSmootherBlock

      END MODULE LaplaceMeshSmootherClass
