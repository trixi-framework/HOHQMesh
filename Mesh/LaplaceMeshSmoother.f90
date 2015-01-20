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
      
      PUBLIC :: ReadLaplaceSmootherBlock
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
      SUBROUTINE laplaceSmoothMesh( self, mesh, model )  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS (LaplaceMeshSmoother)   :: self
         CLASS (SMMesh)      , POINTER :: mesh
         CLASS (SMModel)     , POINTER :: model
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
         CLASS (SMMesh)             , POINTER :: mesh
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
         CHARACTER(LEN=32)          :: str
         CHARACTER(LEN=LINE_LENGTH) :: inputLine = " "
         REAL(KIND=RP), EXTERNAL    :: GetRealValue
         INTEGER      , EXTERNAL    :: GetIntValue
         
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         str = GetStringValue( inputLine )
         
         IF( str == "ON" )     THEN
            smp % smoothingON = .true.
         ELSE
            smp % smoothingON = .false.
         END IF
         
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         smp % numSteps = GetIntValue( inputLine )

      END SUBROUTINE ReadLaplaceSmootherBlock

      END MODULE LaplaceMeshSmootherClass
