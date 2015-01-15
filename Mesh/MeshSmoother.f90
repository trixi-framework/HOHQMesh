!
!////////////////////////////////////////////////////////////////////////
!
!      MeshSmoother.f90
!      Created: May 29, 2014 at 3:38 PM 
!      By: David Kopriva  
!
!      BASE CLASS FOR SMOOTHERS
!
!////////////////////////////////////////////////////////////////////////
!
      Module MeshSmootherClass
      USE SMMeshClass
      USE SMModelClass
      USE MeshBoundaryMethodsModule
      IMPLICIT NONE 
      
      TYPE :: MeshSmoother
!         
!        ========
         CONTAINS
!        ========
!         
         PROCEDURE  :: smoothMesh
         PROCEDURE  :: destruct
      END TYPE MeshSmoother
!
!     ========
      CONTAINS
!     ========
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE CollectBoundaryAndInterfaceNodes(allNodesIterator,boundaryNodesList)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTLinkedListIterator), POINTER :: allNodesIterator
         CLASS(FTLinkedList)        , POINTER :: boundaryNodesList
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(SMNode)  , POINTER :: currentNode => NULL()
         CLASS(FTObject), POINTER :: obj => NULL()
!
!        ------------------------------------------------------
!        Loop through all the nodes and add those whose
!        distance to a boundary is zero to the boundaryNodeList
!        ------------------------------------------------------
!
         CALL allNodesIterator % setToStart()
         DO WHILE ( .NOT.allNodesIterator % isAtEnd() )
            obj => allNodesIterator % object()
            CALL cast(obj,currentNode)
            IF ( IsOnBoundaryCurve(currentNode) .AND. &
                 currentNode%distToBoundary == 0.0_RP )     THEN
               CALL boundaryNodesList % add(obj)
            END IF 
            CALL allNodesIterator % moveToNext()
         END DO

      END SUBROUTINE CollectBoundaryAndInterfaceNodes
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE destruct( self )
         IMPLICIT NONE
         CLASS (MeshSmoother)   :: self
      END SUBROUTINE destruct
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE smoothMesh( self, mesh, model )
         IMPLICIT NONE
         CLASS (MeshSmoother)   :: self
         CLASS (SMMesh)      , POINTER :: mesh
         CLASS (SMModel)     , POINTER :: model
      END SUBROUTINE smoothMesh
      
      END MODULE MeshSmootherClass
