!
!////////////////////////////////////////////////////////////////////////
!
!      UnstructuredMeshClassBase.f90
!      Created: August 21, 2013 1:08 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      Module MeshOperationsModule
      USE SMConstants
      USE QuadTreeGridClass
      USE SMMeshObjectsModule
      USE FTobjectClass
      USE FTLinkedListClass
      USE FTLinkedListIteratorClass
      USE SMMeshClass
      IMPLICIT NONE 

!
!     ========
      CONTAINS 
!     ========
!
!@mark -
!
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CheckMeshForDuplicateNodes( mesh ) 
      IMPLICIT NONE 
      CLASS(SMMesh)  :: mesh
!
!     ------------------------------------------------------------
!     This routine is intended for debugging purposes ONLY because
!     it does a simple linear search through ALL of the nodes. It
!     is intended to check for the integrity of the mesh, but not
!     for use on grids of large size or anything else.
!     ------------------------------------------------------------
!
      CLASS(SMNode)            , POINTER :: node     => NULL()
      CLASS(FTObject)          , POINTER :: obj      => NULL()
      CLASS(FTLinkedListRecord), POINTER :: llRecord => NULL()
     
      REAL(KIND=RP), DIMENSION(3)    :: xTest, x, d
      INTEGER                        :: idTest, id
      LOGICAL                        :: duplicateWasFound
!      
      
      PRINT *, "Testing for duplicate nodes..."
      
      duplicateWasFound = .false.
      CALL mesh % nodesIterator % setToStart()
      DO WHILE( .NOT.mesh % nodesIterator % isAtEnd())
         obj => mesh % nodesIterator % object()
         CALL castToSMNode(obj,node)
         xTest = node % x; idTest = node % id
!
!        ----------------------------------
!        Now search through remaining nodes
!        ----------------------------------
!
         llRecord => mesh % nodesIterator % currentRecord()
         llRecord => llRecord % next
         DO WHILE( ASSOCIATED(llREcord) )
            obj => llRecord % recordObject
            CALL castToSMNode(obj,node)
            
            x    = node % x; id   = node % id
            d    = ABS(x-xTest)
            
            IF( MAXVAL(d) < 1.0d-10 )     THEN
               PRINT *, "Duplicate Node at ",x, " with ids", idTest, id, " and level ", node % level
               duplicateWasFound = .true.
            END IF
            
            llRecord => llrecord % next
         END DO  
         
         CALL mesh % nodesIterator % moveToNext() 
      END DO 
       
      IF( .NOT.duplicateWasFound ) PRINT *, "No duplicate nodes found. Mesh OK"
      PRINT *, "Test complete."
      
      END SUBROUTINE CheckMeshForDuplicateNodes

      END MODULE MeshOperationsModule
