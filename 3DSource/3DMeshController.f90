!
!////////////////////////////////////////////////////////////////////////
!
!      3DMeshController.f90
!      Created: April 1, 2013 3:16 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      Module MeshController3D
      USE FTValuedictionaryClass
      USE SimpleSweepModule
      USE MeshProjectClass
      USE SMMeshObjectsModule
      USE HexMeshObjectsModule

      IMPLICIT NONE
! 
!---------------------------------------------------------------------
! This module Coordinates the generation of a 3D Mesh from the 2D Mesh 
!---------------------------------------------------------------------
!
!     ---------
!     Constants
!     ---------
!
      CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: SM_3D_ALGORITHM_CHOICE_KEY = "AlgorithmChoice"
      CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: SM_ELEMENT_TYPE_KEY        = "elementType"
      CHARACTER(LEN=STRING_CONSTANT_LENGTH), PARAMETER :: SM_GENERATE3D_MESH_KEY     = "generate3DMesh"
!
!     ----------------
!     Module variables
!     ----------------
!
      TYPE(FTValueDictionary) :: hexMeshParametersDictionary
!
!     --------
!     The mesh
!     --------
!
      TYPE(StructuredHexMesh) :: hex8Mesh
!
!     ======== 
      CONTAINS
!     ========
!
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE InitMeshController3D  
         IMPLICIT NONE
         CALL hexMeshParametersDictionary % initWithSize(16)
      END SUBROUTINE InitMeshController3D
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE DestructMeshController3D  
         IMPLICIT NONE
         CALL hexMeshParametersDictionary % release()
          
      END SUBROUTINE DestructMeshController3D
!
!//////////////////////////////////////////////////////////////////////// 
! 
      LOGICAL FUNCTION shouldGenerate3DMesh(fUnit)
!
!     ---------------------------------------------
!     See if we should generate a 3D mesh, as given
!     by the presense of the Define3DMesh block
!     ---------------------------------------------
!
         IMPLICIT NONE  
         INTEGER :: fUnit
         INTEGER :: iOS
         
         REWIND(fUnit)
         CALL MoveToBlock("\begin{Define3DMesh}", fUnit, iOS )
         
         IF ( iOS == 0 )     THEN
            shouldGenerate3DMesh = .true. 
         ELSE 
            shouldGenerate3DMesh = .false. 
         END IF 
         CALL hexMeshParametersDictionary % addValueForKey( shouldGenerate3DMesh, SM_GENERATE3D_MESH_KEY )

      END FUNCTION shouldGenerate3DMesh
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE generate3DMesh(fUnit,project)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER                  :: fUnit
         TYPE(MeshProject)        :: project
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                                :: iOS
         INTEGER                                :: algorithmChoice
         CHARACTER(LEN=LINE_LENGTH)             :: meshAlgorithm, algorithmName
         CHARACTER(LEN=ERROR_MSG_STRING_LENGTH) :: msg
         CLASS(FTException), POINTER            :: exception
!
!        --------------------------------
!        Read in control file information
!        --------------------------------
!
         REWIND(fUnit)
         CALL MoveToBlock("\begin{Define3DMesh}", fUnit, iOS )
         
         IF( ios == 0 )     THEN
         
            CALL Read3DMeshBlock( fUnit, hexMeshParametersDictionary )
            
            algorithmName = hexMeshParametersDictionary % stringValueForKey( SM_3D_ALGORITHM_CHOICE_KEY, LINE_LENGTH )
            meshAlgorithm = "\begin{" //ADJUSTL(TRIM(algorithmName)) // "}"
            
            REWIND(fUnit)
            CALL MoveToBlock(meshAlgorithm, fUnit, iOS )
            IF( ios /= 0 )     THEN
               msg = "Missing algorithm block"
               exception => ReaderException("Block read error", msg, meshAlgorithm, "generate3DMesh")
               CALL throw(exception)
               CALL exception % release()
               CALL hexMeshParametersDictionary % release()
               RETURN
            END IF
            
            CALL Read3DAlgorithmBlock(fUnit,hexMeshParametersDictionary)
            
         END IF
!
!        ---------------------
!        Generate the Hex mesh
!        ---------------------
!
         algorithmName = hexMeshParametersDictionary % stringValueForKey(SM_3D_ALGORITHM_CHOICE_KEY,LINE_LENGTH)
         IF(algorithmName == SIMPLE_EXTRUSION_ALGORITHM_KEY)     THEN
            algorithmChoice = SIMPLE_EXTRUSION_ALGORITHM
         ELSE
            algorithmChoice = SIMPLE_ROTATION_ALGORITHM
         END IF 

         SELECT CASE ( algorithmChoice )
            CASE( SIMPLE_EXTRUSION_ALGORITHM, SIMPLE_ROTATION_ALGORITHM )
               CALL PerformSimpleMeshSweep(project,hex8Mesh,hexMeshParametersDictionary, algorithmChoice)
            CASE DEFAULT
               exception => ReaderException("Generate 3D Mesh Algorithm",&
                                            "Unknown algorithm specified", algorithmName, "generate3DMesh")
               CALL throw(exception)
               CALL exception % release()
         END SELECT 
         
      END SUBROUTINE generate3DMesh
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE Read3DMeshBlock( fUnit, dict ) 
!
!        Example block is:
!
!         \begin{Define3DMesh}
!            meshType  = "HEX"
!            algorithm = "SimpleExtrusion" or "SimpleRotation"
!         \end{Define3DMesh}
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER                  :: fUnit
         CLASS(FTValueDictionary) :: dict
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                     :: ios
         CHARACTER(LEN=LINE_LENGTH)  :: inputLine = " ", tmpString
         CLASS(FTException), POINTER :: exception
         
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         IF ( ios == 0 )     THEN
            tmpString = GetStringValue( inputLine )
            CALL dict % addValueForKey(tmpString,SM_ELEMENT_TYPE_KEY)
         ELSE
            exception => ReaderException("Read Variable","Error reading variable", "Element type", "Read3DMeshBlock")
            CALL throw(exception)
            CALL exception % release()
            RETURN 
         END IF 
         
         READ ( fUnit, FMT = '(a132)', IOSTAT = ios ) inputLine
         IF ( ios == 0 )     THEN
            tmpString = GetStringValue( inputLine )
            CALL dict % addValueForKey(tmpString,SM_3D_ALGORITHM_CHOICE_KEY)
         ELSE 
            exception => ReaderException("Read Variable","Error reading variable", "algorithm", "Read3DMeshBlock")
            CALL throw(exception)
            CALL exception % release()
           RETURN 
         END IF 

      END SUBROUTINE Read3DMeshBlock
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE Read3DAlgorithmBlock( fUnit, dict ) 
         USE SimpleSweepModule
!
!     -----------------------------------------
!     Select among the choices of 3D algorithms
!     -----------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER                  :: fUnit
         CLASS(FTValueDictionary) :: dict
!
!        ---------------
!        Local variables
!        ---------------
!
         CHARACTER(LEN=LINE_LENGTH)  :: algorithmName
         CLASS(FTException), POINTER :: exception
         
         algorithmName = dict % stringValueForKey(SM_3D_ALGORITHM_CHOICE_KEY,LINE_LENGTH)

         SELECT CASE ( TRIM(algorithmName) )
            CASE( SIMPLE_EXTRUSION_ALGORITHM_KEY ) 
               CALL ReadSimpleExtrusionBlock( fUnit, dict )
               IF ( errorCount() > 0 )     RETURN ! Don't deal with errors at this point
            CASE (SIMPLE_ROTATION_ALGORITHM_KEY)
               CALL ReadSimpleRotationBlock(fUnit = fUnit,dict = dict)
            CASE DEFAULT
               exception => ReaderException("Read Algorithm","Unknown algorithm specified", &
                                           algorithmName, "Read3DAlgorithmBlock")
               CALL throw(exception)
               CALL exception % release()
         END SELECT 

         
      END SUBROUTINE Read3DAlgorithmBlock

   END Module MeshController3D 