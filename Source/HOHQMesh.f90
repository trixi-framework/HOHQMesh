!
!////////////////////////////////////////////////////////////////////////
!
!      HOQMesh.f90
!      Created: May 12, 2021 at 1:47 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
   Module HOHQMeshModule 
      USE MeshProjectClass
      USE FTTimerClass
      USE MeshGenerationMethods
      USE MeshOutputMethods
      USE FTTimerClass
      USE MeshCleaner
      USE MeshQualityAnalysisClass
      USE MeshController3D
      USE MeshOutputMethods3D
      USE ControlFileReaderClass
      USE FTValueDictionaryClass
      IMPLICIT NONE  
! 
!------------------------------------------------------------------- 
!                Main entry for running HOHQMesh 
!------------------------------------------------------------------- 
! 
!  ========
   CONTAINS  
!  ========
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE HOHQMesh(projectDict, project, stats, didGenerate3DMesh, test)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS( MeshProject )  , POINTER                      :: project           ! Input and output. Release when done
         TYPE( MeshStatistics )                 , INTENT(OUT) :: stats             ! Output
         LOGICAL                                , INTENT(IN)  :: test              ! Input
         LOGICAL                                , INTENT(out) :: didGenerate3DMesh ! Output
         TYPE (FTValueDictionary), POINTER                    :: projectDict       ! Input
!
!        ----
!        File
!        ----
!
         INTEGER                                 :: fUnit, ios
         INTEGER, EXTERNAL                       :: StdInFileUnitCopy, UnusedUnit
         TYPE(ControlFileReader)                 :: cfReader
!
!        -----
!        Other
!        -----
!         
         CHARACTER(LEN=8) :: version           = "05.03.21"
         LOGICAL          :: shouldGenerate3D  = .FALSE.
         INTEGER          :: errorCode         =  NONE 
         INTEGER          :: k
         TYPE(FTTimer)    :: stopWatch
         
         CHARACTER(LEN=16)                       :: namesFmt = "(   7A16 )"
         CHARACTER(LEN=16)                       :: valuesFmt = '(  7F16.3)'
         CHARACTER(LEN=16)                       :: numb = "9"
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: str
         
         CLASS(FTObject)         , POINTER :: obj
         TYPE (FTValueDictionary), POINTER :: modelDict, controlDict
!
!        -----------------------------------------------------
!        Initialize the project and check the integrity of the
!        control file
!        -----------------------------------------------------
!
         CALL project % initWithDictionary( projectDict )
         CALL trapExceptions !Abort on fatal exceptions
         
         obj              => projectDict % objectForKey(key = "CONTROL_INPUT")
         controlDict      => valueDictionaryFromObject(obj)
         
         shouldGenerate3D = shouldGenerate3DMesh(controlDict = controlDict)
         IF ( shouldGenerate3D )     THEN
            obj            => projectDict % objectForKey(key = "MODEL")
            modelDict      => valueDictionaryFromObject(obj)
            CALL modelDict % retain()
            CALL Check3DMeshParametersIntegrity(controlDict, modelDict) 
            CALL releaseFTValueDictionary(modelDict)
         END IF 
         CALL trapExceptions !Abort on fatal exceptions
!
!        -----------------
!        Generate the mesh
!        -----------------
!
         CALL stopWatch % start()
            CALL GenerateQuadMesh(project, errorCode)
         CALL stopwatch % stop()
         CALL trapExceptions !Abort on fatal exceptions
         
         IF(PrintMessage) PRINT *, "Mesh generated"
!
!        -----------------------------
!        Gather and publish statistics
!        -----------------------------
!
         IF(.NOT.test)     THEN 
            PRINT *, " "
            PRINT *, "2D Mesh Statistics:"
            PRINT *, "   Total time         = ", stopWatch % elapsedTime(TC_SECONDS)
            PRINT *, "   Number of nodes    = ", project % mesh % nodes    % COUNT()
            PRINT *, "   Number of Edges    = ", project % mesh % edges    % COUNT()
            PRINT *, "   Number of Elements = ", project % mesh % elements % COUNT()
         END IF 
!
!        ---------------------------------
!        Write averages to standard output
!        ---------------------------------
!
         CALL ComputeMeshQualityStatistics( stats = stats, mesh  = project % mesh)

         IF(.NOT.test)     THEN 
            WRITE(numb,FMT='(I3)') SIZE(stats % avgValues)
            namesFmt  = "(" // TRIM(numb) // "A16)"
            valuesFmt = "(A16," // TRIM(numb) // "(F16.8))"
            
            PRINT *, " "
            PRINT *, "Mesh Quality:"
            WRITE(6,namesFmt) "Measure", "Minimum", "Maximum", "Average", "Acceptable Low", "Acceptable High", "Reference"
            DO k = 1, SIZE(measureNames)       
               WRITE(6,valuesFmt) TRIM(measureNames(k)), stats % minValues(k), stats % maxValues(k), stats % avgValues(k),&
                                       acceptableLow(k), acceptableHigh(k), refValues(k)
            END DO
            PRINT *, " "
         END IF 

         CALL CheckMeshForDuplicateNodes(project % mesh)
!
!        -----------------------------------------------
!        Perform transformations on 2D mesh if requested
!        -----------------------------------------------
!
         CALL Perform2DMeshTransformations(project)
!
!        -----------------------------------------
!        Generate a 3D Extrusion mesh if requested
!        -----------------------------------------
!         
         didGenerate3DMesh = .FALSE.
         IF ( shouldGenerate3D )     THEN
            IF(printMessage) PRINT *, "Sweeping quad mesh to Hex mesh..."
            
            CALL stopWatch % start()
               CALL generate3DMesh( controlDict, project )
            CALL stopWatch % stop()
            
            CALL trapExceptions !Aborts on fatal exceptions
            didGenerate3DMesh = .TRUE.
            
            IF ( didGenerate3DMesh )     THEN
               IF(printMessage) PRINT *, "Hex mesh generated"
!
!              -----------------------------
!              Gather and publish statistics
!              -----------------------------
!
               IF(.NOT.test)     THEN 
                  PRINT *, " "
                  PRINT *, "3D Mesh Statistics:"
                  PRINT *, "    Total time         = ", stopWatch % elapsedTime(TC_SECONDS)
                  PRINT *, "    Number of nodes    = ", SIZE(project % hexMesh % nodes)
                  PRINT *, "    Number of Elements = ", SIZE(project % hexMesh % elements)
                  PRINT *
               END IF 
            ELSE
               IF(printMessage) PRINT *, "Hex mesh generation failed"
            END IF 
         END IF 
         
      END SUBROUTINE HOHQMesh
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE ReadControlFile(controlFileName, projectDict)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH), INTENT(IN)  :: controlFileName ! Input
         TYPE(FTValueDictionary), POINTER                     :: projectDict     ! Output, retained
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                                 :: fUnit, ios
         INTEGER, EXTERNAL                       :: StdInFileUnitCopy, UnusedUnit
         TYPE(ControlFileReader)                 :: cfReader
         
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: str
         
         CALL cfReader % init()
!
         ios = 0
         str = controlFileName
         CALL toLower(str)
         IF ( str == "none" )     THEN
            fUnit   = StdInFileUnitCopy( )
         ELSE
            fUnit = UnusedUnit()
            OPEN(UNIT = fUnit, FILE = controlFileName, STATUS = "OLD",  IOSTAT = ios)
            IF(ios /= 0)  fUnit = NONE 
         END IF
         
         IF ( ios == 0 )     THEN
            CALL cfReader % importFromControlFile(fileUnit = fUnit)
         ELSE
            PRINT *, "Unable to open input file: ", TRIM(controlFileName)
            STOP
         END IF 
         CLOSE(fUnit)
         
         projectDict => cfReader % controlDict
         CALL projectDict % retain()
         CALL destructControlFileReader(cfReader)
         
      END SUBROUTINE ReadControlFile
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE WritePlotFile(project, didGenerate3DMesh)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS( MeshProject ), POINTER :: project ! Input
         LOGICAL                       :: didGenerate3DMesh
!
!        ---------------
!        Local variables
!        ---------------
!
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: str
         
         str = project % runParams % plotFileName
         CALL toLower(str)
         
         IF( str /= "none" )     THEN
            IF( PrintMessage ) PRINT *, "Writing tecplot file..."
            
               IF ( didGenerate3DMesh )     THEN
               
                  IF ( project % runParams % plotFileFormat == SKELETON_FORMAT )     THEN
                     CALL WriteHex8SkeletonToTecplot( project % hexMesh, &
                                                      fName = project % runParams % plotFileName)
                  ELSE
                     CALL WriteHex8MeshToTecplot(hex8Mesh = project % hexMesh, &
                                                 fName    = project % runParams % plotFileName, &
                                                 N        = project % runParams % polynomialOrder)
                  END IF 
                 
               ELSE
               
                  IF ( project % runParams % plotFileFormat == SKELETON_FORMAT )     THEN
                     CALL WriteSkeletonToTecplot( project % mesh, project % runParams % plotFileName )
                  ELSE 
                     CALL WriteSEMMeshToTecplot(mesh  = project % mesh, &
                                                fName = project % runParams % plotFileName, &
                                                N     = project % runParams % polynomialOrder ) 
                  END IF 
                  
               END IF 
            IF( PrintMessage ) PRINT *, "Tecplot file written"
         END IF
         
      END SUBROUTINE WritePlotFile
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE WriteMeshFile(project, didGenerate3DMesh)
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CLASS( MeshProject ), POINTER :: project ! Input
         LOGICAL                       :: didGenerate3DMesh
!
!        ---------------
!        Local variables
!        ---------------
!
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: str
         
         str = project % runParams % MeshFileName
         CALL toLower(str)
         
         IF( str /= "none" )     THEN
            IF( PrintMessage ) PRINT *, "Writing mesh file..."
         
            IF ( project % runParams % meshFileFormat == BASIC_MESH_FORMAT )     THEN
               PRINT *, "*** BSC Format needs to be implemented ***"
            ELSE
               IF ( didGenerate3DMesh )     THEN
                  CALL WriteISMHexMeshFile(mesh    = project % hexMesh,&
                                           fName   = project % runParams % MeshFileName,&
                                           N       = project % runParams % polynomialOrder,&
                                           version = project % runParams % meshFileFormat) 
               ELSE
                  CALL WriteISMMeshFile( project % mesh, project % runParams % MeshFileName, &
                                         project % runParams % polynomialOrder, &
                                         project % runParams % meshFileFormat )
               END IF 
            END IF
            IF( PrintMessage ) PRINT *, "Mesh file written."
            
         END IF
         
      END SUBROUTINE WriteMeshFile
      
   END Module HOHQMeshModule
