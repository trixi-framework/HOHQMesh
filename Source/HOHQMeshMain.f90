!
!////////////////////////////////////////////////////////////////////////
!
!      HOMeshMain.f90
!      Created: August 19, 2013 11:18 AM 
!      By: David Kopriva  
!
!      Main program for 2/3D meshing
!
!////////////////////////////////////////////////////////////////////////
!
      PROGRAM HOMeshMain 
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
!        --------------------
!        Mesh project storage
!        --------------------
!
         CLASS( MeshProject )  , POINTER :: project     => NULL()
         TYPE( MeshStatistics )          :: stats
!
!        ----
!        File
!        ----
!
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: controlFileName
         INTEGER                                 :: fUnit, ios
         INTEGER, EXTERNAL                       :: StdInFileUnitCopy, UnusedUnit
         TYPE(ControlFileReader)                 :: cfReader
!
!        -----
!        Other
!        -----
!         
         CHARACTER(LEN=8) :: version           = "3.5.19"
         LOGICAL          :: debug             = .FALSE.
         LOGICAL          :: didGenerate3DMesh = .FALSE.
         LOGICAL          :: shouldGenerate3D  = .FALSE.
         INTEGER          :: errorCode = NONE 
         INTEGER          :: k
         TYPE(FTTimer)    :: stopWatch
         
         CHARACTER(LEN=16)                       :: namesFmt = "(   7A16 )"
         CHARACTER(LEN=16)                       :: valuesFmt = '(  7F16.3)'
         CHARACTER(LEN=16)                       :: numb = "9"
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: str
         
         CLASS(FTObject)         , POINTER :: obj
         TYPE (FTValueDictionary), POINTER :: controlDict, modelDict
!
!        ***********************************************
!                             Start
!        ***********************************************
!
         CALL initializeFTExceptions
         CALL SetUpPreferences
         CALL ReadCommandLineArguments(version, debug, controlFileName)
         CALL cfReader % init()
!
!        ------------------------
!        Read in the control file
!        ------------------------
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
!
!        -----------------------------------------------------
!        Initialize the project and check the integrity of the
!        control file
!        -----------------------------------------------------
!
         ALLOCATE(project)
         
         CALL project % initWithDictionary( cfReader % controlDict )
         CALL trapExceptions !Abort on fatal exceptions
         
         obj              => cfReader % controlDict % objectForKey(key = "CONTROL_INPUT")
         controlDict      => valueDictionaryFromObject(obj)
         CALL controlDict % retain()
         
         shouldGenerate3D = shouldGenerate3DMesh(controlDict = controlDict)
         IF ( shouldGenerate3D )     THEN
            obj            => cfReader % controlDict % objectForKey(key = "MODEL")
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
         
         IF(PrintMessage) PRINT *, "Mesh generated"
!
!        -----------------------------
!        Gather and publish statistics
!        -----------------------------
!
         PRINT *, " "
         PRINT *, "2D Mesh Statistics:"
         PRINT *, "   Total time         = ", stopWatch % elapsedTime(TC_SECONDS)
         PRINT *, "   Number of nodes    = ", project % mesh % nodes    % COUNT()
         PRINT *, "   Number of Edges    = ", project % mesh % edges    % COUNT()
         PRINT *, "   Number of Elements = ", project % mesh % elements % COUNT()
!
!        ---------------------------------------
!        Write mesh quality statistics to a file
!        ---------------------------------------
!
         str = project % runParams % statsFileName
         CALL toLower(str)
         IF ( str /= "none" )     THEN
            CALL Write2DMeshStatistics(mesh          = project % mesh, &
                                       statsFileName = project % runParams % statsFileName)
         END IF
!
!        ---------------------------------
!        Write averages to standard output
!        ---------------------------------
!
         CALL ComputeMeshQualityStatistics( stats = stats, mesh  = project % mesh)
      
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

         CALL CheckMeshForDuplicateNodes(project % mesh)
!
!        -----------------------------------------
!        Generate a 3D Extrusion mesh if requested
!        -----------------------------------------
!         
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
               PRINT *, " "
               PRINT *, "3D Mesh Statistics:"
               PRINT *, "    Total time         = ", stopWatch % elapsedTime(TC_SECONDS)
               PRINT *, "    Number of nodes    = ", SIZE(project % hexMesh % nodes)
               PRINT *, "    Number of Elements = ", SIZE(project % hexMesh % elements)
               PRINT *
               
            ELSE
               IF(printMessage) PRINT *, "Hex mesh generation failed"
            END IF 
         END IF 
!
!        -------------------
!        Write the Plot file
!        -------------------
!
         str = project % runParams % plotFileName
         CALL toLower(str)
         IF( str /= "none" )     THEN
            IF( PrintMessage ) PRINT *, "Writing tecplot file..."
            
               IF ( didGenerate3DMesh )     THEN
               
                  IF ( project % runParams % plotFileFormat == SKELETON_FORMAT )     THEN
                     CALL WriteHex8SkeletonToTecplot( project % hexMesh, fName = project % runParams % plotFileName)
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
!
!        -------------------
!        Write the mesh file
!        -------------------
!
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
!
!        --------
!        Clean up
!        --------
!
         CALL releaseFTValueDictionary(controlDict)
         CALL releaseMeshProject(project)
         CALL destructFTExceptions
         IF( PrintMessage ) PRINT *, "Execution complete. Exit."
         
      END PROGRAM HOMeshMain
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE ReadCommandLineArguments(version, debug, controlFileName)  
         USE CommandLineReader
         USE ProgramGlobals
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CHARACTER(LEN=DEFAULT_CHARACTER_LENGTH) :: controlFileName
         CHARACTER(LEN=8)                      :: version
         LOGICAL                               :: debug
         
         
         IF ( CommandLineArgumentIsPresent("-version") )     THEN
            PRINT *, "HOMesh Version ", version
         END IF
         
         IF ( CommandLineArgumentIsPresent("-help") )     THEN
            PRINT *, "No help avalable yet. Sorry!"
            STOP
         END IF
         
         debug = .false.
         IF ( CommandLineArgumentIsPresent("-debug") )     THEN
            debug = .true.
         END IF
   
         PrintMessage = .false.
         IF ( CommandLineArgumentIsPresent("-verbose") )     THEN
            printMessage = .true.
         END IF
         
         controlFileName = "none"
         IF ( CommandLineArgumentIsPresent(argument = "-f") )     THEN
            controlFileName = StringValueForArgument(argument = "-f")
         END IF 
         
      END SUBROUTINE ReadCommandLineArguments
