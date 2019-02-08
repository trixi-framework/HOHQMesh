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
         
         INTERFACE
            SUBROUTINE Write2DMeshStatistics(project)
               USE FTMutableObjectArrayClass
               USE MeshQualityAnalysisClass
               USE MeshProjectClass
               IMPLICIT NONE
               CLASS( MeshProject ), POINTER :: project
            END SUBROUTINE Write2DMeshStatistics
         END INTERFACE 
!
!        --------------------
!        Mesh project storage
!        --------------------
!
         CLASS( MeshProject )        , POINTER :: project     => NULL()
         CLASS(FTMutableObjectArray) , POINTER :: badElements => NULL()
         TYPE( MeshStatistics )                :: stats
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
         CHARACTER(LEN=8) :: version           = "2.4.19"
         LOGICAL          :: debug             = .FALSE.
         LOGICAL          :: didGenerate3DMesh = .FALSE.
         INTEGER          :: k
         TYPE(FTTimer)    :: stopWatch
         CHARACTER(LEN=16):: namesFmt = "(   7A16 )", valuesFmt = '(  7F16.3)', numb = "9"
         
         CLASS(FTObject)         , POINTER :: obj
         CLASS(FTValueDictionary), POINTER :: controlDict
         
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
         IF ( controlFileName == "None" )     THEN
            fUnit   = StdInFileUnitCopy( )
         ELSE
            fUnit = UnusedUnit()
            OPEN(UNIT = fUnit, FILE = controlFileName, STATUS = "OLD",  IOSTAT = ios)
            IF(ios /= 0)  fUnit = NONE 
         END IF
         
         IF ( ios == 0 )     THEN
            CALL cfReader % importFromControlFile(fileUnit = fUnit)
         ELSE
            PRINT *, "Unable to open input file"
            STOP !TODO: be more gracious
         END IF 
         CLOSE(fUnit)
!
!        ----------------------
!        Initialize the project
!        ----------------------
!
         ALLOCATE(project)
         
         CALL project % initWithDictionary( cfReader % controlDict )
         CALL trapExceptions !Abort on fatal exceptions
!
!        -----------------
!        Generate the mesh
!        -----------------
!
         CALL stopWatch % start()
         
            IF(PrintMessage) PRINT *, "Generate 2D mesh..."
            
            CALL GenerateQuadMeshForProject( project )
            CALL trapExceptions !Abort on fatal exceptions
!
!           ------------------------
!           Perform topology cleanup
!           ------------------------
!
            CALL PerformTopologyCleanup(project % mesh)
!
!           ------------------------
!           Smooth mesh if requested
!           ------------------------
!
            IF(Associated(project % smoother))     THEN
               IF(PrintMessage) PRINT *, "   Begin Smoothing..."
               CALL project % smoother % smoothMesh(  project % mesh, project % model )
               IF(PrintMessage) PRINT *, "   Smoothing done."
            END IF
!
!           -------------
!           Clean up mesh
!           -------------
!
            IF(PrintMessage) PRINT *, "   Performing final mesh cleanup..."
               CALL PerformFinalMeshCleanup( project % mesh, project % model )
            IF(PrintMessage) PRINT *, "   Mesh cleanup done."
!
!           --------------------------------------
!           Smooth mesh one more time if requested
!           --------------------------------------
!
            IF(Associated(project % smoother))     THEN
               IF(PrintMessage) PRINT *, "   Begin Final Smoothing..."
               CALL project % smoother % smoothMesh(  project % mesh, project % model )
               IF(PrintMessage) PRINT *, "   final Smoothing done."
            END IF
            
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
         
         IF ( project % runParams % statsFileName /= "None" )     THEN
            CALL Write2DMeshStatistics(project)
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
         obj => cfReader % controlDict % objectForKey(key = "CONTROL_INPUT")
         controlDict => valueDictionaryFromObject(obj)
         
         IF ( shouldGenerate3DMesh( controlDict) )     THEN
            IF(printMessage) PRINT *, "Sweeping quad mesh to Hex mesh..."
            
            CALL stopWatch % start()
               CALL generate3DMesh(controlDict,project)
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
               PRINT *, "    Number of nodes    = ", SIZE(hex8Mesh % nodes)
               PRINT *, "    Number of Faces    = ", SIZE(hex8Mesh % faces) + SIZE(hex8Mesh % capFaces)
               PRINT *, "    Number of Elements = ", SIZE(hex8Mesh % elements)
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
         IF( project % runParams % plotFileName /= "None" )     THEN
            IF( PrintMessage ) PRINT *, "Writing tecplot file..."
               IF ( didGenerate3DMesh )     THEN
                  CALL WriteHex8MeshToTecplot(hex8Mesh,fName = project % runParams % plotFileName)
               ELSE
                  CALL WriteToTecplot( project % mesh, project % runParams % plotFileName )
               END IF 
            IF( PrintMessage ) PRINT *, "Tecplot file written"
         END IF
!
!        -------------------
!        Write the mesh file
!        -------------------
!
         IF( project % runParams % MeshFileName /= "None" )     THEN
            IF( PrintMessage ) PRINT *, "Writing mesh file..."
         
            IF ( project % runParams % meshFileFormat == BASIC_MESH_FORMAT )     THEN
               PRINT *, "*** BSC Format needs to be implemented ***"
            ELSE
               IF ( didGenerate3DMesh )     THEN
                  CALL WriteISMHexMeshFile(mesh    = hex8Mesh,&
                                           fName   = project % runParams % MeshFileName,&
                                           N       = project % runParams % polynomialOrder,&
                                           version = project % runParams % meshFileFormat) 
               ELSE
                  CALL WriteISMMeshFile( project % mesh, project % runParams % MeshFileName, &
                                         project % model, &
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
         CLOSE( fUnit )
         CALL release(project)
         
         CALL destructFTExceptions
         IF( PrintMessage ) PRINT *, "Execution complete. Exit."
         
      END PROGRAM HOMeshMain
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE trapExceptions  
         USE SharedExceptionManagerModule
         IMPLICIT NONE 

         INTEGER                    :: errorSeverity = FT_ERROR_NONE
         TYPE(FTException), POINTER :: exception
         
         errorSeverity = FT_ERROR_NONE
         
         IF ( catch() )     THEN
            PRINT *
            PRINT *, "------------------------------------------------------------------"
            PRINT *
            PRINT *, "The following errors were found when constructing the project:"
            
            DO
               exception => popLastException()
               IF ( .NOT.ASSOCIATED(exception) )     EXIT
               CALL exception % printDescription(6)
               errorSeverity = MAX(errorSeverity, exception % severity())
            END DO
            PRINT *
            PRINT *, "------------------------------------------------------------------"
            PRINT *
            
            IF ( errorSeverity > FT_ERROR_WARNING )     THEN
               STOP "The Errors were Fatal. Cannot generate mesh." 
            END IF 
         END IF 

      END SUBROUTINE trapExceptions
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE SetUpPreferences
      USE ProgramGlobals
         IMPLICIT NONE
         INTEGER           :: preferencesFileUnit
         INTEGER, EXTERNAL :: UnusedUnit
         INTEGER           :: ios
!
!        ----------------------------------------------------
!        Read in the preferences file if it exists, write out
!        default values if it doesn't
!        ----------------------------------------------------
!
         preferencesFileUnit = UnusedUnit()
         OPEN( UNIT = preferencesFileUnit, FILE = "HOMesh2DPreferences.txt", &
               STATUS = "OLD", IOSTAT = ios )
         IF( ios == 0 )     THEN
            CALL LoadPreferences(preferencesFileUnit)
            CLOSE(preferencesFileUnit)
         ELSE
            OPEN( UNIT = preferencesFileUnit, FILE = "HOMesh2DPreferences.txt", &
                  STATUS = "NEW", IOSTAT = ios )
            CALL WriteDefaultPreferences(preferencesFileUnit)
            CLOSE(preferencesFileUnit)
         END IF
         
      END SUBROUTINE SetUpPreferences
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
         
         controlFileName = "None"
         IF ( CommandLineArgumentIsPresent(argument = "-f") )     THEN
            controlFileName = StringValueForArgument(argument = "-f")
         END IF 
         
      END SUBROUTINE ReadCommandLineArguments
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE Write2DMeshStatistics(project)
         USE FTMutableObjectArrayClass
         USE MeshQualityAnalysisClass
         USE MeshProjectClass
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS( MeshProject ), POINTER :: project
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(FTMutableObjectArray) , POINTER :: badElements => NULL()
         INTEGER                               :: statsFileUnit, k
         CLASS(FTObject)             , POINTER :: obj => NULL()
         CLASS(SMElement)            , POINTER :: e   => NULL()
         
         OPEN(FILE = project % runParams % statsFileName, UNIT = statsFileUnit)
         badElements => BadElementsInMesh( mesh  = project % mesh)
         
         IF ( ASSOCIATED(POINTER = badElements) )     THEN
            PRINT *, badElements % COUNT()," Bad element(s) Found"
            WRITE(statsFileUnit,*) " "
            WRITE(statsFileUnit,*) "----------------"
            WRITE(statsFileUnit,*) "Bad Element Info"
            WRITE(statsFileUnit,*) "----------------"
            WRITE(statsFileUnit,*) " "
            
            DO k = 1, badElements % COUNT()
               obj => badElements % objectAtIndex(indx = k)
               CALL cast(obj,e)
               CALL PrintBadElementInfo( e, statsFileUnit )
            END DO
            CALL release(badElements)
            
         ELSE IF (PrintMessage)     THEN 
            PRINT *, "********* Elements are OK *********"
         END IF 
         
         WRITE(statsFileUnit,*) " "
         WRITE(statsFileUnit,*) "------------------------"
         WRITE(statsFileUnit,*) "2D Mesh Quality Measures"
         WRITE(statsFileUnit,*) "------------------------"
         WRITE(statsFileUnit,*) " "
         CALL OutputMeshQualityMeasures( mesh = project % mesh, fUnit  = statsFileUnit )
         CLOSE(statsFileUnit)
         
      END SUBROUTINE Write2DMeshStatistics
