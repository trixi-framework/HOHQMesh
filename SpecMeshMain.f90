!
!////////////////////////////////////////////////////////////////////////
!
!      SpecMeshMain.f90
!      Created: August 19, 2013 11:18 AM 
!      By: David Kopriva  
!
!      Main program for 2/3D meshing
!
!////////////////////////////////////////////////////////////////////////
!
      PROGRAM SpecMeshMain 
         USE MeshProjectClass
         USE FTTimerClass
         USE CommandLineReader
         USE MeshGenerationMethods
         USE MeshOutputMethods
         USE FTTimerClass
         USE MeshCleaner
         USE MeshQualityAnalysisClass
         IMPLICIT NONE
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
         INTEGER           :: fUnit, preferencesFileUnit, ios
         INTEGER, EXTERNAL :: StdInFileUnitCopy, UnusedUnit
!
!        ---------------
!        Error reporting
!        ---------------
!
         CLASS(FTException), POINTER :: exception => NULL()
         INTEGER                     :: errorSeverity = FT_ERROR_NONE
!
!        -----
!        Other
!        -----
!
         CLASS(FTObject) , POINTER :: obj => NULL()
         CLASS(SMElement), POINTER :: e => NULL()
         
         CHARACTER(LEN=8) :: version = "10.08.12"
         LOGICAL          :: debug = .FALSE.
         INTEGER          :: k
         INTEGER          :: statsFileUnit
         TYPE(FTTimer)    :: stopWatch
         CHARACTER(LEN=16):: namesFmt = "(   7A16 )", valuesFmt = '(  7F16.3)', numb = "9"
!
!        ***********************************************
!                             Start
!        ***********************************************
!
         CALL initializeFTExceptions
!
!        ----------------
!        Read preferences
!        ----------------
!
         preferencesFileUnit = UnusedUnit()
         OPEN( UNIT = preferencesFileUnit, FILE = "SpecMesh2DPreferences.txt", &
               STATUS = "OLD", IOSTAT = ios )
         IF( ios == 0 )     THEN
            CALL LoadPreferences(preferencesFileUnit)
            CLOSE(preferencesFileUnit)
         ELSE
            OPEN( UNIT = preferencesFileUnit, FILE = "SpecMesh2DPreferences.txt", &
                  STATUS = "NEW", IOSTAT = ios )
            CALL WriteDefaultPreferences(preferencesFileUnit)
            CLOSE(preferencesFileUnit)
         END IF
!
!        ---------------------------
!        Read command line Arguments
!        ---------------------------
!
         IF ( CommandLineArgumentIsPresent("-version") )     THEN
            PRINT *, "SpecMesh2D Version ", version
         END IF
         
         IF ( CommandLineArgumentIsPresent("-help") )     THEN
            PRINT *, "No help avalable yet"
            STOP
         END IF
         
         debug = .false.
         IF ( CommandLineArgumentIsPresent("-debug") )     THEN
            debug = .true.
         END IF
   
         PrintMessage = .false.
         IF ( CommandLineArgumentIsPresent("-verbose") )     THEN
            PrintMessage = .true.
         END IF
!
!        ----------------------
!        Initialize the project
!        ----------------------
!
         ALLOCATE(project)
         fUnit   = StdInFileUnitCopy( )
         IF( fUnit /= NONE )     THEN
         
            CALL project % initWithContentsOfFileUnit( fUnit )
            CLOSE( fUnit )
!
!           ----------------
!           Catch any errors
!           ----------------
!
            IF ( catch() )     THEN
               PRINT *, "The following errors were found when constructing the project:"
               PRINT *, " "
               
               DO
                  exception => popLastException()
                  IF ( .NOT.ASSOCIATED(exception) )     EXIT
                  CALL exception % printDescription(6)
                  errorSeverity = MAX(errorSeverity, exception % severity())
               END DO
               
               IF ( errorSeverity > FT_ERROR_WARNING )     THEN
                  STOP "The Errors were Fatal. Cannot generate mesh." 
               END IF 
            END IF 
            
         ELSE
            PRINT *, "Unable to open input file"
            STOP
         END IF
!
!        -----------------
!        Generate the mesh
!        -----------------
!
         CALL stopWatch % start()
         
            IF(PrintMessage) PRINT *, "Generate mesh..."
            
            CALL GenerateQuadMeshForProject( project )
            IF ( catch() )     THEN
               CALL printAllExceptions
               STOP 
            END IF 
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
         PRINT *, "Mesh Statistics:"
         PRINT *, "Total time         = ", stopWatch % elapsedTime(TC_SECONDS)
         PRINT *, "Number of nodes    = ", project % mesh % nodes    % COUNT()
         PRINT *, "Number of Edges    = ", project % mesh % edges    % COUNT()
         PRINT *, "Number of Elements = ", project % mesh % elements % COUNT()
!
!        ---------------------------------------
!        Write mesh quality statistics to a file
!        ---------------------------------------
!
         
         IF ( project % runParams % statsFileName /= "None" )     THEN
         
            statsFileUnit = UnusedUnit()
            OPEN(FILE=project % runParams % statsFileName,UNIT=statsFileUnit)
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
               CALL badElements % release()
               IF(badElements % isUnreferenced()) DEALLOCATE(badElements)
               
            ELSE IF (PrintMessage)     THEN 
               PRINT *, "********* Elements are OK *********"
            END IF 
            
            WRITE(statsFileUnit,*) " "
            WRITE(statsFileUnit,*) "---------------------"
            WRITE(statsFileUnit,*) "Mesh Quality Measures"
            WRITE(statsFileUnit,*) "---------------------"
            WRITE(statsFileUnit,*) " "
            CALL OutputMeshQualityMeasures( mesh = project % mesh, fUnit  = statsFileUnit )
            CLOSE(statsFileUnit)
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
!
!        -----------------------------------
!        Write the mesh in requested formats
!        -----------------------------------
!
         IF( project % runParams % plotFileName /= "None" )     THEN
            IF( PrintMessage ) PRINT *, "Writing tecplot file..."
               CALL WriteToTecplot( project % mesh, project % runParams % plotFileName )
            IF( PrintMessage ) PRINT *, "Tecplot file written"
         END IF
         
         IF( project % runParams % MeshFileName /= "None" )     THEN
            IF( PrintMessage ) PRINT *, "Writing mesh file"
         
            IF ( project % runParams % meshFileFormat == BASIC_MESH_FORMAT )     THEN
               PRINT *, "*** BSC Format needs to be implemented ***"
            ELSE
               CALL WriteISMMeshFile( project % mesh, project % runParams % MeshFileName, &
                                   project % model, &
                                   project % runParams % polynomialOrder, &
                                   project % runParams % meshFileFormat )
            END IF
            
         END IF
!
!        --------
!        Clean up
!        --------
!
         CALL CheckMeshForDuplicateNodes(project % mesh)
         
         CALL project % release()
         DEALLOCATE(project)
         
         CALL destructFTExceptions
         
      END PROGRAM SpecMeshMain
