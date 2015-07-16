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
         USE MeshController3D
         USE MeshOutputMethods3D
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
!         CLASS(FTException), POINTER :: exception => NULL()
!         INTEGER                     :: errorSeverity = FT_ERROR_NONE
!
!        -----
!        Other
!        -----
!
         CLASS(FTObject) , POINTER :: obj => NULL()
         CLASS(SMElement), POINTER :: e => NULL()
         
         CHARACTER(LEN=8) :: version = "6.10.15"
         LOGICAL          :: debug = .FALSE., didGenerate3DMesh = .FALSE.
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
            CALL trapExceptions !Abort on fatal exceptions
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
         
            IF(PrintMessage) PRINT *, "Generate 2D mesh..."
            
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
            WRITE(statsFileUnit,*) "------------------------"
            WRITE(statsFileUnit,*) "2D Mesh Quality Measures"
            WRITE(statsFileUnit,*) "------------------------"
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

         CALL CheckMeshForDuplicateNodes(project % mesh)
!
!        -----------------------------------------
!        Generate a 3D Extrusion mesh if requested
!        -----------------------------------------
!
         CALL InitMeshController3D
         IF ( shouldGenerate3DMesh(fUnit) )     THEN
            IF(printMessage) PRINT *, "Sweeping quad mesh to Hex mesh..."
            
            CALL stopWatch % start()
            CALL generate3DMesh(fUnit,project) 
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
!        -----------------------------------
!        Write the mesh in requested formats
!        -----------------------------------
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
         
         IF( project % runParams % MeshFileName /= "None" )     THEN
            IF( PrintMessage ) PRINT *, "Writing mesh file"
         
            IF ( project % runParams % meshFileFormat == BASIC_MESH_FORMAT )     THEN
               PRINT *, "*** BSC Format needs to be implemented ***"
            ELSE
               IF ( didGenerate3DMesh )     THEN
                  CALL WriteISMHexMeshFile(mesh = hex8Mesh,&
                                           fName = project % runParams % MeshFileName,&
                                           model = project % model,&
                                           N = project % runParams % polynomialOrder,&
                                           version = project % runParams % meshFileFormat) 
               ELSE
                  CALL WriteISMMeshFile( project % mesh, project % runParams % MeshFileName, &
                                      project % model, &
                                      project % runParams % polynomialOrder, &
                                      project % runParams % meshFileFormat )
               END IF 
            END IF
            
         END IF
!
!        --------
!        Clean up
!        --------
!
         CLOSE( fUnit )
         CALL project % release()
         DEALLOCATE(project)
         
         CALL destructFTExceptions
         
      END PROGRAM SpecMeshMain
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE trapExceptions  
         USE SharedExceptionManagerModule
         IMPLICIT NONE 

         INTEGER                    :: errorSeverity = FT_ERROR_NONE
         TYPE(FTException), POINTER :: exception
          
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

      END SUBROUTINE trapExceptions
