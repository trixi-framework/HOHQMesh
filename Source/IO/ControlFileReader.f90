!
!////////////////////////////////////////////////////////////////////////
!
!      ControlFileReader.f90
!      Created: May 29, 2018 at 3:42 PM 
!      By: David Kopriva 
!      All Rights Reserved.
!
!      The control file reader parses the control file and returns
!      a Control file dictionary.
!
!      A Control file dictionary contains the keys
!         TYPE
!         CONTROL_INPUT
!         MODEL
!
!      TYPE is a string naming the type (class) of object stored
!
!      The CONTROL_INPUT dictionary contains the keys
!         TYPE
!         RUN_PARAMETERS
!         MESH_PARAMETERS
!         SPRING_SMOOTHER
!         REFINEMENT_REGIONS
!         SCALE_TRANSFORMATION
!         ROTATION_TRANSFORMATION
!         SIMPLE_EXTRUSION
!         SIMPLE_ROTATION
!         SWEEP_ALONG_CURVE
!
!      The MODEL dictionary contains the keys
!         TYPE
!         OUTER_BOUNDARY
!         INNER_BOUNDARIES
!         SWEEP_CURVE
!         SWEEP_SCALE_FACTOR
!         TOPOGRAPHY
!      
!      The OUTER_BOUNDARY dictionary contains the keys
!         TYPE
!         LIST
!           The list contains dictionaries describing
!               PARAMETRIC_EQUATION_CURVE
!               SPLINE_CURVE
!               END_POINTS_LINE
!
!      The INNER_BOUNDARIES dictionary contains the keys
!         TYPE
!         LIST
!            The list contains CHAIN dictionaries
!      
!      A CHAIN dictionary contains the keys
!         TYPE
!         LIST
!            The list will contain dictionaries of type
!                PARAMETRIC_EQUATION_CURVE
!                SPLINE_CURVE
!                END_POINTS_LINE
!                CIRCULAR_ARC
!
!      A PARAMETRIC_EQUATION_CURVE dictionary contains the keys
!         TYPE
!         name
!         xEqn
!         yEqn
!         zEqn
!
!      A SPLINE_CURVE block contains the keys
!         TYPE
!         name
!         SPLINE_DATA
!
!      SPLINE_DATA block contains keys and data
!         nKnots
!         t_1 x_1 y_1 z_1
!         t_2 x_2 y_2 z_2
!         ...
!         t_nKnots x_nKnots y_nKnots z_nKnots
!
!      An END_POINTS_LINE has the following keys
!         TYPE
!         name
!         xStart
!         xEnd
!
!      A CIRCULAR_ARC block contains
!
!         TYPE
!         name
!         units
!         center
!         radius
!         start angle
!         end angle
!
!      REFINEMENT_REGIONS dictionary contains the keys
!         TYPE
!         LIST
!            LIST is a list of 
!                REFINEMENT_CENTER
!                REFINEMENT_LINE
!
!      A REFINEMENT_CENTER contains the keys
!         TYPE
!         center
!         h
!         w
!
!      A REFINEMENT_LINE contains the keys
!         TYPE
!         xStart
!         xEnd
!         h
!         w
!      
!      A ROTATION_TRANSFORMATION contains the keys
!         TYPE
!         direction
!         rotationPoint
!
!      A SCALE_TRANSFORMATION contains the keys
!         TYPE
!         origin
!         scaleFactor
!      
!      The SWEEP_CURVE dictionary contains the keys
!         TYPE
!         LIST
!           The list contains dictionaries describing
!               PARAMETRIC_EQUATION_CURVE
!               SPLINE_CURVE
!               END_POINTS_LINE
!      
!      The SWEEP_SCALE_FACTOR dictionary contains the keys
!         TYPE
!         LIST
!           The list contains dictionaries describing
!               PARAMETRIC_EQUATION
!
!        But the equation definitions contain only one equation r(t) = ...
!
!      the TOPOGRAPHY dictionary contains the keys
!         TYPE
!         eqn (for equation defined topography)
!         
!      The SIMPLE_EXTRUSION  block is defined in SimpleSweep.f90
!      The SIMPLE_ROTATION   block is defined in SimpleSweep.f90
!      The SWEEP_ALONG_CURVE block is defined in SweeperClass.f90
! 
!////////////////////////////////////////////////////////////////////////
!
      Module ControlFileReaderClass
      
      USE FTValueDictionaryClass
      USE FTLinkedListClass
      USE FTExceptionClass
      USE SharedExceptionManagerModule
      USE FTStackClass
      USE ErrorTypesModule
      USE FTStringSetClass
      
      IMPLICIT NONE 
      
      TYPE ControlFileReader
         TYPE(FTValueDictionary), POINTER :: controlDict
!
!        --------
         CONTAINS
!        --------
!
         PROCEDURE :: init     => initControlFileReader
         PROCEDURE :: destruct => destructControlFileReader
         PROCEDURE :: importFromControlFile
      END TYPE ControlFileReader
      
      INTEGER, PARAMETER :: CFR_STRING_LENGTH = 132
      
      CHARACTER(LEN=CFR_STRING_LENGTH) :: blockStack(5)
      INTEGER                          :: blockStackTop
      TYPE(FTStringSet)                :: blocksWithListsSet
      CHARACTER(LEN=20)                :: blocksWithLists(7) =      &
                                          ["OUTER_BOUNDARY      ",  &
                                           "REFINEMENT_REGIONS  ",  &
                                           "INNER_BOUNDARIES    ",  &
                                           "INTERFACE_BOUNDARIES",  &
                                           "SWEEP_CURVE         ",  &
                                           "SWEEP_SCALE_FACTOR  ",  &
                                           "CHAIN               "]
!
!     ========      
      CONTAINS 
!     ========
!
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initControlFileReader(self)  
         IMPLICIT NONE
         CLASS(ControlFileReader) :: self
         
         ALLOCATE(self % controlDict)
         CALL self % controlDict % initWithSize(sze = 32)
         CALL self % controlDict % addValueForKey(s = "root",key = "TYPE")
         CALL initializeFTExceptions
         CALL blocksWithListsSet % initWithStrings(strings = blocksWithLists)
         blockStack    = ""
         blockStackTop = 0
         
      END SUBROUTINE initControlFileReader
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE destructControlFileReader(self)  
         IMPLICIT NONE  
         CLASS(ControlFileReader) :: self
         
         CALL releaseFTValueDictionary(self % controlDict)
         CALL destructFTStringSet( blocksWithListsSet)
         blockStack    = ""
         blockStackTop = 0
         
      END SUBROUTINE destructControlFileReader
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE importFromControlFile(self, fileUnit)  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(ControlFileReader) :: self
         INTEGER                  :: fileUnit
!
!        ---------------
!        Local Variables
!        ---------------
!
         CLASS(FTObject), POINTER           :: obj
         CHARACTER(LEN=CFR_STRING_LENGTH)   :: objectName
         
         obj        => self % controlDict
         objectName = "root"

         CALL performImport(fileUnit   = fileUnit, &
                            collection = obj,      &
                            objectName = objectName)

      END SUBROUTINE importFromControlFile
!
!//////////////////////////////////////////////////////////////////////// 
! 
      RECURSIVE SUBROUTINE performImport(fileUnit, collection, objectName)  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER, INTENT(IN)       :: fileUnit
         CHARACTER(LEN=*)          :: objectName
         CLASS(FTObject), POINTER  :: collection
!
!        ---------------
!        Local Variables
!        ---------------
!
         CLASS(FTValueDictionary), POINTER :: dict
         CHARACTER(LEN=CFR_STRING_LENGTH)  :: line
         INTEGER                           :: iStat
         TYPE (FTException)      , POINTER :: exception

         IF(catch()) RETURN  
         
         DO
           READ(fileUnit,"(A)", IOSTAT = iStat, END = 1000) line

           IF(iStat /= 0)   EXIT
           CALL replaceTabs(line)
            
            IF ( INDEX(STRING = line, SUBSTRING = "begin{") > 0)     THEN !Start reading a block
               CALL startNewCollectionInCollection(fileUnit,line,collection)
               IF(catch()) EXIT 
               
            ELSE IF (INDEX(STRING = line, SUBSTRING = "end{") > 0)     THEN
               IF(INDEX(STRING = line, SUBSTRING = "end{FILE}") > 0)   RETURN 
               CALL completeBlock(line,objectName)
               RETURN ! Done with this block, one way or another
               
            ELSE IF (TRIM(line) == "" .OR. line(1:1) == "%") THEN   !Skip blank lines 
               CYCLE
            ELSE IF (INDEX(STRING = line, SUBSTRING = "{") > 0 .OR. &
                     INDEX(STRING = line, SUBSTRING = "}") > 0)     THEN 
               ALLOCATE(exception)
               CALL exception % initFatalException( "Syntax error in control file line: "// TRIM(ADJUSTL(line)) // &
                                                     ". Commands are lower case.")
               CALL throw(exceptionToThrow = exception)
               CALL releaseFTException(exception)
               RETURN 
            ELSE 
               dict => valueDictionaryFromObject(collection)
               CALL readBlock(fileUnit  = fileUnit, &
                              blockDict = dict, &
                              firstLine = line,    &
                              blockName = objectName)
               IF(catch()) EXIT 
               RETURN 
            END IF
         END DO
         
1000     CONTINUE

      END SUBROUTINE performImport
!
!//////////////////////////////////////////////////////////////////////// 
! 
      RECURSIVE SUBROUTINE startNewCollectionInCollection(fileUnit, line, collection)  
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER, INTENT(IN)       :: fileUnit
         CHARACTER(LEN=*)          :: line
         CLASS(FTObject), POINTER  :: collection
!
!        ---------------
!        Local variables
!        ---------------
!
         CHARACTER(LEN=CFR_STRING_LENGTH)  :: objectName
         CLASS(FTObject)         , POINTER :: obj
         TYPE (FTValueDictionary), POINTER :: newDict
         CLASS(FTLinkedList)     , POINTER :: newList
!
!        ---------------------------------------
!        New block to add to current collection 
!        ---------------------------------------
!
         objectName                = parseObjectName(line)
         blockStackTop             = blockStackTop + 1
         blockStack(blockStackTop) = objectName

         ALLOCATE(newDict)
         CALL newDict % initWithSize(sze = 32)
         CALL newDict % addValueForKey(s = objectName,key = "TYPE")
         obj => newDict
         CALL addObjectToCollection(obj, collection, objectName)
!
!        --------------------------------------------
!        Certain blocks have lists of objects in them
!        --------------------------------------------
!
          IF( blocksWithListsSet % containsString(objectName))     THEN 
              ALLOCATE(newList)
              CALL newList % init()
              obj => newList
              CALL newDict % addObjectForKey(obj,"LIST")
              CALL releaseFTLinkedList(self = newList)
              
              IF(objectName == "CHAIN")     THEN ! Read the name of the chain
                 READ(fileUnit,"(A)", END = 1000) line
                 CALL replaceTabs(line)
                 CALL addKeyAndValueFromLineToDict(blockDict = newDict,line = line)
              END IF
              obj => newList
         END IF
         
         CALL releaseFTValueDictionary(self = newDict)
         CALL performImport(fileUnit, obj, objectName)
1000     CONTINUE
         
      END SUBROUTINE startNewCollectionInCollection
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE completeBlock(line, objectName)
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CHARACTER(LEN=*) :: line, objectName
!
!        ---------------
!        Local Variables
!        ---------------
!
         TYPE (FTException)      , POINTER :: exception
         CHARACTER(LEN=CFR_STRING_LENGTH)  :: foundName
         CHARACTER(LEN=132)                :: msg
         
         foundName = parseObjectName(line)
         IF(foundName /= blockStack(blockStackTop))     THEN
            msg = "Block termination error. Expected: "//TRIM(objectName)//"; Found: "//TRIM(foundName)
            PRINT *, msg
            ALLOCATE(exception)
            CALL exception % initFatalException(msg)
            CALL throw(exceptionToThrow = exception)
            CALL releaseFTException(self = exception)
            RETURN 
         END IF 
         blockStack(blockStackTop) = ""
         blockStackTop = blockStackTop - 1
      END SUBROUTINE completeBlock
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE addObjectToCollection(obj, collection, objectName)  
          IMPLICIT NONE
          CHARACTER(LEN=*)         :: objectName
          CLASS(FTObject), POINTER :: collection, obj
          
          SELECT TYPE (c => collection) 
            TYPE IS (FTValueDictionary)
              CALL c % addObjectForKey(obj, objectName)
            TYPE IS (FTLinkedList)
               CALL c % add(obj)
            CLASS DEFAULT 
            
         END SELECT 

      END SUBROUTINE addObjectToCollection
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE keyAndValueOnLine(line, key, lValue)  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CHARACTER(LEN = CFR_STRING_LENGTH) :: key, lValue, line
!
!        ---------------
!        Local Variables
!        ---------------
!
         INTEGER :: indxOfEqual
         
         indxOfEqual = INDEX(STRING = line, SUBSTRING = "=")
         IF ( indxOfEqual <= 1 )     THEN
            lValue = ""
            key    = "" 
         ELSE 
            key    = TRIM(ADJUSTL(line(1:indxOfEqual-1))) 
            lValue = TRIM(ADJUSTL(line(indxOfEqual+1:)))
         END IF 
         
      END SUBROUTINE keyAndValueOnLine
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE readBlock(fileUnit, blockDict, firstLine, blockName)  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER                           :: fileUnit
         CLASS(FTValueDictionary), POINTER :: blockDict
         CHARACTER(LEN=*)                  :: firstLine,blockName
         CHARACTER(LEN=132)                :: errMsg
!
!        ---------------
!        Local Variables
!        ---------------
!
         INTEGER                          :: s, e
         CHARACTER(LEN=CFR_STRING_LENGTH) :: line
         TYPE (FTException)     , POINTER :: exception
         
         errMsg = ""
         CALL addKeyAndValueFromLineToDict(blockDict = blockDict, &
                             line      = firstLine)
         DO
           READ(fileUnit,"(A)", END = 1000) line
           CALL replaceTabs(line)
!
!           -------------
!           Check for end
!           -------------
!
            s = INDEX(STRING = line, SUBSTRING = "end{")
            IF ( s > 0 )     THEN
               s = s + 3
               e =  INDEX(STRING = line, SUBSTRING = "}")
               IF(line(s+1:e-1) /= TRIM(blockName))     THEN
                  errMsg = "Improper block termination. Expected "// TRIM(blockName)// "; found "// line(s+1:e-1)
                  PRINT *, errMsg
                  ALLOCATE(exception)
                  CALL exception % initFatalException(errMsg)
                  CALL throw(exceptionToThrow = exception)
                  CALL releaseFTException(self = exception)
               END IF 
               blockStack(blockStackTop) = ""
               blockStackTop = blockStackTop - 1
               RETURN 
            END IF
!
!           -----------------------------------------------
!           Spline curve definitions contain the nodes and 
!           values
!           -----------------------------------------------
!
            s = INDEX(STRING = line, SUBSTRING = "\begin{SPLINE_DATA}")
            IF ( s > 0 )     THEN
               CALL readSplineData(fileUnit, blockDict)
               CYCLE 
            END IF 
!
!           ------------------
!           Otherwise carry on
!           ------------------
!
            CALL addKeyAndValueFromLineToDict(blockDict = blockDict, &
                                              line      = line)
         END DO 
         
1000     CONTINUE 
         
      END SUBROUTINE readBlock
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE addKeyAndValueFromLineToDict(blockDict,line)  
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         TYPE(FTValueDictionary) :: blockDict
         CHARACTER(LEN=*)        :: line
!
!        ---------------
!        Local variables
!        ---------------
!
         CHARACTER(LEN=CFR_STRING_LENGTH) :: key, lValue
         
         CALL keyAndValueOnLine(line   = line, &
                                key    = key,  &
                                lValue = lValue)
         CALL blockDict % addValueForKey(lValue, key)
         
      END SUBROUTINE addKeyAndValueFromLineToDict
!
!//////////////////////////////////////////////////////////////////////// 
! 
      FUNCTION parseObjectName(line) RESULT(objName)
         IMPLICIT NONE  
!
!        ---------
!        Arguments
!        ---------
!
         CHARACTER(LEN=*)                 :: line
         CHARACTER(LEN=CFR_STRING_LENGTH) :: objName
         INTEGER                          :: l, r
         
         l = INDEX(STRING = line, SUBSTRING = "{")
         r = INDEX(STRING = line, SUBSTRING = "}")
         
         objName = line(l+1:r-1)
         
      END FUNCTION parseObjectName
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE replaceTabs(line)  
         IMPLICIT NONE
         CHARACTER(LEN=*) :: line
         CHARACTER(LEN=1) :: tab = CHAR(9), space = CHAR(32)
         INTEGER          :: i
         DO i = 1, LEN_TRIM(line)
            IF(line(i:i) == tab)   line(i:i) = space
         END DO 
      END SUBROUTINE replaceTabs
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE readSplineData(fileUnit, dict) 
         USE EncoderModule
         USE FTDataClass
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER                           :: fileUnit
         CLASS(FTValueDictionary), POINTER :: dict
!
!        ---------------
!        Local Variables
!        ---------------
!
         INTEGER                          :: j, k, N
         CHARACTER(LEN=CFR_STRING_LENGTH) :: line
         REAL(KIND(1.0d0)), ALLOCATABLE   :: array(:,:)
         CHARACTER(LEN=1), ALLOCATABLE    :: enc(:)
         TYPE (FTData)   , POINTER        :: dta
         CLASS(FTObject) , POINTER        :: obj
         TYPE (FTException)     , POINTER :: exception
!
!        ----------------
!        Number of points
!        ----------------
!                                           
         N = dict % integerValueForKey("nKnots")
         IF ( N < HUGE(N) )     THEN
         
            ALLOCATE(array(4,N))
            DO j = 1, N
                READ(fileUnit, *) (array(k,j), k = 1,4)
            END DO
            
            CALL encode(arrayIn = array, enc = enc)
            
            ALLOCATE(dta)
            CALL dta % initWithDataOfType(genericData = enc,&
                                          dataType    = "Array2DReal")
            
            obj => dta
            CALL dict % addObjectForKey(object = obj, key = "data")
            CALL releaseFTData(self = dta)
!
!           ----------------------------------------
!           Data is followed by an \end{SPLINE_DATA}
!           ----------------------------------------
!
            READ(fileUnit,"(A)", END = 1000) line
            j = INDEX(STRING = line, SUBSTRING = "\end{SPLINE_DATA}")
            IF ( j <= 0 )     THEN
               ALLOCATE(exception)
               CALL exception % initFatalException( "No \end{SPLINE_DATA} marker for spline data" )
               CALL throw(exceptionToThrow = exception)
               CALL releaseFTException(exception)
            END IF 
            
         ELSE 
            ALLOCATE(exception)
            CALL exception % initFatalException( "Malformed Spline data. No nKnots" )
            CALL throw(exceptionToThrow = exception)
            CALL releaseFTException(exception)
         END IF 
          
1000  CONTINUE 
         
      END SUBROUTINE readSplineData
      END MODULE ControlFileReaderClass
