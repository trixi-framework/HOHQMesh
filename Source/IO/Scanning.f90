!
!////////////////////////////////////////////////////////////////////////
!
!      Scanning.f90
!      Created: June 23, 2026 at 10:13 AM
!      By: David Kopriva
!
!////////////////////////////////////////////////////////////////////////
!
   Module ScanningModule
   USE SMConstants, ONLY: RP
   USE SMScannerClass
   IMPLICIT NONE
   PRIVATE
   PUBLIC :: ScanForBreaks, ScanForBreaksIsOK, flaggingIsOK
!
!  ========
   CONTAINS
!  ========
!
!
!////////////////////////////////////////////////////////////////////////
!
   SUBROUTINE ScanForBreaks(connectionString, breaks, nSegments)
!
!     -----------------------------------------------------------------------
!     Compute the locations of the breaks in a curve as specified by the user
!     in the control file. Breaks are expressed in the form

!     connect = N1-N2, N3-N4 [,...]

!     e.g.
!     connect = 2-4, 6-9, 10-11
!
!     if there are four segments and connect = 2-3 then the breaks are
!     at the end of segments 1 and 3, and breaks = [0.0, 0.25, 0.75, 1.0],
!     skipping the breaks at 1/2 between segments 2 and 3. See
!     ScanForBreaksIsOK for examples
!  -----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      CHARACTER(LEN=*)           :: connectionString
      REAL(KIND=RP), ALLOCATABLE :: breaks(:)
      INTEGER                    :: nSegments
!
!     ---------------
!     Local variables
!     ---------------
!
      TYPE(SMScanner) :: scanner
      INTEGER         :: flagged(0:nSegments)
      INTEGER         :: flagCount
      LOGICAL         :: fail
      INTEGER         :: numberOfBreaks
      INTEGER         :: j, k
      REAL(KIND=RP) :: c

      flagged = 0
      CALL scanner % initWithString(connectionString,delims = ",-")
!
!     -------------------------------------------------------------
!     Flag the locations (with a 1) where breaks will NOT be placed
!     -------------------------------------------------------------
!
      CALL flagSegments(scanner, flagged, flagCount, 1, fail)
      IF ( fail )     THEN
         WRITE(0,*) "Scanning of the following connection lines failed with a syntax error"
         WRITE(0,*) TRIM(connectionString)
         RETURN
      END IF

      numberOfBreaks = nSegments - flagCount
      ALLOCATE(breaks(0:numberOfBreaks))
      breaks(0)              = 0.0_RP
      breaks(numberOfBreaks) = 1.0_RP

      k = 1
      c = 1.0_RP/REAL(nSegments,KIND=RP)
      DO j = 1, nSegments-1
         IF(flagged(j) == 1) CYCLE
         breaks(k) = j*c
         k = k + 1
      END DO


   END SUBROUTINE ScanForBreaks
!
!////////////////////////////////////////////////////////////////////////
!
   SUBROUTINE flagSegments(scanner, flagged, flagCount, flagOn, fail )
!
!  -----------------------------------------------------------------------
!  Using the scanner, scan its string for groups
!  in the form of start-stop, e.g. 4-6, separated by commas. Set
!  the entry in the array flagged to flagOn for each element of each group
!  For this procedure, the scanner must include "-" and "," as the delimiters.
!  -----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      TYPE(SMScanner) :: scanner
      INTEGER         :: flagged(0:)
      INTEGER         :: flagOn
      INTEGER         :: flagCount ! The total number of flagged segments
      LOGICAL         :: fail
!
!     ---------------
!     Local Variables
!     ---------------
!
      INTEGER          :: fSize
      INTEGER          :: j, k
      INTEGER          :: strt, stp

      fSize     = SIZE(flagged)
      fail      = .FALSE.
      flagCount = 0

      DO j = 1, fSize          ! Cannot be bigger than this
!
!        ------------------------
!        Get the start of a group
!        ------------------------
!
         strt = scanInt(scanner)
!
!        ---------------------------------------------------
!        The delimiter that stops this scan must be a "-" or
!        there is an error in the string
!        ---------------------------------------------------
!
         IF ( scanner % lastDelimiter() .NE. "-" )     THEN
            WRITE(0,*) "String start configuration error"
            fail = .TRUE.
            RETURN
         END IF

         stp = scanInt(scanner)
!
!        --------------------------------------------------------
!        After the integer is scanned, the next delimiter must be
!        either a comma or an end of string. It cannot be a -
!        --------------------------------------------------------
!
         IF ( scanner % lastDelimiter() .EQ. "-")     THEN
            WRITE(0,*) "String stop configuration error"
            fail = .TRUE.
            RETURN
         END IF
!
!        --------------------------------------------------------
!        At the end of the string, we don't flag the last segment
!        --------------------------------------------------------
!
         DO k = strt, stp-1
            flagged(k) = flagOn
            flagCount  = flagCount + 1
         END DO
!
!        -----------------------------------------------
!        We're done when we get to the end of the string
!        -----------------------------------------------
!
         IF (scanner % isAtEnd() )     EXIT
      END DO

   END SUBROUTINE flagSegments

!
!////////////////////////////////////////////////////////////////////////
!
   LOGICAL FUNCTION flagString(str, flagged, flagCount)
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      CHARACTER(LEN=*) :: str
      INTEGER          :: flagged(0:)
      INTEGER          :: flagCount
!
!     ---------------
!     Local variables
!     ---------------
!
      TYPE(SMScanner) :: scanner
      LOGICAL         :: fail

      flagged = 0
      CALL scanner % initWithString(str,delims = ",-")

      CALL flagSegments(scanner, flagged, flagCount, 1, fail)

      flagString = fail

   END FUNCTION flagString
!
!////////////////////////////////////////////////////////////////////////
!
   LOGICAL FUNCTION flaggingIsOK()
      IMPLICIT NONE
      LOGICAL           :: fail
      CHARACTER(LEN=16) :: str
      INTEGER           :: flagged(0:8)
      INTEGER           :: flagCount
      INTEGER           :: flagged1(0:8), flagged2(0:8)

      flaggingIsOK = .TRUE.
!
!     -----
!     Try 1
!     -----
!
      str = "2-3,5-8"
      flagged1 = [0,0,1,0,0,1,1,1,0]
      fail     = flagString(str, flagged, flagCount)
      flaggingIsOK = .NOT.fail .AND. flaggingIsOK
      flaggingIsOK = MAXVAL(flagged - flagged1) == 0 .AND. flaggingIsOK
!
!     -----
!     Try 2
!     -----
!
      str = "1-3,6-7"
      flagged2 = [0,1,1,0,0,0,1,0,0]
      fail = flagString(str, flagged, flagCount)
      flaggingIsOK = .NOT.fail .AND. flaggingIsOK
      flaggingIsOK = MAXVAL(flagged - flagged2) == 0 .AND. flaggingIsOK

   END FUNCTION flaggingIsOK
!
!////////////////////////////////////////////////////////////////////////
!
   LOGICAL FUNCTION ScanForBreaksIsOK()
      IMPLICIT NONE
      CHARACTER(LEN=16) :: testString1 = "2-3"
      CHARACTER(LEN=16) :: testString2 = "1-2,4-6"
      REAL(KIND=RP), ALLOCATABLE :: breaks(:)
      REAL(KIND=RP)              :: b1(0:3) = [0.0_RP, 0.25_RP, 0.75_RP, 1.0_RP]
      REAL(KIND=RP)              :: b2(0:5) = [0.0_RP, 0.25_RP, 0.375_RP, 0.75_RP, 0.875_RP, 1.0_RP]
      INTEGER                    :: nSegments = 4

      ScanForBreaksIsOK = .TRUE.

      CALL ScanForBreaks(testString1, breaks, nSegments )
      ScanForBreaksIsOK = MAXVAL(b1 - breaks) == 0.0_RP
      DEALLOCATE(breaks)

      CALL ScanForBreaks(testString2, breaks, nSegments*2 )
      ScanForBreaksIsOK = ScanForBreaksIsOK .AND. MAXVAL(b2 - breaks) == 0.0_RP

   END FUNCTION ScanForBreaksIsOK


   END Module ScanningModule
