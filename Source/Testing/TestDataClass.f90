!
!////////////////////////////////////////////////////////////////////////
!
!      TestingModule.f90
!      Created: May 12, 2021 at 4:33 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
   Module TestDataClass
   USE SMConstants, ONLY: RP
   IMPLICIT NONE  
! 
!------------------------------------------------------------------- 
! Routines for use in testing HOHQMesh 
!------------------------------------------------------------------- 
!
   INTEGER, PARAMETER :: NUMBER_OF_INT_TEST_VALUES = 4
   INTEGER, PARAMETER :: TR_NUM_ELEMENTS = 1, TR_NUM_NODES = 2, TR_NUM_EDGES = 3, TR_NUM_BAD_ELEMENTS = 4
   CHARACTER(LEN=32), DIMENSION(NUMBER_OF_INT_TEST_VALUES) :: integerNames = ["number of elements    ", &
                                                                              "number of Nodes       ", &
                                                                              "number of Edges       ", &
                                                                              "number of bad elements"]
   INTEGER, PARAMETER :: NUMBER_OF_REAL_TEST_VALUES = 7
   INTEGER, PARAMETER :: TR_SIGNED_AREA = 1, TR_ASPECT_RATIO = 2, TR_CONDITION = 3
   INTEGER, PARAMETER :: TR_EDGE_RATIO = 4, JACOBIAN = 5, MIN_ANGLE = 6, MAX_ANGLE= 7
   
   CHARACTER(LEN=32), DIMENSION(NUMBER_OF_REAL_TEST_VALUES) :: realNames = ["signed area     ", &
                                                                              "aspect ratio    ", &
                                                                              "condition number", &
                                                                              "edge ratio      ", &
                                                                              "jacobian        ", &
                                                                              "minimum angle   ", &
                                                                              "maximum angle   "]
   TYPE testData
      INTEGER      , DIMENSION(NUMBER_OF_INT_TEST_VALUES)  :: intValues = 0
      REAL(KIND=RP), DIMENSION(NUMBER_OF_REAL_TEST_VALUES) :: realValues = 0.0_RP
!
!     ========
      CONTAINS
!     ========
!
      PROCEDURE :: addIntValue
      PROCEDURE :: getIntValue
      PROCEDURE :: addRealValue
      PROCEDURE :: getRealValue
      PROCEDURE :: writeTestValues
   END TYPE testData
!
!  ========
   CONTAINS  
!  ========
!
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE addIntValue(self, whichValue, intValue)  
      IMPLICIT NONE  
      CLASS(testData) :: self
      INTEGER         :: whichValue
      INTEGER         :: intValue
      
      self % intValues(whichValue) = intValue
   END SUBROUTINE addIntValue
!
!//////////////////////////////////////////////////////////////////////// 
! 
   INTEGER FUNCTION getIntvalue(self, whichValue)  
      IMPLICIT NONE  
      CLASS(testData) :: self
      INTEGER         :: whichValue
      getIntValue = self % intValues(whichValue)
   END FUNCTION getIntvalue
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE addRealValue(self, whichValue, realValue)  
      IMPLICIT NONE  
      CLASS(testData) :: self
      INTEGER         :: whichValue
      REAL(KIND=RP)   :: realValue
      
      self % realValues(whichValue) = realValue
   END SUBROUTINE addRealValue
!
!//////////////////////////////////////////////////////////////////////// 
! 
   REAL(KIND=RP) FUNCTION getRealValue(self, whichValue)  
      IMPLICIT NONE  
      CLASS(testData) :: self
      INTEGER         :: whichValue
      getRealValue = self % realValues(whichValue)
   END FUNCTION getRealValue
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE writeTestValues(self, fUnit)  
      IMPLICIT NONE  
      CLASS(testData) :: self
      INTEGER         :: fUnit
      
      WRITE(fUnit, *)           self % intValues
      WRITE(fUnit, '(1pe12.5)') self % realValues
       
   END SUBROUTINE writeTestValues
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE readTestValues(self, fUnit)  
      IMPLICIT NONE  
      TYPE(testData) :: self
      INTEGER        :: fUnit
      
      read(fUnit, *)         self % intValues
      read(fUnit, '(1pe12.5)') self % realValues
       
   END SUBROUTINE readTestValues
  
   END Module TestDataClass
