! MIT License
!
! Copyright (c) 2010-present David A. Kopriva and other contributors: AUTHORS.md
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!
! --- End License
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
   INTEGER, PARAMETER :: QUAD_MESH_TEST = 1, HEX_MESH_TEST = 2, TEST_UNDEFINED = -1

!
!  -------------
!  2D Parameters
!  -------------
!
   INTEGER, PARAMETER :: NUMBER_OF_INT_TEST_VALUES2D  = 4
   INTEGER, PARAMETER :: NUMBER_OF_REAL_TEST_VALUES2D = 7

   INTEGER, PARAMETER :: TR_NUM_ELEMENTS = 1, TR_NUM_NODES = 2, TR_NUM_EDGES = 3, TR_NUM_BAD_ELEMENTS = 4
   CHARACTER(LEN=32), DIMENSION(NUMBER_OF_INT_TEST_VALUES2D) :: integerNames2D = ["number of elements    ", &
                                                                                  "number of Nodes       ", &
                                                                                  "number of Edges       ", &
                                                                                  "number of bad elements"  &
                                                                                 ]


   INTEGER, PARAMETER :: TR_SIGNED_AREA = 1, TR_ASPECT_RATIO = 2, TR_CONDITION = 3
   INTEGER, PARAMETER :: TR_EDGE_RATIO  = 4, JACOBIAN = 5       , MIN_ANGLE    = 6, MAX_ANGLE= 7

   CHARACTER(LEN=32), DIMENSION(NUMBER_OF_REAL_TEST_VALUES2D) :: realNames3D = ["signed area     ", &
                                                                                "aspect ratio    ", &
                                                                                "condition number", &
                                                                                "edge ratio      ", &
                                                                                "jacobian        ", &
                                                                                "minimum angle   ", &
                                                                                "maximum angle   "  &
                                                                               ]
!
!  -------------
!  3D Parameters
!  -------------
!
   INTEGER, PARAMETER :: NUMBER_OF_INT_TEST_VALUES3D  = 2
   INTEGER, PARAMETER :: NUMBER_OF_REAL_TEST_VALUES3D = 6

   CHARACTER(LEN=32), DIMENSION(NUMBER_OF_INT_TEST_VALUES3D) :: integerNames3D = ["number of elements    ", &
                                                                                  "number of Nodes       "  &
                                                                                 ]


   INTEGER, PARAMETER :: TR_DIAGONAL = 1, TR_EDGE_RATIO3D = 2, TR_JACOBIAN3D = 3
   INTEGER, PARAMETER :: TR_SHAPE    = 4,TR_SKEW          = 5, TR_VOLUME     = 6

   CHARACTER(LEN=32), DIMENSION(NUMBER_OF_REAL_TEST_VALUES3D) :: realNames2D = ["diagonal  ", &
                                                                                "edge ratio", &
                                                                                "jacobian  ", &
                                                                                "shape     ", &
                                                                                "skew      ", &
                                                                                "volume    "  &
                                                                               ]
!
!----------------------------------------------------------------------------------------
!
   TYPE testData
      INTEGER                                   :: testDataType
      INTEGER      , DIMENSION(:), ALLOCATABLE  :: intValues
      REAL(KIND=RP), DIMENSION(:), ALLOCATABLE  :: realValues
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
   SUBROUTINE ConstructTestData(self, testDataType)
      IMPLICIT NONE
      CLASS(testData) :: self
      INTEGER         :: testDataType

      CALL DestructTestData(self)

      self % testDataType = testDataType

      IF ( testDataType == QUAD_MESH_TEST )     THEN
         ALLOCATE( self % intValues( NUMBER_OF_INT_TEST_VALUES2D) )
         ALLOCATE( self % realValues( NUMBER_OF_REAL_TEST_VALUES2D))
      ELSE
         ALLOCATE( self % intValues( NUMBER_OF_INT_TEST_VALUES3D) )
         ALLOCATE( self % realValues( NUMBER_OF_REAL_TEST_VALUES3D))
      END IF

   END SUBROUTINE ConstructTestData
!
!////////////////////////////////////////////////////////////////////////
!
   SUBROUTINE DestructTestData(self)
      IMPLICIT NONE
      CLASS(testData) :: self

      IF(ALLOCATED(self % intValues))  DEALLOCATE(self % intValues)
      IF(ALLOCATED(self % realValues)) DEALLOCATE(self % realValues)

      self % testDataType       = TEST_UNDEFINED
   END SUBROUTINE DestructTestData
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

      WRITE(fUnit,*)            self % testDataType
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

      READ(fUnit, *)           self % testDataType
      CALL ConstructTestData(self,self % testDataType)

      read(fUnit, *)           self % intValues
      read(fUnit, '(1pe12.5)') self % realValues

   END SUBROUTINE readTestValues

   END Module TestDataClass
