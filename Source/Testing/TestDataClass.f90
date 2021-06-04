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
! HOHQMesh contains code that, to the best of our knowledge, has been released as
! public domain software:
! * `b3hs_hash_key_jenkins`: originally by Rich Townsend, 
!    https://groups.google.com/forum/#!topic/comp.lang.fortran/RWoHZFt39ng, 2005
! * `fmin`: originally by George Elmer Forsythe, Michael A. Malcolm, Cleve B. Moler, 
!    Computer Methods for Mathematical Computations, 1977
! * `spline`: originally by George Elmer Forsythe, Michael A. Malcolm, Cleve B. Moler, 
!    Computer Methods for Mathematical Computations, 1977
! * `seval`: originally by George Elmer Forsythe, Michael A. Malcolm, Cleve B. Moler, 
!    Computer Methods for Mathematical Computations, 1977
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
