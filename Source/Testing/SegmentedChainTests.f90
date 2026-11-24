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
!      SegmentedChainTests.f90
!      Created: 2026-11-19
!
!      Unit tests for ChainedSegmentedCurve % add (addSegmentedCurveToChain):
!        1. A curve that is joined to the chain backwards is reversed
!        2. A curve that joins at neither end throws the
!           "Segmented Curves dont join" exception without stopping
!
!      Real models cannot reach these branches because SMChainedCurve
!      checks the continuous curves before they are discretized, so
!      they are tested directly here.
!      ChainedCurveJoinTests covers that earlier check, SMChainedCurve %
!      complete: a chain whose curves do not adjoin, or that does not
!      close, throws "Curves dont join" when the chain must close.
!
!////////////////////////////////////////////////////////////////////////
!
      Module SegmentedChainTestsModule
      USE SMConstants
      USE SMLineClass
      USE FRSegmentedCurveClass
      USE ChainedSegmentedCurveClass
      USE SMChainedCurveClass
      USE ProgramGlobals, ONLY : OUTER, NOT_APPLICABLE
      USE FTLinkedListClass
      USE FTValueClass
      USE SharedExceptionManagerModule
      USE FTAssertions
      IMPLICIT NONE

      PRIVATE
      PUBLIC :: SegmentedChainTests, ChainedCurveJoinTests
!
!     ========
      CONTAINS
!     ========
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SegmentedChainTests(optData)
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:)  ! Required by the test suite interface; unused
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(ChainedSegmentedCurve), POINTER :: chain
         CLASS(FRSegmentedCurve)    , POINTER :: segCurve
         CLASS(FTLinkedList)        , POINTER :: controls
         CLASS(FTException)         , POINTER :: exception
         CLASS(FTObject)            , POINTER :: obj
         REAL(KIND=RP)                        :: x(3)
         REAL(KIND=RP)                        :: h   = 0.25_RP
         REAL(KIND=RP)                        :: tol = 1.0E-12_RP

         ALLOCATE(controls)
         CALL controls % init()

         ALLOCATE(chain)
         CALL chain % initWithNameAndID(curveName = "testChain", id = 1)
!
!        -----------------------------------------------------------
!        First curve (0,0) -> (1,0) starts the chain in any direction
!        -----------------------------------------------------------
!
         segCurve => newSegmentedLine([0.0_RP,0.0_RP,0.0_RP], [1.0_RP,0.0_RP,0.0_RP], "bottom", 1, h, controls)
         CALL chain % add(segCurve)
         CALL releaseFRSegmentedCurve(self = segCurve)
         CALL FTAssertEqual(expectedValue = 1, actualValue = chain % curveCount(), &
                            msg = "First curve starts the chain")
!
!        -------------------------------------------------------------
!        Second curve given backwards: (1,1) -> (1,0). Its END matches
!        the end of the chain, so it must be reversed when it is added.
!        -------------------------------------------------------------
!
         segCurve => newSegmentedLine([1.0_RP,1.0_RP,0.0_RP], [1.0_RP,0.0_RP,0.0_RP], "right", 2, h, controls)
         CALL chain % add(segCurve)

         CALL FTAssert(test = .NOT.catch(), msg = "Reversed curve should join without an exception")
         CALL FTAssertEqual(expectedValue = 2, actualValue = chain % curveCount(), &
                            msg = "Reversed curve is added to the chain")
         x = segCurve % positionAtIndex(1)
         CALL FTAssert(test = MAXVAL(ABS(x - [1.0_RP,0.0_RP,0.0_RP])) < tol, &
                       msg = "Reversed curve now starts at the end of the chain")
         x = segCurve % positionAtIndex(segCurve % COUNT())
         CALL FTAssert(test = MAXVAL(ABS(x - [1.0_RP,1.0_RP,0.0_RP])) < tol, &
                       msg = "Reversed curve now ends at its old start")
         CALL FTAssert(test = .NOT.chain % isCircular, msg = "Open chain is not circular")
         CALL releaseFRSegmentedCurve(self = segCurve)
!
!        -----------------------------------------------------------------
!        Third curve (5,5) -> (6,6) touches neither end of the chain. The
!        exception is posted (not fatal to the run) and the curve is not
!        added.
!        -----------------------------------------------------------------
!
         segCurve => newSegmentedLine([5.0_RP,5.0_RP,0.0_RP], [6.0_RP,6.0_RP,0.0_RP], "stray", 3, h, controls)
         CALL chain % add(segCurve)

         CALL FTAssert(test = catch("Segmented Curves dont join"), &
                       msg  = "Disconnected curve should throw 'Segmented Curves dont join'")
         exception => errorObject()
         IF ( ASSOCIATED(exception) )     THEN
            CALL FTAssertEqual(expectedValue = FT_ERROR_FATAL, actualValue = exception % severity(), &
                               msg = "Disconnected curve exception is fatal")
         END IF
         CALL FTAssertEqual(expectedValue = 2, actualValue = chain % curveCount(), &
                            msg = "Disconnected curve is not added to the chain")
         CALL releaseFRSegmentedCurve(self = segCurve)
!
!        --------------------------------------------------------
!        Reset the shared exception manager so the fatal severity
!        recorded above does not leak into later tests
!        --------------------------------------------------------
!
         CALL FTAssert(test = .NOT.catch(), msg = "No exceptions left over")
         CALL initializeFTExceptions
!
!        --------
!        Clean up
!        --------
!
         CALL releaseChainChainedSegmentedCurve(chain)
         obj => controls
         CALL release(obj)

      END SUBROUTINE SegmentedChainTests
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION newSegmentedLine(xStart, xEnd, cName, id, h, controls) RESULT(seg)
!
!        ---------------------------------------------------------
!        Discretize a straight line from xStart to xEnd with size h
!        ---------------------------------------------------------
!
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         REAL(KIND=RP)                    :: xStart(3), xEnd(3)
         CHARACTER(LEN=*)                 :: cName
         INTEGER                          :: id
         REAL(KIND=RP)                    :: h
         CLASS(FTLinkedList)    , POINTER :: controls
         CLASS(FRSegmentedCurve), POINTER :: seg
!
!        ---------------
!        Local variables
!        ---------------
!
         CLASS(SMLine)  , POINTER :: line
         CLASS(SMCurve) , POINTER :: curvePtr
         CLASS(FTObject), POINTER :: obj

         ALLOCATE(line)
         CALL line % initWithStartEndNameAndID(xStart = xStart, xEnd = xEnd, cName = cName, id = id)
         curvePtr => line

         ALLOCATE(seg)
         CALL seg % initWithCurve(curvePtr, id, h, controls)

         obj => line
         CALL release(obj)

      END FUNCTION newSegmentedLine
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE ChainedCurveJoinTests(optData)
         IMPLICIT NONE
!        Arguments
         CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:)  ! Required by the test suite interface; unused
!        Local variables
         TYPE(SMChainedCurve), POINTER :: chain
!
!        ---------------------------------------------
!        A closed triangle completes without exception
!        ---------------------------------------------
!
         chain => newLineChain(RESHAPE([0.0_RP,0.0_RP, 1.0_RP,0.0_RP, 0.0_RP,1.0_RP], [2,3]))
         CALL chain % complete(innerOrOuterCurve = OUTER, chainMustClose = .TRUE.)
         CALL FTAssert(test = .NOT.catch(), msg = "Closed triangle should complete without exception")
         CALL releaseChainedCurve(chain)
!
!        ---------------------------------------------------------------
!        (0,0)->(1,0) followed by (2,0)->(3,0): the curves do not adjoin
!        ---------------------------------------------------------------
!
         chain => newChainOfTwo([0.0_RP,0.0_RP], [1.0_RP,0.0_RP], [2.0_RP,0.0_RP], [3.0_RP,0.0_RP])
         CALL chain % complete(innerOrOuterCurve = OUTER, chainMustClose = .TRUE.)
         CALL CheckDontJoinException(expectedMessage = "Curves do not adjoin", &
                                     context         = "Curves with a gap")
         CALL releaseChainedCurve(chain)
!
!        --------------------------------------------------------------
!        (0,0)->(1,0) followed by (1,0)->(1,1): the curves adjoin, but
!        the chain does not return to its start
!        --------------------------------------------------------------
!
         chain => newChainOfTwo([0.0_RP,0.0_RP], [1.0_RP,0.0_RP], [1.0_RP,0.0_RP], [1.0_RP,1.0_RP])
         CALL chain % complete(innerOrOuterCurve = OUTER, chainMustClose = .TRUE.)
         CALL CheckDontJoinException(expectedMessage = "Chain does not close", &
                                     context         = "Open chain")
         CALL releaseChainedCurve(chain)
!
!        ------------------------------------------------------
!        The same open chain is accepted when it need not close
!        ------------------------------------------------------
!
         chain => newChainOfTwo([0.0_RP,0.0_RP], [1.0_RP,0.0_RP], [1.0_RP,0.0_RP], [1.0_RP,1.0_RP])
         CALL chain % complete(innerOrOuterCurve = NOT_APPLICABLE, chainMustClose = .FALSE.)
         CALL FTAssert(test = .NOT.catch(), msg = "Open chain that need not close should not throw")
         CALL releaseChainedCurve(chain)
!
!        ------------------------------------------------------
!        Reset the shared exception manager so the fatal
!        severities recorded above do not leak into later tests
!        ------------------------------------------------------
!
         CALL FTAssert(test = .NOT.catch(), msg = "No exceptions left over")
         CALL initializeFTExceptions

      END SUBROUTINE ChainedCurveJoinTests
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION newChainOfTwo(a0, a1, b0, b1) RESULT(chain)
!        A chain of the two lines a0->a1 and b0->b1 (2D points)
         IMPLICIT NONE
!        Arguments
         REAL(KIND=RP)                 :: a0(2), a1(2), b0(2), b1(2)
         TYPE(SMChainedCurve), POINTER :: chain

         ALLOCATE(chain)
         CALL chain % initChainWithNameAndID(chainName = "testChain", id = 1)
         CALL AddLineToChain(chain, a0, a1, "first" , 1)
         CALL AddLineToChain(chain, b0, b1, "second", 2)

      END FUNCTION newChainOfTwo
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION newLineChain(corners) RESULT(chain)
!        A closed chain of lines through the given 2D corners
         IMPLICIT NONE
!        Arguments
         REAL(KIND=RP)                 :: corners(:,:)
         TYPE(SMChainedCurve), POINTER :: chain
!        Local variables
         INTEGER :: k, n

         n = SIZE(corners,2)
         ALLOCATE(chain)
         CALL chain % initChainWithNameAndID(chainName = "testChain", id = 1)
         DO k = 1, n
            CALL AddLineToChain(chain, corners(:,k), corners(:,MODULO(k,n)+1), "side", k)
         END DO

      END FUNCTION newLineChain
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE AddLineToChain(chain, x0, x1, cName, id)
         IMPLICIT NONE
!        Arguments
         CLASS(SMChainedCurve) :: chain
         REAL(KIND=RP)         :: x0(2), x1(2)
         CHARACTER(LEN=*)      :: cName
         INTEGER               :: id
!        Local variables
         CLASS(SMLine)  , POINTER :: line
         CLASS(SMCurve) , POINTER :: curvePtr
         CLASS(FTObject), POINTER :: obj

         ALLOCATE(line)
         CALL line % initWithStartEndNameAndID(xStart = [x0(1), x0(2), 0.0_RP], &
                                               xEnd   = [x1(1), x1(2), 0.0_RP], &
                                               cName  = cName, id = id)
         curvePtr => line
         CALL chain % addCurve(curvePtr)
         obj => line
         CALL release(obj)

      END SUBROUTINE AddLineToChain
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CheckDontJoinException(expectedMessage, context)
!        Pop the "Curves dont join" exception and check that it is fatal
!        and carries the expected message
         IMPLICIT NONE
!        Arguments
         CHARACTER(LEN=*) :: expectedMessage, context
!        Local variables
         CLASS(FTException) , POINTER :: exception
         CLASS(FTDictionary), POINTER :: info
         CLASS(FTValue)     , POINTER :: v

         CALL FTAssert(test = catch("Curves dont join"), &
                       msg  = context // " should throw 'Curves dont join'")
         exception => errorObject()
         IF ( .NOT.ASSOCIATED(exception) )     RETURN

         CALL FTAssertEqual(expectedValue = FT_ERROR_FATAL, actualValue = exception % severity(), &
                            msg = context // " exception is fatal")
         info => exception % infoDictionary()
         v    => valueFromObject(info % objectForKey("message"))
         CALL FTAssert(test = ASSOCIATED(v), msg = context // " exception has a message")
         IF ( ASSOCIATED(v) )     THEN
            CALL FTAssertEqual(expectedValue = expectedMessage,                      &
                               actualValue   = v % stringValue(requestedLength = 32), &
                               msg           = context // " exception message")
         END IF

      END SUBROUTINE CheckDontJoinException

      END Module SegmentedChainTestsModule
