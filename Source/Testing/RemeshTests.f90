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
!      RemeshTests.f90
!      Created: 2026-11-24
!      Tests of "Remeshing for accuracy": when the boundary curves
!      written to the mesh miss the error tolerance requested for a
!      chain, ResetInverseScales refines the sizer along that chain and
!      GenerateQuadMesh generates the mesh again. The sizer already aims
!      for the tolerance, so a remesh is rare. These tests take the steps
!      of GenerateQuadMesh one at a time, so that they can check what
!      happens at each one.
!////////////////////////////////////////////////////////////////////////
      Module RemeshTestsModule
      USE SMConstants
      USE ProgramGlobals         , ONLY : A_OK_ERROR_CODE, L2_NORM
      USE FTAssertions
      USE FTObjectClass
      USE FTValueDictionaryClass
      USE SharedExceptionManagerModule
      USE ErrorTypesModule
      USE SMChainedCurveClass
      USE MeshProjectClass
      USE MeshGenerationMethods  , ONLY : GenerateAQuadMesh, ComputeBoundaryApproximations
      USE BoundaryErrorModule    , ONLY : ComputeBoundaryErrors
      USE ControlInputTestsModule, ONLY : ClearExceptions
      USE ProjectInputTestsModule, ONLY : BuildProjectFromText, controlInputText, runParametersText, &
                                          backgroundGridText, chainText
      IMPLICIT NONE

      INTEGER, PARAMETER, PRIVATE :: LL = 80 ! Length of a control file line in these tests
!
!     ========
      CONTAINS
!     ========
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION remeshModelText( ) RESULT(text)
!
!     ---------------------------------------------------------------
!     The blob of BlobAdapt as the outer boundary, with a five petal
!     flower as an inner boundary. Both are optimized in the H1 norm
!     with C2 continuity. With polynomial order 4 and the background
!     grid of backgroundGridText, the first mesh misses the tolerance
!     on both chains (by factors of about 1.4 and 5.5), and the second
!     mesh meets it (0.6 and 0.98). Found by a search over polynomial orders,
!     tolerances, norms, continuities and grid sizes: the outer blob
!     misses its tolerance for every grid size from 0.75 to 1.5, and
!     the flower for every grid size from 0.7 to 1.2.
!     ---------------------------------------------------------------
!
         IMPLICIT NONE
         CHARACTER(LEN=LL), ALLOCATABLE :: text(:)

         text = [CHARACTER(LEN=LL) :: "\begin{MODEL}", "\begin{OUTER_BOUNDARY}",                        &
                                      "optimize = H1Norm", "continuity = 2", "tolerance = 1.0e-3",       &
                                      "\begin{PARAMETRIC_EQUATION_CURVE}", "name = blob",                &
                                      "xEqn = x(t) = 4*cos(2*pi*t) - 3/5*cos(8*pi*t)^3",                 &
                                      "yEqn = y(t) = 4*sin(2*pi*t) - 0.5*sin(11*pi*t)^2",                &
                                      "zEqn = z(t) = 0.0", "\end{PARAMETRIC_EQUATION_CURVE}",           &
                                      "\end{OUTER_BOUNDARY}", "\begin{INNER_BOUNDARIES}"]
         text = [CHARACTER(LEN=LL) :: text,                                                             &
                 chainText("flower", [CHARACTER(LEN=LL) ::                                              &
                           "optimize = H1Norm", "continuity = 2", "tolerance = 5.0e-5",                  &
                           "\begin{PARAMETRIC_EQUATION_CURVE}", "name = flower",                         &
                           "xEqn = x(t) = (1.5 + 0.4*cos(10*pi*t))*cos(2*pi*t)",                         &
                           "yEqn = y(t) = (1.5 + 0.4*cos(10*pi*t))*sin(2*pi*t)",                         &
                           "zEqn = z(t) = 0.0", "\end{PARAMETRIC_EQUATION_CURVE}"]),                     &
                 "\end{INNER_BOUNDARIES}", "\end{MODEL}"]
      END FUNCTION remeshModelText
!
!////////////////////////////////////////////////////////////////////////
!
      FUNCTION modelChain( project, name ) RESULT(chain)
         IMPLICIT NONE
         CLASS(MeshProject)   , POINTER :: project
         CHARACTER(LEN=*)               :: name
         CLASS(SMChainedCurve), POINTER :: chain

         CLASS(FTObject), POINTER :: obj
         INTEGER                  :: k

         chain => NULL()
         DO k = 1, project % model % numberOfChains()
            obj => project % model % allChains(k) % object
            CALL castToSMChainedCurve(obj, chain)
            IF ( TRIM(chain % curveName()) == name )     RETURN
         END DO
         chain => NULL()
      END FUNCTION modelChain
!
!////////////////////////////////////////////////////////////////////////
!
      REAL(KIND=RP) FUNCTION errorRatio( project, chain )
!
!     ------------------------------------------------------------
!     The largest error of a boundary curve segment in the norm the
!     chain is optimized in, as a multiple of the chain's tolerance
!     ------------------------------------------------------------
!
         IMPLICIT NONE
         CLASS(MeshProject)   , POINTER :: project
         CLASS(SMChainedCurve), POINTER :: chain

         IF ( chain % optimization == L2_NORM )     THEN
            errorRatio = MAXVAL(project % L2BoundaryError(chain % id()) % array)/chain % tolerance
         ELSE
            errorRatio = MAXVAL(project % H1BoundaryError(chain % id()) % array)/chain % tolerance
         END IF
      END FUNCTION errorRatio
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE MeshAndMeasure( project, label )
!
!     ---------------------------------------------------------------
!     One pass of the loop in GenerateQuadMesh before the rescaling:
!     generate the mesh, fit the boundary curves through its boundary
!     nodes, and measure their errors
!     ---------------------------------------------------------------
!
         IMPLICIT NONE
         CLASS(MeshProject), POINTER :: project
         CHARACTER(LEN=*)            :: label
         INTEGER                     :: errorCode

         CALL GenerateAQuadMesh(project, errorCode)
         CALL FTAssertEqual(expectedValue = A_OK_ERROR_CODE, actualValue = errorCode, &
                            msg = "The " // label // " mesh is generated")
         CALL ComputeBoundaryApproximations(project)
         CALL ComputeBoundaryErrors(project)
      END SUBROUTINE MeshAndMeasure
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE RemeshForAccuracyTests(optData)
!
!     ---------------------------------------------------------------
!     A first mesh that misses the H1 tolerance on the outer and the
!     inner boundary asks for a remesh, and the remesh is finer with
!     smaller errors on both. Errors set by hand show that errors within
!     the tolerance ask for no remesh, and cover the L2 branches: an L2
!     error above the tolerance on either chain asks for a remesh, and
!     the remesh is finer.
!     ---------------------------------------------------------------
!
         IMPLICIT NONE
         CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:)  ! Required by the test suite interface; unused

         TYPE (FTValueDictionary), POINTER :: controlDict
         CLASS(MeshProject)      , POINTER :: project
         CLASS(SMChainedCurve)   , POINTER :: outer, flower
         LOGICAL                           :: needsRemesh
         INTEGER                           :: nElements, k
         INTEGER                           :: ids(2)
         REAL(KIND=RP)                     :: tolerances(2), firstRatios(2)

         CALL ClearExceptions
         CALL BuildProjectFromText([controlInputText([runParametersText("ISM"), backgroundGridText()]), &
                                    remeshModelText()], controlDict, project)
         CALL FTAssert(test = .NOT.catch(FATAL_ERROR_EXCEPTION), msg = "The remesh model is read")
         outer  => project % model % outerBoundary
         flower => modelChain(project, "flower")
         CALL FTAssert(test = ASSOCIATED(outer) .AND. ASSOCIATED(flower), msg = "Both chains are found")
         IF ( .NOT.ASSOCIATED(outer) .OR. .NOT.ASSOCIATED(flower) )     THEN
            CALL releaseMeshProject(project)
            CALL releaseFTValueDictionary(controlDict)
            CALL ClearExceptions
            RETURN
         END IF
!
!        -----------------------------------------------------------
!        The first mesh misses the tolerance on both chains. If one
!        of these fails, the case no longer exercises the remesh and
!        a new one must be found
!        -----------------------------------------------------------
!
         CALL MeshAndMeasure(project, "first")
         CALL FTAssert(test = errorRatio(project, outer) > 1.0_RP, &
                       msg = "The first mesh misses the H1 tolerance on the outer boundary")
         CALL FTAssert(test = errorRatio(project, flower) > 1.0_RP, &
                       msg = "The first mesh misses the H1 tolerance on the inner boundary")
         nElements   = project % mesh % elements % COUNT()
         firstRatios = [errorRatio(project, outer), errorRatio(project, flower)]

         CALL ResetInverseScales(project, needsRemesh)
         CALL FTAssert(test = needsRemesh, msg = "Errors above the H1 tolerance ask for a remesh")
!
!        ------------------------------------------------------
!        The remesh, as in GenerateQuadMesh, is finer and has
!        smaller errors on both chains. It meets the tolerance
!        too (0.54 and 0.24 of it), but that is not asserted
!        ------------------------------------------------------
!
         CALL ResetProject(project)
         CALL MeshAndMeasure(project, "second")
         CALL FTAssert(test = project % mesh % elements % COUNT() > nElements, &
                       msg = "The remesh has more elements than the first mesh")
         CALL FTAssert(test = errorRatio(project, outer) < firstRatios(1), &
                       msg = "The remesh has smaller errors on the outer boundary")
         CALL FTAssert(test = errorRatio(project, flower) < firstRatios(2), &
                       msg = "The remesh has smaller errors on the inner boundary")
!
!        -------------------------------------------------------------
!        Errors set by hand. Half the tolerance on both chains asks for
!        no remesh, in either norm. Then the L2 branches: twice the
!        tolerance on one segment of either chain asks for a remesh
!        -------------------------------------------------------------
!
         nElements  = project % mesh % elements % COUNT()
         ids        = [outer % id(), flower % id()]
         tolerances = [outer % tolerance, flower % tolerance]

         DO k = 1, 2
            project % H1BoundaryError(ids(k)) % array = 0.5_RP*tolerances(k)
         END DO
         CALL ResetInverseScales(project, needsRemesh)
         CALL FTAssert(test = .NOT.needsRemesh, msg = "Errors within the H1 tolerance ask for no remesh")

         outer  % optimization = L2_NORM
         flower % optimization = L2_NORM
         DO k = 1, 2
            project % L2BoundaryError(ids(k)) % array = 0.5_RP*tolerances(k)
         END DO
         CALL ResetInverseScales(project, needsRemesh)
         CALL FTAssert(test = .NOT.needsRemesh, msg = "Errors within the L2 tolerance ask for no remesh")

         DO k = 1, 2
            project % L2BoundaryError(ids(k)) % array = 0.5_RP*tolerances(k)
            project % L2BoundaryError(ids(k)) % array(1) = 2.0_RP*tolerances(k)
            CALL ResetInverseScales(project, needsRemesh)
            CALL FTAssert(test = needsRemesh, msg = "An L2 error above the tolerance on one chain asks for a remesh")
            project % L2BoundaryError(ids(k)) % array = 0.5_RP*tolerances(k)
         END DO

         DO k = 1, 2
            project % L2BoundaryError(ids(k)) % array = 2.0_RP*tolerances(k)
         END DO
         CALL ResetInverseScales(project, needsRemesh)
         CALL ResetProject(project)
         CALL MeshAndMeasure(project, "L2 remesh")
         CALL FTAssert(test = project % mesh % elements % COUNT() > nElements, &
                       msg = "Errors above the L2 tolerance give a finer mesh")

         CALL releaseMeshProject(project)
         CALL releaseFTValueDictionary(controlDict)
         CALL ClearExceptions

      END SUBROUTINE RemeshForAccuracyTests

      END Module RemeshTestsModule
