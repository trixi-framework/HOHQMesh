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
!      ExtrusionTests.f90
!      Created: 2026-11-21
!      Tests of the SIMPLE_EXTRUSION in the x and y directions. The 2D
!      mesh nodes are permuted before the extrusion, and the high order
!      interior points of the hex elements must be extruded the same way
!      (issue #82). The regression benchmarks cannot see this, since the
!      3D statistics and skeleton plots only use the corner nodes.
!////////////////////////////////////////////////////////////////////////
      Module ExtrusionTestsModule
      USE SMConstants
      USE FTAssertions
      USE FTValueDictionaryClass
      USE EncoderModule
      USE HOHQMeshModule          , ONLY : HOHQMesh, ReadControlFile
      USE MeshProjectClass
      USE MeshQualityAnalysisClass, ONLY : MeshStatistics
      IMPLICIT NONE
!
!     ========
      CONTAINS
!     ========
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE SimpleExtrusionTests(optData)
!
!     -------------------------------------------------------------
!     optData holds the directory of the benchmark control files,
!     ending with a "/"
!     -------------------------------------------------------------
!
         IMPLICIT NONE
         CHARACTER(LEN=1), POINTER, OPTIONAL :: optData(:)

         CHARACTER(LEN=DEFAULT_FILE_PATH_LENGTH) :: controlDirectory

         controlDirectory = ""
         CALL DECODE(enc = optData, strOut = controlDirectory)

         CALL CheckExtrudedInteriorPoints(TRIM(controlDirectory) // "HalfCircleExtrude.control",  &
                                          direction = 1, height = 4.0_RP)
         CALL CheckExtrudedInteriorPoints(TRIM(controlDirectory) // "HalfCircleExtrudeY.control", &
                                          direction = 2, height = 4.0_RP)

      END SUBROUTINE SimpleExtrusionTests
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE CheckExtrudedInteriorPoints(controlFileName, direction, height)
!
!     -------------------------------------------------------------------
!     Mesh the control file and check that, in the extrusion direction,
!     the interior points of the bottom layer start at zero and those of
!     the top layer end at the extrusion height
!     -------------------------------------------------------------------
!
         IMPLICIT NONE
         CHARACTER(LEN=*)  :: controlFileName
         INTEGER           :: direction
         REAL(KIND=RP)     :: height

         TYPE (FTValueDictionary), POINTER :: projectDict => NULL()
         CLASS(MeshProject)      , POINTER :: project     => NULL()
         TYPE( MeshStatistics )            :: stats
         LOGICAL                           :: didGenerate3DMesh
         INTEGER                           :: l, N, numberOfLayers
         REAL(KIND=RP)                     :: bottomError, topError
         REAL(KIND=RP)                     :: tolerance

         CALL ReadControlFile(controlFileName, projectDict)
         ALLOCATE(project)
         didGenerate3DMesh = .FALSE.
         CALL HOHQMesh(projectDict, project, stats, didGenerate3DMesh, .TRUE.)
         CALL FTAssert(test = didGenerate3DMesh, msg = "Extrusion generates a hex mesh: " // controlFileName)

         IF ( didGenerate3DMesh )     THEN
            N              = project % runParams % polynomialOrder
            numberOfLayers = project % hexMesh % numberOfLayers
            bottomError    = 0.0_RP
            topError       = 0.0_RP
            DO l = 1, project % hexMesh % numberOfQuadElements
               bottomError = MAX(bottomError, &
                                 MAXVAL(ABS(project % hexMesh % elements(l,1) % x(direction,:,:,0))))
               topError    = MAX(topError, &
                                 MAXVAL(ABS(project % hexMesh % elements(l,numberOfLayers) % x(direction,:,:,N) - height)))
            END DO
            tolerance = 1.0E-12_RP*height
            CALL FTAssert(test = bottomError <= tolerance, &
                          msg  = "Interior points of the bottom layer start at zero: " // controlFileName)
            CALL FTAssert(test = topError <= tolerance, &
                          msg  = "Interior points of the top layer end at the height: " // controlFileName)
         END IF

         CALL releaseMeshProject(project)
         CALL releaseFTValueDictionary(projectDict)

      END SUBROUTINE CheckExtrudedInteriorPoints

      END Module ExtrusionTestsModule
