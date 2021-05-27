!
!////////////////////////////////////////////////////////////////////////
!
!      Encoder.f90
!      Created: June 29, 2018 at 2:37 PM 
!      By: David Kopriva 
!      All Rights Reserved.
!
!      Encode predefined arrays into a generic
!      string array. Add arrays as necessary.
!
!////////////////////////////////////////////////////////////////////////
!
      Module EncoderModule
      IMPLICIT NONE
      
      INTERFACE encode
         MODULE PROCEDURE :: encodeIntArray
         MODULE PROCEDURE :: encode2DRArray
         MODULE PROCEDURE :: encodeString
      END INTERFACE  
      
      INTERFACE DECODE
         MODULE PROCEDURE :: decodeIntArray
         MODULE PROCEDURE :: decode2DRArray
         MODULE PROCEDURE :: decodeString
      END INTERFACE 
!
!     --------      
      CONTAINS
!     -------- 
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE encodeString(str, enc)
         IMPLICIT NONE
         CHARACTER(LEN=*)              :: str
         CHARACTER(LEN=1), ALLOCATABLE :: enc(:)
         INTEGER                       :: lngth, j
         
         lngth = LEN_TRIM(str)
         ALLOCATE(enc(lngth))
         DO j = 1, lngth 
            enc(j) = str(j:j) 
         END DO 
         
      END SUBROUTINE encodeString
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE decodeString(enc,strOut)
         IMPLICIT NONE  
         CHARACTER(LEN=*)   :: strOut
         CHARACTER(LEN=1)   :: enc(:)
         INTEGER :: j
         
         strOut = ""
         DO j = 1, SIZE(enc) 
            strOut(j:j) = enc(j) 
         END DO 
         
      END SUBROUTINE decodeString
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE encodeIntArray(arrayIn, enc)
         IMPLICIT NONE
         INTEGER                       :: arrayIn(:)
         CHARACTER(LEN=1), ALLOCATABLE :: enc(:)
         INTEGER                       :: lngth
         
         lngth = SIZE(TRANSFER(SOURCE = arrayIn, MOLD = enc))
         ALLOCATE(enc(lngth))
         enc = TRANSFER(SOURCE = arrayIn, MOLD = enc)
         
      END SUBROUTINE encodeIntArray
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE decodeIntArray(enc,N,arrayOut)
         IMPLICIT NONE  
         INTEGER              :: N
         INTEGER, ALLOCATABLE :: arrayOut(:)
         CHARACTER(LEN=1)     :: enc(:)
         
         ALLOCATE(arrayOut(N))
         
         arrayOut = TRANSFER(SOURCE = enc, MOLD = arrayOut) 
         
      END SUBROUTINE decodeIntArray
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE encode2DRArray(arrayIn, enc)
         IMPLICIT NONE
         REAL(KIND(1.0d0))             :: arrayIn(:,:)
         CHARACTER(LEN=1), ALLOCATABLE :: enc(:)
         INTEGER                       :: lngth
         
         lngth = SIZE(TRANSFER(SOURCE = arrayIn, MOLD = enc))
         ALLOCATE(enc(lngth))
         enc = TRANSFER(SOURCE = arrayIn, MOLD = enc)
         
      END SUBROUTINE encode2DRArray
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE decode2DRArray(enc,N,M,arrayOut)
         IMPLICIT NONE  
         INTEGER                        :: N,M
         REAL(KIND(1.0d0)), ALLOCATABLE :: arrayOut(:,:)
         CHARACTER(LEN=1)               :: enc(:)
         
         ALLOCATE(arrayOut(N,M))
         
         arrayOut = RESHAPE(SOURCE = TRANSFER(SOURCE = enc, MOLD = arrayOut), SHAPE = [N,M]) 
         
      END SUBROUTINE decode2DRArray
      
      END MODULE EncoderModule
