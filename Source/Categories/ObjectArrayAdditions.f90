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
!      ObjectArrayAdditions.f90
!      Created: July 25, 2013 5:40 PM 
!      By: David Kopriva  
!
!////////////////////////////////////////////////////////////////////////
!
      Module ObjectArrayAdditionsModule
      IMPLICIT NONE 
!
!     ========
      CONTAINS
!     ======== 
!
!
!//////////////////////////////////////////////////////////////////////// 
! 
      SUBROUTINE initArrayWithLinkedList(self,list)  
         USE FTMutableObjectArrayClass
         USE FTLinkedListClass
         USE FTLinkedListIteratorClass
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         CLASS(FTMutableObjectArray)        :: self
         CLASS(FTLinkedList),       POINTER :: list
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER                     :: listSize
         TYPE(FTLinkedListIterator)  :: iterator
         CLASS(FTObject), POINTER    :: obj => NULL()
!
!        ----------------------------
!        Allocate the required memory
!        ----------------------------
!
         listSize = list % COUNT()
         CALL self % initwithSize(listSize)
!
!        --------------------------------------------
!        "Copy" the contents of the list to the array
!        --------------------------------------------
!
         CALL iterator % initWithFTLinkedList(list)
         CALL iterator % setToStart()
         
         DO WHILE (.NOT.iterator % isAtEnd())
            obj => iterator % object()
            CALL self % addObject(obj)
            CALL iterator % moveToNext()
         END DO
         
      END SUBROUTINE initArrayWithLinkedList
      
      END Module ObjectArrayAdditionsModule
