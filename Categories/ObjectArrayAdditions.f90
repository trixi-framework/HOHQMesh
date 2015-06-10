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
         
         CALL iterator % release()
         
      END SUBROUTINE initArrayWithLinkedList
      
      END Module ObjectArrayAdditionsModule
