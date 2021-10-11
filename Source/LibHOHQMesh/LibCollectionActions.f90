!
!////////////////////////////////////////////////////////////////////////
!
!      LibDictActions.f90
!      Created: October 7, 2021 at 10:12 AM 
!      By: David Kopriva  
!
!      FUNCTION   HML_NewDictionary() BIND(C) RESULT(cPtr)
!      FUNCTION   HML_NewList() BIND(C) RESULT(cPtr)
!
!      SUBROUTINE HML_InitDictionary(cPtr, errFlag) BIND(C)
!      SUBROUTINE HML_CloseDictionary(cPtr, errFlag)   BIND(C)
!
!      SUBROUTINE HML_InitList(cPtr, errFlag) BIND(C)
!      SUBROUTINE HML_CloseList(cPtr, errFlag)   BIND(C)
!
!      SUBROUTINE HML_AddDictKeyAndValue(cPtrToDict, cKey, cValue, errFlag)  
!      SUBROUTINE HML_AddDictForKey(cPtrToDict, cPtrToDictToAdd, key, errFlag)  
!      SUBROUTINE HML_AddDictToList( cPtrToDictToAdd, cPtrToList, errFlag)  
!      SUBROUTINE HML_AddListToDict( cPtrToList, cPtrToDict, errFlag)  
!
!////////////////////////////////////////////////////////////////////////
!
   Module ContainerInterfaceActions 
   USE FTValueDictionaryClass
   USE InteropUtilitiesModule
   USE HMLConstants
   IMPLICIT NONE  

   CONTAINS  
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Function that returns a c_ptr pointer to a new FTValueDictionary
!>
   FUNCTION HML_NewDictionary() BIND(C) RESULT(cPtr)
      IMPLICIT NONE
      TYPE( FTValueDictionary ), POINTER :: dict
      TYPE(c_ptr)                        :: cPtr
      ALLOCATE(dict)
      cPtr = C_LOC(dict)
   END FUNCTION HML_NewDictionary
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Initializer for an FTValueDictionary referenced as a c_ptr
!>
   SUBROUTINE HML_InitDictionary(cPtr, errFlag) BIND(C)
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      TYPE(c_ptr)    :: cPtr
      INTEGER(C_INT) :: errFlag
!
!     ---------------
!     Local variables
!     ---------------
!
      TYPE( FTValueDictionary ), POINTER :: dict
      
      CALL ptrToDictionary(cPtr = cPtr, dict = dict, errFlag = errFlag)
      IF(errFlag /= HML_ERROR_NONE)     RETURN 
      CALL dict % init()
      errFlag = 0
    
   END SUBROUTINE HML_InitDictionary
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Call to close out/kill an FTValueDictionary referenced as a c_ptr and release all its memory
!> CloseDictionary returns a null pointer even if there are multiple references
!> to the dictionary. 
!>
   SUBROUTINE HML_CloseDictionary(cPtr, errFlag)   BIND(C)
      IMPLICIT NONE  
!
!     ---------
!     Arguments
!     ---------
!
      TYPE(c_ptr)                   :: cPtr
      INTEGER(C_INT), INTENT(OUT)   :: errFlag
!
!     ---------------
!     Local Variables
!     ---------------
!
      TYPE  ( FTValueDictionary ), POINTER :: dict
      CLASS ( FTValueDictionary ), POINTER :: dictAsClass
      
      CALL ptrToDictionary(cPtr = cPtr, dict = dict, errFlag = errFlag)
      IF(errFlag /= HML_ERROR_NONE)     RETURN 
      dictAsClass => dict
      
      CALL releaseFTValueDictionary(dictAsClass)
      cPtr    = c_null_ptr
      errFlag = 0

   END SUBROUTINE HML_CloseDictionary
!
!//////////////////////////////////////////////////////////////////////// 
! 
!> Add a key and value (string) referenced as C compatible variables
!> to an FTValueDictionary
!>
   SUBROUTINE HML_AddDictKeyAndValue(cPtrToDict, cKey, cValue, errFlag)  
      IMPLICIT NONE  
!
!     ---------
!     Arguments
!     ---------
!
      TYPE(c_ptr)                          :: cPtrToDict
      CHARACTER(KIND=c_char), DIMENSION(*) :: cKey, cValue
      INTEGER(C_INT), INTENT(OUT)          :: errFlag
!
!     ---------------
!     Local Variables
!     ---------------
!
      TYPE  ( FTValueDictionary ), POINTER :: dict
      CLASS ( FTValueDictionary ), POINTER :: dictAsClass
      CHARACTER(len=:), ALLOCATABLE        :: fKey, fValue
      
      CALL ptrToDictionary(cPtr = cPtrToDict, dict = dict, errFlag = errFlag)
      IF(errFlag /= HML_ERROR_NONE)     RETURN 
      dictAsClass => dict
      
      fKey   = c_to_f_string(c_string = cKey )
      fValue = c_to_f_string(c_string = cValue )
      
      CALL dict % addValueForKey(fValue, fKey)
      errFlag = 0

   END SUBROUTINE HML_AddDictKeyAndValue
!
!//////////////////////////////////////////////////////////////////////// 
! 
!> Add an FTValueDictionary to another FTValueDictionary with a given key.
!>
   SUBROUTINE HML_AddDictForKey(cPtrToDict, cPtrToDictToAdd, key, errFlag)  
      IMPLICIT NONE  
!
!     ---------
!     Arguments
!     ---------
!
      TYPE(c_ptr)                          :: cPtrToDict,cPtrToDictToAdd
      CHARACTER(KIND=c_char), DIMENSION(*) :: key
      INTEGER(C_INT), INTENT(OUT)          :: errFlag
!
!     ---------------
!     Local Variables
!     ---------------
!
      TYPE  ( FTValueDictionary ), POINTER :: dict, dictToAdd
      CLASS(FTObject)            , POINTER :: obj
      CHARACTER(len=:), ALLOCATABLE        :: fKey

      CALL ptrToDictionary(cPtr = cPtrToDict, dict = dict, errFlag = errFlag)
      IF(errFlag /= HML_ERROR_NONE)     RETURN 
      CALL ptrToDictionary(cPtr = cPtrToDictToAdd, dict = dictToAdd, errFlag = errFlag)
      IF(errFlag /= HML_ERROR_NONE)     RETURN 

      fKey = c_to_f_string(key)
      
      obj => dictToAdd
      CALL dict % addObjectForKey(object = obj,key = fKey)
      errFlag = 0
      
   END SUBROUTINE HML_AddDictForKey
!
!//////////////////////////////////////////////////////////////////////// 
! 
!> Add an FTValueDictionary to an FTLinkedList, both referenced as c_ptrs.
!>
   SUBROUTINE HML_AddDictToList( cPtrToDictToAdd, cPtrToList, errFlag)  
      IMPLICIT NONE  
!
!     ---------
!     Arguments
!     ---------
!
      TYPE(c_ptr)                          :: cPtrToList,cPtrToDictToAdd
      INTEGER(C_INT), INTENT(OUT)          :: errFlag
!
!     ---------------
!     Local variables
!     ---------------
!
      CLASS(FTObject)         , POINTER :: obj
      TYPE (FTLinkedList)     , POINTER :: list
      TYPE(FTValueDictionary) , POINTER :: dict
      
      CALL ptrToList(cPtr = cPtrToList, list = list, errFlag = errFlag)
      IF(errFlag /= HML_ERROR_NONE)     RETURN
      
      CALL ptrToDictionary(cPtr = cPtrToDictToAdd, dict = dict,errFlag = errFlag)
      IF(errFlag /= HML_ERROR_NONE)     RETURN
      
      obj => dict
      CALL list % add(obj)
      errFlag = 0
   END SUBROUTINE HML_AddDictToList
!
!//////////////////////////////////////////////////////////////////////// 
! 
!> Add an FTLinkedList to an FTValueDictionary with key = "LIST",
!> both referenced as c_ptrs.
!>
   SUBROUTINE HML_AddListToDict( cPtrToList, cPtrToDict, errFlag)  
      IMPLICIT NONE  
!
!     ---------
!     Arguments
!     ---------
!
      TYPE(c_ptr)                 :: cPtrToList,cPtrToDict
      INTEGER(C_INT), INTENT(OUT) :: errFlag
!
!     ---------------
!     Local variables
!     ---------------
!
      CLASS(FTObject)        , POINTER :: obj
      TYPE(FTLinkedList)     , POINTER :: list
      TYPE(FTValueDictionary), POINTER :: dict
      
      CALL ptrToList(cPtr = cPtrToList, list = list, errFlag = errFlag)
      IF(errFlag /= HML_ERROR_NONE)     RETURN
      
      CALL ptrToDictionary(cPtr = cPtrToDict, dict = dict,errFlag = errFlag)
      IF(errFlag /= HML_ERROR_NONE)     RETURN
      
      obj => list
      CALL dict % addObjectForKey(object = obj,key = "LIST")
      errFlag = 0
      
   END SUBROUTINE HML_AddListToDict   
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Function that returns a c_ptr pointer to a new FTLinkedList list
!>
   FUNCTION HML_NewList() BIND(C) RESULT(cPtr)
      IMPLICIT NONE
      TYPE( FTLinkedList ), POINTER :: list
      TYPE(c_ptr)                   :: cPtr
      ALLOCATE(list)
      cPtr = C_LOC(list)
   END FUNCTION HML_NewList
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Initializer for an FTLinkedList referenced as a c_ptr
!>
   SUBROUTINE HML_InitList(cPtr, errFlag) BIND(C)
      IMPLICIT NONE
!
!     ---------
!     Arguments
!     ---------
!
      TYPE(c_ptr)                          :: cPtr
      INTEGER(C_INT)                       :: errFlag
!
!     ---------------
!     Local variables
!     ---------------
!
      TYPE( FTLinkedList ), POINTER     :: list
      
      CALL ptrToList(cPtr = cPtr, list = list, errFlag = errFlag)
      IF(errFlag /= HML_ERROR_NONE)     RETURN 
      CALL list % init()
      errFlag = 0
    
   END SUBROUTINE HML_InitList
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Call to close out/kill an FTLinkedList referenced as a c_ptr and release all its memory
!> HML_CloseList returns a null pointer even if there are multiple references
!> to the dictionary. 
!>
   SUBROUTINE HML_CloseList(cPtr, errFlag)   BIND(C)
      IMPLICIT NONE  
!
!     ---------
!     Arguments
!     ---------
!
      TYPE(c_ptr)                   :: cPtr
      INTEGER(C_INT), INTENT(OUT)   :: errFlag
!
!     ---------------
!     Local Variables
!     ---------------
!
      TYPE  ( FTLinkedList ), POINTER :: list
      CLASS ( FTLinkedList ), POINTER :: listAsClass
      
      CALL ptrToList(cPtr = cPtr, list  = list, errFlag = errFlag)
      IF(errFlag /= HML_ERROR_NONE)     RETURN 
      listAsClass => list
      
      CALL releaseFTLinkedList(listAsClass)
      cPtr = c_null_ptr
      errFlag = 0

   END SUBROUTINE HML_CloseList
!
!//////////////////////////////////////////////////////////////////////// 
! 
      LOGICAL FUNCTION IsDictionaryPtr(ptr)
         IMPLICIT NONE  
         CLASS ( FTValueDictionary ), POINTER :: ptr
         SELECT TYPE (p => ptr)
            TYPE IS(FTValueDictionary)
               IsDictionaryPtr = .TRUE.
            CLASS DEFAULT
               IsDictionaryPtr = .FALSE.
         END SELECT
      END FUNCTION IsDictionaryPtr
!
!//////////////////////////////////////////////////////////////////////// 
! 
      LOGICAL FUNCTION CptrIsDictionaryPtr(cPtr)  
         IMPLICIT NONE
         TYPE(c_ptr) :: cPtr
!
         TYPE( FTValueDictionary )  , POINTER     :: dict
         CLASS ( FTValueDictionary ), POINTER     :: dictAsClass
         
         CALL C_F_POINTER(cPtr = cPtr, FPTR = dict)
         
         dictAsClass         => dict
         CptrIsDictionaryPtr = .FALSE.
         
         IF ( IsDictionaryPtr(dictAsClass) )     THEN
               CptrIsDictionaryPtr = .TRUE.
         END IF 

      END FUNCTION CptrIsDictionaryPtr
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE ptrToDictionary(cPtr, dict, errFlag)  
      IMPLICIT NONE  
      TYPE(c_ptr)                          :: cPtr
      TYPE( FTValueDictionary )  , POINTER :: dict
      CLASS ( FTValueDictionary ), POINTER :: dictAsClass
      INTEGER(C_INT)                       :: errFlag
      
      errFlag     = HML_ERROR_NONE
      IF ( .NOT. C_ASSOCIATED(cPtr) )     THEN
         errFlag = HML_ERROR_NULL_POINTER
         return 
      END IF 
      
      CALL C_F_POINTER(cPtr = cPtr, FPTR = dict)
      dictAsClass => dict
      IF ( .NOT. IsDictionaryPtr(dictAsClass) )     THEN
            errFlag = HML_ERROR_NOT_A_DICT
            dict => NULL()
      END IF 
      
   END SUBROUTINE ptrToDictionary
!
!//////////////////////////////////////////////////////////////////////// 
! 
      LOGICAL FUNCTION IsListPtr(ptr)
         IMPLICIT NONE  
         CLASS ( FTLinkedList ), POINTER :: ptr
         SELECT TYPE (p => ptr)
            TYPE IS(FTLinkedList)
               IsListPtr = .TRUE.
            CLASS DEFAULT
               IsListPtr = .FALSE.
         END SELECT
      END FUNCTION IsListPtr
!
!//////////////////////////////////////////////////////////////////////// 
! 
   SUBROUTINE ptrToList(cPtr, list, errFlag)  
      IMPLICIT NONE  
      TYPE(c_ptr)                     :: cPtr
      TYPE( FTLinkedList )  , POINTER :: list
      CLASS ( FTLinkedList ), POINTER :: listAsClass
      INTEGER(C_INT)                  :: errFlag
      
      errFlag     = HML_ERROR_NONE
      IF ( .NOT. C_ASSOCIATED(cPtr) )     THEN
         errFlag = HML_ERROR_NULL_POINTER
         return 
      END IF 
      
      CALL C_F_POINTER(cPtr = cPtr, FPTR = list)
      listAsClass => list
      IF ( .NOT. IsListPtr(listAsClass) )     THEN
            errFlag = HML_ERROR_NOT_A_LIST
            list => NULL()
      END IF 
      
   END SUBROUTINE ptrToList

   END Module ContainerInterfaceActions
