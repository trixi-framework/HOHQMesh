!
!////////////////////////////////////////////////////////////////////////
!
!      LibDictActions.f90
!      Created: October 7, 2021 at 10:12 AM 
!      By: David Kopriva  
!
!      FUNCTION   HML_AllocDictionary() BIND(C) RESULT(cPtr)
!      FUNCTION   HML_AllocList() BIND(C) RESULT(cPtr)
!
!      SUBROUTINE HML_InitDictionary(cPtr, errFlag) BIND(C)
!      SUBROUTINE HML_ReleaseDictionary(cPtr, errFlag)   BIND(C)
!
!      SUBROUTINE HML_InitList(cPtr, errFlag) BIND(C)
!      SUBROUTINE HML_ReleaseList(cPtr, errFlag)   BIND(C)
!
!      SUBROUTINE HML_AddDictKeyAndValue(cPtrToDict, cKey, cValue, errFlag)     BIND(C)
!      SUBROUTINE HML_AddDictForKey(cPtrToDict, cPtrToDictToAdd, key, errFlag)     BIND(C)
!      SUBROUTINE HML_AddDictToList( cPtrToDictToAdd, cPtrToList, errFlag)   BIND(C)  
!      SUBROUTINE HML_AddListToDict( cPtrToList, cPtrToDict, errFlag)   BIND(C) 
!
!      SUBROUTINE HML_AddArrayToDict(array, N, M, cPtrToDict, errFlag)   BIND(C)  
!
!////////////////////////////////////////////////////////////////////////
!
   Module ContainerInterfaceActions 
   USE FTValueDictionaryClass
   USE InteropUtilitiesModule
   USE HMLConstants
   USE FTDataClass
   USE EncoderModule
   IMPLICIT NONE  

   CONTAINS  
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Function that returns a c_ptr pointer to a new FTValueDictionary
!>
   FUNCTION HML_AllocDictionary() BIND(C) RESULT(cPtr)
      IMPLICIT NONE
      TYPE( FTValueDictionary ), POINTER :: dict
      TYPE(c_ptr)                        :: cPtr
      ALLOCATE(dict)
      cPtr = C_LOC(dict)
   END FUNCTION HML_AllocDictionary
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
   SUBROUTINE HML_ReleaseDictionary(cPtr, errFlag)   BIND(C)
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

   END SUBROUTINE HML_ReleaseDictionary
!
!//////////////////////////////////////////////////////////////////////// 
! 
!> Add a key and value (string) referenced as C compatible variables
!> to an FTValueDictionary
!>
   SUBROUTINE HML_AddDictKeyAndValue(cPtrToDict, cKey, cValue, errFlag)     BIND(C)
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
   SUBROUTINE HML_AddDictForKey(cPtrToDict, cPtrToDictToAdd, key, errFlag)     BIND(C)
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
!> Add a two-dimensional double precision array
!> to an FTValueDictionary
!>
   SUBROUTINE HML_AddArrayToDict(array, N, M, cPtrToDict, errFlag)    BIND(C) 
      IMPLICIT NONE  
!
!     ---------
!     Arguments
!     ---------
!
      INTEGER(C_INT)                       :: N, M
      REAL(KIND=C_DOUBLE)                  :: array(N,M)
      TYPE(c_ptr)                          :: cPtrToDict
      INTEGER(C_INT), INTENT(OUT)          :: errFlag
!
!     ---------------
!     Local Variables
!     ---------------
!
      TYPE  ( FTValueDictionary ), POINTER :: dict
      CHARACTER(LEN=1), ALLOCATABLE        :: enc(:)
      TYPE (FTData)   , POINTER            :: dta
      CLASS(FTObject) , POINTER            :: obj
      
      CALL ptrToDictionary(cPtr = cPtrToDict, dict = dict, errFlag = errFlag)
      IF(errFlag /= HML_ERROR_NONE)     RETURN 
      
            
      CALL encode(arrayIn = array, enc = enc)
      
      ALLOCATE(dta)
      CALL dta % initWithDataOfType(genericData = enc,&
                                    dataType    = "Array2DReal")
      
      obj => dta
      CALL dict % addObjectForKey(object = obj, key = "data")
      CALL releaseFTData(self = dta)
      errFlag = 0

   END SUBROUTINE HML_AddArrayToDict
!
!//////////////////////////////////////////////////////////////////////// 
! 
!> Add an FTValueDictionary to an FTLinkedList, both referenced as c_ptrs.
!>
   SUBROUTINE HML_AddDictToList( cPtrToDictToAdd, cPtrToList, errFlag) BIND(C)
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
   SUBROUTINE HML_AddListToDict( cPtrToList, cPtrToDict, errFlag)    BIND(C) 
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
!> Print a value dictionary containing lists and keys. Mostly for debugging
!> purposes.
!>
   RECURSIVE SUBROUTINE HML_PrintDict(cPtrToDict, errFlag)     BIND(C)
      IMPLICIT NONE  
!
!     ---------
!     Arguments
!     ---------
!
      TYPE(c_ptr)                          :: cPtrToDict
      INTEGER(C_INT), INTENT(OUT)          :: errFlag
!
!     ---------------
!     Local Variables
!     ---------------
!
      TYPE  ( FTValueDictionary ) , POINTER :: dict
      INTEGER                               :: indent = 0
      
      CALL ptrToDictionary(cPtr = cPtrToDict, dict = dict, errFlag = errFlag)
      IF(errFlag /= HML_ERROR_NONE)     RETURN 
      
      CALL printProjectDictionary(dict, indent)
      errFlag = 0

   END SUBROUTINE HML_PrintDict
!
!//////////////////////////////////////////////////////////////////////// 
! 
   RECURSIVE SUBROUTINE PrintProjectDictionary(dict, indent)
      IMPLICIT NONE  
!
!     ---------
!     Arguments
!     ---------
!
      TYPE  ( FTValueDictionary ), POINTER :: dict
      INTEGER                              :: indent
!
!     ---------------
!     Local Variables
!     ---------------
!
      CLASS ( FTValueDictionary )            , POINTER :: dictAsClass
      TYPE(FTMutableObjectArray)             , POINTER :: objectArray
      CLASS(FTObject)                        , POINTER :: obj
      CHARACTER(LEN=FTDICT_KWD_STRING_LENGTH), POINTER :: keys(:)
      INTEGER                                          :: k, i
      CHARACTER(LEN=4)                                 :: tab = "    "
      
      keys        => dict % allKeys()
      objectArray => dict % allObjects()
      
      DO k = 1, objectArray % COUNT()
         obj => objectArray % objectAtIndex(k)
         
         SELECT TYPE (obj)
            TYPE IS (FTValueDictionary)
               Write(6,*)
               DO i = 1,indent 
                  WRITE(6,FMT="(A4)",ADVANCE = "NO") tab
               END DO 
               Write(6,*) "Dictionary"
               Write(6,*)
               CALL PrintProjectDictionary(obj, indent+1)
            TYPE IS (FTLinkedList)
               Write(6,*)
               DO i = 1,indent 
                  WRITE(6,FMT="(A4)",ADVANCE = "NO") tab
               END DO 
               Write(6,*) "List"
               Write(6,*)
               CALL PrintProjectList(obj, indent+1)
            CLASS DEFAULT
               DO i = 1,indent 
                  WRITE(6,FMT="(A4)",ADVANCE = "NO") tab
               END DO 
               Write(6,*) TRIM(keys(k)), " = " ,TRIM(dict % stringValueForKey(key = keys(k), &
                                                  requestedLength = FTDICT_KWD_STRING_LENGTH))
         END SELECT

      END DO 
      
      DEALLOCATE(keys)
      CALL releaseFTMutableObjectArray(objectArray)
      
   END SUBROUTINE PrintProjectDictionary
!
!//////////////////////////////////////////////////////////////////////// 
! 
   RECURSIVE SUBROUTINE PrintProjectList(list, indent)
      IMPLICIT NONE  
!
!     ---------
!     Arguments
!     ---------
!
      TYPE(FTLinkedList)                               :: list
      INTEGER                                          :: indent
!
!     ---------------
!     Local variables
!     ---------------
!
      TYPE(FTMutableObjectArray)             , POINTER :: objectArray
      CLASS(FTObject)                        , POINTER :: obj
      INTEGER                                          :: k
      
      objectArray => list % allObjects()
      
      DO k = 1, objectArray % COUNT() 
      
         SELECT TYPE (obj)
            TYPE IS (FTValueDictionary)
               CALL PrintProjectDictionary(obj, indent)
            TYPE IS (FTLinkedList)
               CALL printProjectList(obj, indent)
            CLASS DEFAULT
               Write(6,*) "Unknown type to print in list"
         END SELECT
         
      END DO 
      
      CALL releaseFTMutableObjectArray(objectArray)
   END SUBROUTINE PrintProjectList
!
!//////////////////////////////////////////////////////////////////////// 
!
!> Function that returns a c_ptr pointer to a new FTLinkedList list
!>
   FUNCTION HML_AllocList() BIND(C) RESULT(cPtr)
      IMPLICIT NONE
      TYPE( FTLinkedList ), POINTER :: list
      TYPE(c_ptr)                   :: cPtr
      ALLOCATE(list)
      cPtr = C_LOC(list)
   END FUNCTION HML_AllocList
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
!> HML_ReleaseList returns a null pointer even if there are multiple references
!> to the dictionary. 
!>
   SUBROUTINE HML_ReleaseList(cPtr, errFlag)   BIND(C)
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

   END SUBROUTINE HML_ReleaseList
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
