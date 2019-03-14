!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE NodeLocs_ForTemplate_At( nodeLocs, templateType, i, j, rotation )
         IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER                  :: i, j, templateType
         INTEGER, DIMENSION(2,4)  :: nodeLocs
         INTEGER                  :: rotation
         
         INTEGER, PARAMETER :: TEMPLATE0 = 0, TEMPLATE1 = 1, TEMPLATE2A = 21, TEMPLATE2B = 22
         INTEGER, PARAMETER :: TEMPLATE3 = 3, TEMPLATE4 = 4 !See Schneiders' papers
         
         nodeLocs = 0
         SELECT CASE ( templateType )
            CASE( TEMPLATE0, TEMPLATE4 )
                nodeLocs(:,1) = (/i-1,j-1/)
                nodeLocs(:,2) = (/i  ,j-1/)
                nodeLocs(:,3) = (/i  ,j  /)
                nodeLocs(:,4) = (/i-1,j  /)
            CASE( TEMPLATE1 )
               CALL NodeLocs_ForTemplate1_at( nodeLocs, i, j, rotation )
            CASE( TEMPLATE2A )
               CALL NodeLocs_ForTemplate2A_at( nodeLocs, i, j, rotation )
            CASE( TEMPLATE2B )
               CALL NodeLocs_ForTemplate2B_at( nodeLocs, i, j, rotation )
            CASE( TEMPLATE3 )
               CALL NodeLocs_ForTemplate3_at( nodeLocs, i, j, rotation )
            CASE DEFAULT
!               PRINT *, "Undetermined templateType: ", templateType, " for i,j = ", i, j
         END SELECT
        
      END SUBROUTINE NodeLocs_ForTemplate_At
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE NodeLocs_ForTemplate1_at( nodeLocs, i, j, rotation )
         USE ProgramGlobals, ONLY: refinementType
         IMPLICIT NONE
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER                  :: i, j
         INTEGER, DIMENSION(2,4)  :: nodeLocs
         INTEGER                  :: rotation
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER  :: N(2) = (/3,3/)
         
         N = refinementType
         
         SELECT CASE ( rotation )
         
            CASE( 1 )
               
               IF( i == 1 .AND.  j == 1 )         THEN ! #1
                  nodeLocs(:,1) = (/0,0/)
                  nodeLocs(:,2) = (/1,0/)
                  nodeLocs(:,3) = (/1,1/)
                  nodeLocs(:,4) = (/0,1/)
               
               ELSE IF (i == 2 .AND. j == 1 )     THEN ! #2
                  nodeLocs(:,1) = (/1,0/)
                  nodeLocs(:,2) = (/N(1),0/)
                  nodeLocs(:,3) = (/N(1),N(2)/)
                  nodeLocs(:,4) = (/1,1/)
               
               ELSE IF( i == 1 .AND. j == 2 )     THEN ! #3
                  nodeLocs(:,1) = (/0,1/)
                  nodeLocs(:,2) = (/1,1/)
                  nodeLocs(:,3) = (/N(1),N(2)/)
                  nodeLocs(:,4) = (/0,N(2)/)
               ELSE
                  nodeLocs = 0
               END IF

            CASE( 2 )
               
               IF( i == 2 .AND.  j == 1 )         THEN ! #1
                  nodeLocs(:,1) = (/0,0/)
                  nodeLocs(:,2) = (/2,0/)
                  nodeLocs(:,3) = (/2,1/)
                  nodeLocs(:,4) = (/0,N(2)/)
               
               ELSE IF (i == 3 .AND. j == 1 )     THEN ! #2
                  nodeLocs(:,1) = (/2,0/)
                  nodeLocs(:,2) = (/N(1),0/)
                  nodeLocs(:,3) = (/3,1/)
                  nodeLocs(:,4) = (/2,1/)
               
               ELSE IF( i == 3 .AND. j == 2 )     THEN ! #3
                  nodeLocs(:,1) = (/2,1/)
                  nodeLocs(:,2) = (/3,1/)
                  nodeLocs(:,3) = (/N(1),N(2)/)
                  nodeLocs(:,4) = (/0,N(2)/)
               ELSE
                  nodeLocs = 0
               END IF
               
            CASE( 3 )
               
               IF( i == 3 .AND.  j == 2 )         THEN  ! #1
                  nodeLocs(:,1) = (/0,0/)
                  nodeLocs(:,2) = (/N(1),0/)
                  nodeLocs(:,3) = (/3,2/)
                  nodeLocs(:,4) = (/2,2/)
               
               ELSE IF (i == 3 .AND. j == 3 )     THEN ! #2
                  nodeLocs(:,1) = (/2,2/)
                  nodeLocs(:,2) = (/3,2/)
                  nodeLocs(:,3) = (/N(1),N(2)/)
                  nodeLocs(:,4) = (/2,3/)
               
               ELSE IF( i == 2 .AND. j == 3 )     THEN ! #3
                  nodeLocs(:,1) = (/0,0/)
                  nodeLocs(:,2) = (/2,2/)
                  nodeLocs(:,3) = (/2,3/)
                  nodeLocs(:,4) = (/0,N(2)/)
               ELSE
                  nodeLocs = 0
               END IF
             
            CASE( 4 )
               
               IF( i == 1 .AND.  j == 2 )         THEN ! #1
                  nodeLocs(:,1) = (/0,0/)
                  nodeLocs(:,2) = (/N(1),0/)
                  nodeLocs(:,3) = (/1,2/)
                  nodeLocs(:,4) = (/0,2/)
               
               ELSE IF (i == 2 .AND. j == 3 )     THEN ! #2
                  nodeLocs(:,1) = (/1,2/)
                  nodeLocs(:,2) = (/N(1),0/)
                  nodeLocs(:,3) = (/N(1),N(2)/)
                  nodeLocs(:,4) = (/1,3/)
               
               ELSE IF( i == 1 .AND. j == 3 )     THEN ! #3
                  nodeLocs(:,1) = (/0,2/)
                  nodeLocs(:,2) = (/1,2/)
                  nodeLocs(:,3) = (/1,3/)
                  nodeLocs(:,4) = (/0,N(2)/)
               ELSE
                  nodeLocs = 0
               END IF
            CASE DEFAULT
         END SELECT
         
      END SUBROUTINE NodeLocs_ForTemplate1_at
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE NodeLocs_ForTemplate2A_at( nodeLocs, i, j, rotation ) 
      IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER                 :: i, j
         INTEGER, DIMENSION(2,4) :: nodeLocs
         INTEGER                 :: rotation
!
!        ---------------
!        Local variables
!        ---------------
!
         
         IF( i == 2 .AND. j == 2 )     THEN 
            nodeLocs(:,1) = (/i-1,j-1/)
            nodeLocs(:,2) = (/i  ,j-1/)
            nodeLocs(:,3) = (/i,  j  /)
            nodeLocs(:,4) = (/i-1,j  /)
            RETURN 
         END IF
         
         SELECT CASE ( rotation )
         
            CASE( 1 )
               IF( j == 1 )     THEN
                  nodeLocs(:,1) = (/i-1,j-1/)
                  nodeLocs(:,2) = (/i  ,j-1/)
                  nodeLocs(:,3) = (/i,  j  /)
                  nodeLocs(:,4) = (/i-1,j  /)
                  
               ELSE IF ( i == 1 .AND. j == 2 )     THEN 
                  nodeLocs(:,1) = (/i-1,j-1/)
                  nodeLocs(:,2) = (/i  ,j-1/)
                  nodeLocs(:,3) = (/i,  j  /)
                  nodeLocs(:,4) = (/i-1,3  /)
               
               ELSE IF( i == 3 .AND. j == 2 )     THEN 
                  nodeLocs(:,1) = (/i-1,j-1/)
                  nodeLocs(:,2) = (/i  ,j-1/)
                  nodeLocs(:,3) = (/i,  3  /)
                  nodeLocs(:,4) = (/i-1,j  /)
               
               ELSE IF ( i == 2 .AND. j == 3 )     THEN     
                  nodeLocs(:,1) = (/i-1,j-1/)
                  nodeLocs(:,2) = (/i  ,j-1/)
                  nodeLocs(:,3) = (/3  ,3  /)
                  nodeLocs(:,4) = (/0  ,3  /)
               ELSE
                  nodeLocs = 0
               END IF
               
            CASE( 2 )
               IF( i == 3 )                       THEN
                  nodeLocs(:,1) = (/i-1,j-1/)
                  nodeLocs(:,2) = (/i  ,j-1/)
                  nodeLocs(:,3) = (/i,  j  /)
                  nodeLocs(:,4) = (/i-1,j  /)
               
               ELSE IF (i == 2 .AND. j == 1 )     THEN
                  nodeLocs(:,1) = (/0,0/)
                  nodeLocs(:,2) = (/i  ,j-1/)
                  nodeLocs(:,3) = (/i,  j  /)
                  nodeLocs(:,4) = (/i-1,j  /)
               
               ELSE IF (i == 1 .AND. j == 2 )     THEN
                  nodeLocs(:,1) = (/0,0/)
                  nodeLocs(:,2) = (/i  ,j-1/)
                  nodeLocs(:,3) = (/i,  j  /)
                  nodeLocs(:,4) = (/0,3  /)
               
               ELSE IF( i == 2 .AND. j == 3 )     THEN
                  nodeLocs(:,1) = (/i-1,j-1/)
                  nodeLocs(:,2) = (/i  ,j-1/)
                  nodeLocs(:,3) = (/i  ,j  /)
                  nodeLocs(:,4) = (/0  ,3  /)
               ELSE
                  nodeLocs = 0
               END IF
               
            CASE( 3 )
               IF( j == 3 )     THEN 
                  nodeLocs(:,1) = (/i-1,j-1/)
                  nodeLocs(:,2) = (/i  ,j-1/)
                  nodeLocs(:,3) = (/i,  j  /)
                  nodeLocs(:,4) = (/i-1,j  /)
               
               ELSE IF( i == 2 .AND. j == 1 )     THEN
                  nodeLocs(:,1) = (/0  ,0/)
                  nodeLocs(:,2) = (/3  ,0/)
                  nodeLocs(:,3) = (/i,  j/)
                  nodeLocs(:,4) = (/i-1,j/)
               
               ELSE IF (i == 1 .AND. j == 2 )     THEN 
                  nodeLocs(:,1) = (/0  ,0  /)
                  nodeLocs(:,2) = (/i  ,j-1/)
                  nodeLocs(:,3) = (/i,  j  /)
                  nodeLocs(:,4) = (/i-1,j  /)
               
               ELSE IF (i == 3 .AND. j == 2 )     THEN
                  nodeLocs(:,1) = (/i-1,j-1/)
                  nodeLocs(:,2) = (/3  ,0  /)
                  nodeLocs(:,3) = (/i  ,j  /)
                  nodeLocs(:,4) = (/i-1  ,j/)
               ELSE
                  nodeLocs = 0
               END IF

            CASE( 4 )
               IF( i == 1 )     THEN 
                  nodeLocs(:,1) = (/i-1,j-1/)
                  nodeLocs(:,2) = (/i  ,j-1/)
                  nodeLocs(:,3) = (/i,  j  /)
                  nodeLocs(:,4) = (/i-1,j  /)
               
               ELSE IF (i == 2 .AND. j == 1 )     THEN
                  nodeLocs(:,1) = (/i-1,j-1/)
                  nodeLocs(:,2) = (/3  ,0/)
                  nodeLocs(:,3) = (/i,  j/)
                  nodeLocs(:,4) = (/i-1,j/)
               
               ELSE IF (i == 2 .AND. j == 3 )     THEN
                  nodeLocs(:,1) = (/i-1,j-1  /)
                  nodeLocs(:,2) = (/i  ,j-1/)
                  nodeLocs(:,3) = (/3,  3  /)
                  nodeLocs(:,4) = (/i-1,j  /)
               
               ELSE IF (i == 3 .AND. j == 2 ) THEN 
                  nodeLocs(:,1) = (/i-1,j-1/)
                  nodeLocs(:,2) = (/3  ,0  /)
                  nodeLocs(:,3) = (/3  ,3  /)
                  nodeLocs(:,4) = (/i-1  ,j/)
               ELSE
                  nodeLocs = 0
               END IF
            CASE DEFAULT
         END SELECT
         
      END SUBROUTINE NodeLocs_ForTemplate2A_at
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE NodeLocs_ForTemplate2B_at( nodeLocs, i, j, rotation ) 
      IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER                 :: rotation
         INTEGER                 :: i, j
         INTEGER, DIMENSION(2,4) :: nodeLocs
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER :: N(2) = (/3,3/)
       
         IF( rotation == 3 .OR. rotation == 4 )     THEN
            nodeLocs = 0
            RETURN
         END IF 
!
!        --------------------------------------
!        Now set the nodes of each of the quads
!        --------------------------------------
!
         SELECT CASE ( rotation )
            CASE( 1 )
               IF( i == j )     THEN
                  nodeLocs(:,1) = (/i-1,j-1/)
                  nodeLocs(:,2) = (/i  ,j-1/)
                  nodeLocs(:,3) = (/i,  j  /)
                  nodeLocs(:,4) = (/i-1,j  /)
               ELSE
                  nodeLocs = 0
               END IF 
            CASE( 2 )
               IF( i == N(1) - j + 1 )     THEN
                  nodeLocs(:,1) = (/i-1,j-1/)
                  nodeLocs(:,2) = (/i  ,j-1/)
                  nodeLocs(:,3) = (/i,  j  /)
                  nodeLocs(:,4) = (/i-1,j  /)
               ELSE
                  nodeLocs = 0
               END IF
            CASE DEFAULT
               nodeLocs = 0
         END SELECT
!
!        -----------------------------
!        Fill in the nonstandard quads
!        ----------------------------
!
         SELECT CASE ( rotation )
         
            CASE( 1 )
               IF( i == 1 .AND. j == 2 )     THEN
                  nodeLocs(:,1) = (/i-1,j-1 /)
                  nodeLocs(:,2) = (/i  ,j-1 /)
                  nodeLocs(:,3) = (/i,  j   /)
                  nodeLocs(:,4) = (/0  ,N(2)/)
               ELSE IF (i == 2 .AND. j == 3 )     THEN 
                  nodeLocs(:,1) = (/i-1,j-1 /)
                  nodeLocs(:,2) = (/i  ,j-1 /)
                  nodeLocs(:,3) = (/i,  j   /)
                  nodeLocs(:,4) = (/0  ,N(2)/)
               ELSE IF ( i == 2 .AND. j == 1 )     THEN
                  nodeLocs(:,1) = (/i-1 ,j-1/)
                  nodeLocs(:,2) = (/N(1),0  /)
                  nodeLocs(:,3) = (/i   ,  j/)
                  nodeLocs(:,4) = (/i-1 ,j  /)
               ELSE IF ( i == 3 .AND. j == 2 )     THEN
                  nodeLocs(:,1) = (/i-1 ,j-1/)
                  nodeLocs(:,2) = (/N(1) ,0/)
                  nodeLocs(:,3) = (/i,  j  /)
                  nodeLocs(:,4) = (/i-1,j  /)
               END IF
               
            CASE( 2 )
               IF( i == 2 .AND. j == 1 )     THEN 
                  nodeLocs(:,1) = (/0,0/)
                  nodeLocs(:,2) = (/i  ,j-1/)
                  nodeLocs(:,3) = (/i,  j  /)
                  nodeLocs(:,4) = (/i-1,j  /)
               ELSE IF ( i == 1 .AND. j == 2 )     THEN 
                  nodeLocs(:,1) = (/0,0/)
                  nodeLocs(:,2) = (/i  ,j-1/)
                  nodeLocs(:,3) = (/i,  j  /)
                  nodeLocs(:,4) = (/i-1,j  /)
               ELSE IF ( i == 3 .AND. j == 2 )     THEN
                  nodeLocs(:,1) = (/i-1,j-1/)
                  nodeLocs(:,2) = (/i  ,j-1/)
                  nodeLocs(:,3) = (/N(1),N(2)  /)
                  nodeLocs(:,4) = (/i-1,j  /)
               ELSE IF ( i == 2 .AND. j == 3 )     THEN
                  nodeLocs(:,1) = (/i-1,j-1/)
                  nodeLocs(:,2) = (/i  ,j-1/)
                  nodeLocs(:,3) = (/N(1),N(2)  /)
                  nodeLocs(:,4) = (/i-1,j  /)
               END IF
            CASE DEFAULT
         END SELECT
   
      END SUBROUTINE NodeLocs_ForTemplate2B_at
!
!////////////////////////////////////////////////////////////////////////
!
      SUBROUTINE NodeLocs_ForTemplate3_at( nodeLocs, i, j, rotation ) 
      IMPLICIT NONE 
!
!        ---------
!        Arguments
!        ---------
!
         INTEGER                 :: rotation
         INTEGER                 :: i, j
         INTEGER, DIMENSION(2,4) :: nodeLocs
!
!        ---------------
!        Local variables
!        ---------------
!
         INTEGER :: N(2) = (/3,3/)

                                                               
         LOGICAL, DIMENSION(3,3,4) :: quadMask = RESHAPE( (/.false.,.false.,.false.,&
                                                            .true.,.false.,.false.,&
                                                            .true.,.true.,.false.,& !#1
                                                            .true.,.true.,.false.,&
                                                            .true.,.false.,.false.,&
                                                            .false.,.false.,.false.,& !#2
                                                            .false.,.true.,.true.,&
                                                            .false.,.false.,.true.,&
                                                            .false.,.false.,.false.,& !#3
                                                            .false.,.false.,.false.,&
                                                            .false.,.false.,.true.,&
                                                            .false.,.true.,.true./), (/3,3,4/))
       
!
         nodeLocs = 0
!
!        --------------------------
!        Fill in the standard quads
!        --------------------------
!
         IF( .NOT.quadMask(i,j,rotation) ) THEN
            nodeLocs(:,1) = (/i-1,j-1/)
            nodeLocs(:,2) = (/i  ,j-1/)
            nodeLocs(:,3) = (/i,  j  /)
            nodeLocs(:,4) = (/i-1,j  /)
         END IF
!
!        -----------------------------
!        Fill in the nonstandard quads
!        ----------------------------
!
         SELECT CASE ( rotation )
         
            CASE( 1 )
               IF( i == 1 .AND. j == 2 )         THEN
                  nodeLocs(:,1) = (/i-1,j-1 /)
                  nodeLocs(:,2) = (/i  ,j-1 /)
                  nodeLocs(:,3) = (/i,  j   /)
                  nodeLocs(:,4) = (/0  ,N(2)/)
               
               ELSE IF (i == 2 .AND. j == 3 )    THEN
                  nodeLocs(:,1) = (/i-1,j-1 /)
                  nodeLocs(:,2) = (/i  ,j-1 /)
                  nodeLocs(:,3) = (/i,  j   /)
                  nodeLocs(:,4) = (/0  ,N(2)/)
               END IF
               
            CASE( 2 )
               IF( i == 2 .AND. j == 1 )         THEN
                  nodeLocs(:,1) = (/0,0/)
                  nodeLocs(:,2) = (/i  ,j-1/)
                  nodeLocs(:,3) = (/i,  j  /)
                  nodeLocs(:,4) = (/i-1,j  /)
               ELSE IF( i == 1 .AND. j == 2 )    THEN
                  nodeLocs(:,1) = (/0,0/)
                  nodeLocs(:,2) = (/i  ,j-1/)
                  nodeLocs(:,3) = (/i,  j  /)
                  nodeLocs(:,4) = (/i-1,j  /)
               END IF
               
            CASE( 3 )
               IF( i == 2 .AND. j == 1 )         THEN 
                  nodeLocs(:,1) = (/i-1,j-1/)
                  nodeLocs(:,2) = (/N(1)  ,0/)
                  nodeLocs(:,3) = (/i,  j  /)
                  nodeLocs(:,4) = (/i-1,j  /)
               ELSE IF (i == 3 .AND. j == 2)     THEN
                  nodeLocs(:,1) = (/i-1,j-1/)
                  nodeLocs(:,2) = (/N(1)  ,0/)
                  nodeLocs(:,3) = (/i,  j  /)
                  nodeLocs(:,4) = (/i-1,j  /)
               END IF
               
            CASE( 4 )
               IF( i == 3 .AND. j == 2 )          THEN 
                  nodeLocs(:,1) = (/i-1,j-1/)
                  nodeLocs(:,2) = (/i  ,j-1/)
                  nodeLocs(:,3) = (/N(1),  N(2)  /)
                  nodeLocs(:,4) = (/i-1,j  /)
               ELSE IF (i == 2 .AND. j == 3 )    THEN
                  nodeLocs(:,1) = (/i-1,j-1/)
                  nodeLocs(:,2) = (/i  ,j-1/)
                  nodeLocs(:,3) = (/N(1),  N(2)  /)
                  nodeLocs(:,4) = (/i-1,j  /)
               END IF
            CASE DEFAULT
         END SELECT
         
   
      END SUBROUTINE NodeLocs_ForTemplate3_at
