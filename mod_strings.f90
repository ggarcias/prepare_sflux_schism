module mod_strings
    use netcdf
    use mod_nc_tools
    implicit none

    contains

    subroutine get_delimiters(delimiters)
        integer :: i
        CHARACTER(LEN=66)  :: delimiters(0:55)
        CHARACTER(LEN=128) :: alphabet ! (0:127)

        ! DECIMAL
        ! *-------*-------*-------*-------*-------*-------*-------*-------*
        ! | 00 nul| 01 soh| 02 stx| 03 etx| 04 eot| 05 enq| 06 ack| 07 bel|
        ! | 08 bs | 09 ht | 10 nl | 11 vt | 12 np | 13 cr | 14 so | 15 si |
        ! | 16 dle| 17 dc1| 18 dc2| 19 dc3| 20 dc4| 21 nak| 22 syn| 23 etb|
        ! | 24 can| 25 em | 26 sub| 27 esc| 28 fs | 29 gs | 30 rs | 31 us |
        ! | 32 sp | 33  ! | 34  " | 35  # | 36  $ | 37  % | 38  & | 39  ' |
        ! | 40  ( | 41  ) | 42  * | 43  + | 44  , | 45  - | 46  . | 47  / |
        ! | 48  0 | 49  1 | 50  2 | 51  3 | 52  4 | 53  5 | 54  6 | 55  7 |
        ! | 56  8 | 57  9 | 58  : | 59  ; | 60  < | 61  = | 62  > | 63  ? |
        ! | 64  @ | 65  A | 66  B | 67  C | 68  D | 69  E | 70  F | 71  G |
        ! | 72  H | 73  I | 74  J | 75  K | 76  L | 77  M | 78  N | 79  O |
        ! | 80  P | 81  Q | 82  R | 83  S | 84  T | 85  U | 86  V | 87  W |
        ! | 88  X | 89  Y | 90  Z | 91  [ | 92  \ | 93  ] | 94  ^ | 95  _ |
        ! | 96  ` | 97  a | 98  b | 99  c |100  d |101  e |102  f |103  g |
        ! |104  h |105  i |106  j |107  k |108  l |109  m |110  n |111  o |
        ! |112  p |113  q |114  r |115  s |116  t |117  u |118  v |119  w |
        ! |120  x |121  y |122  z |123  { |124  | |125  } |126  ~ |127 del|
        ! *-------*-------*-------*-------*-------*-------*-------*-------*
        DO i = 0, 127
            ! note: CHAR(I [, KIND]) returns the character represented by the integer I. 
            alphabet(i + 1:i + 1) = char(i)
        ENDDO
        delimiters = alphabet(0 + 1:47 + 1) // alphabet(58 + 1:64 + 1) // alphabet(91 + 1:96 + 1) // alphabet(123 + 1:127 + 1)
    end subroutine get_delimiters

    subroutine get_number_of_splits(string_to_split, number_of_strings)
        character(len=*), intent(in) :: string_to_split
        integer, intent(out) :: number_of_strings
        character(LEN=255) :: strtok
        character(LEN=80)  :: token
        CHARACTER(LEN=66)  :: delimiters(0:55)
        integer :: number_of_splits

        call get_delimiters(delimiters)
        token = strtok(string_to_split, delimiters(1))
        number_of_splits = 1

        ! Compute number of splits
        DO WHILE (token .NE. char(0))
            token = strtok(CHAR(0), delimiters(1))
            number_of_splits = number_of_splits + 1
        ENDDO
        number_of_strings = number_of_splits
        
    end subroutine get_number_of_splits

    subroutine split(string_to_split,  number_of_splits, string_splitted)
        character(len=*), intent(in) :: string_to_split
        integer, intent(in) :: number_of_splits
        character(len=*), intent(out) :: string_splitted(number_of_splits)
        character(LEN=255) :: strtok
        character(LEN=80)  :: token
        CHARACTER(LEN=66)  :: delimiters(0:55)
        integer :: i  

        call get_delimiters(delimiters)

        ! call get_number_of_splits(string_to_split, number_of_splits)

        token = strtok(string_to_split, delimiters(1))
        
        ! Split string
        i = 1
        DO WHILE (token .NE. char(0))
            string_splitted(i) = token
            token = strtok(CHAR(0), delimiters(1))
            i = i + 1
        ENDDO
    end subroutine split
   

    subroutine shift_time(ncname, time_name, diff_julian_time, timescale) 
        implicit none
        
        character (len = *), intent(in) :: ncname, time_name

        real(kind=8), intent(out) :: diff_julian_time

        integer, intent(out) :: timescale

        character (len = 100) :: unit_str, unitref
        character(len=100), dimension(:), allocatable :: time_strings

        real(kind=8) :: julian_time

        integer :: ncid, varid, number_of_strings, stat
        integer ::  yearref, monthref, dayref, hourref, &
            minref, secref

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! Read time unit and return the difference in julian time in SECONDS and the
        ! timescale of the netcdf file 
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
        call check(nf90_open(ncname, NF90_NOWRITE, ncid), "open nc file")
        call check(nf90_inq_varid(ncid, time_name, varid), "getting time id")
        call check(nf90_get_att(ncid, varid, "units", unit_str), "unit time")
        call check(nf90_close(ncid),"Close nc")

        call get_number_of_splits(unit_str, number_of_strings)
        allocate(time_strings(number_of_strings))
        call split(unit_str, number_of_strings, time_strings)

        unitref  = time_strings(1)
        read(time_strings(3),*, iostat = stat) yearref
        read(time_strings(4),*, iostat = stat) monthref
        read(time_strings(5),*, iostat = stat) dayref
        read(time_strings(6),*, iostat = stat) hourref
        read(time_strings(7),*, iostat = stat) minref
        read(time_strings(8),*, iostat = stat) secref

        julian_time  = JULIAN(yearref, monthref, dayref) + FRACTIONTIME(hourref, minref,&
        secref)
        diff_julian_time  = (julian_time - JULIAN(2000, 1, 1)) * 24 * 3600

        IF (unitref == "seconds") THEN
            timescale = 1
        ELSE IF (unitref == "minutes") THEN
            timescale = 60
        ELSE IF (unitref == "hours") THEN
            timescale = 3600
        ELSE IF (unitref == "days") THEN
            timescale = 3600 * 24
        ENDIF

    end subroutine shift_time


    INTEGER FUNCTION JULIAN(YEAR, MONTH, DAY)

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: YEAR, MONTH, DAY

      JULIAN = DAY - 32075 + 1461 * (YEAR + 4800 + (MONTH - 14) / 12) / 4 + &
          367 * (MONTH - 2 - (MONTH - 14) / 12 * 12) / 12 - 3 * &
          ((YEAR + 4900 + (MONTH - 14) / 12) / 100) / 4

    END FUNCTION


    REAL(KIND = 8) FUNCTION FRACTIONTIME(HOUR, MINUTE, SECOND)

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: HOUR, MINUTE, SECOND

      FRACTIONTIME = (HOUR + (MINUTE + SECOND / 60.D0) / 60.D0) / 24.D0

    END FUNCTION FRACTIONTIME


    SUBROUTINE GREGORIAN(JD, YEAR, MONTH, DAY, HOUR, MINUTE, SECOND)

      IMPLICIT NONE
      REAL(KIND=8), INTENT(IN) :: JD
      INTEGER, INTENT(OUT) :: YEAR, MONTH, DAY, HOUR, MINUTE, SECOND
      REAL(KIND=8) :: JT
      INTEGEr :: I, J, K, L, N

      L = INT(JD) + 68569
      N = 4 * L / 146097
      L = L - (146097 * N + 3) / 4
      I = 4000 * (L + 1) / 1461001
      L = L - 1461 * I / 4 + 31
      J = 80 * L / 2447
      K = L - 2447 * J / 80
      L = J / 11
      J = J + 2 - 12 * L
      I = 100 * (N - 49) + I + L

      YEAR = I
      MONTH = J
      DAY = K

      JT = DMOD(JD, 1.D0) * 24.D0
      HOUR = INT(JT)
      JT = DMOD(JT, 1.D0) * 60.D0
      MINUTE = INT(JT)
      JT = DMOD(JT, 1.D0) * 60.D0
      SECOND = NINT(JT)

      IF (SECOND == 60) THEN
          SECOND = SECOND - 60
          MINUTE = MINUTE + 1
      END IF

    END SUBROUTINE GREGORIAN

end module

! By: http://fortranwiki.org/fortran/show/strtok
CHARACTER * 255 FUNCTION strtok (source_string, delimiters)

! @(#) Tokenize a string in a similar manner to C routine
! strtok(3c).
!
! Usage:  First call STRTOK() with the string to tokenize as
! SOURCE_STRING,
! and the delimiter list used to tokenize SOURCE_STRING in
! DELIMITERS.
!
! then, if the returned value is not equal to CHAR(0), keep
! calling until it is
! with SOURCE_STRING set to CHAR(0).
!
! STRTOK will return a token on each call until the entire
! line is processed,
! which it signals by returning CHAR(0).
!
! Input:  source_string =   Source string to tokenize.
! delimiters    =   delimiter string.  Used to determine the
! beginning/end of each token in a string.
!
! Output: strtok()
!
! LIMITATIONS:
! can not be called with a different string until current string is
! totally processed, even from different procedures
! input string length limited to set size
! function returns fixed 255 character length
! length of returned string not given

! PARAMETERS:
      CHARACTER(len=*), intent(in)  :: source_string
      CHARACTER(len=*), intent(in)  :: delimiters

! SAVED VALUES:
      CHARACTER(len=255), save :: saved_string
      INTEGER, save :: isaved_start  ! points to beginning of unprocessed data
      INTEGER, save :: isource_len   ! length of original input string

! LOCAL VALUES:
      INTEGEr :: ibegin        ! beginning of token to return
      INTEGEr :: ifinish       ! end of token to return

      ! initialize stored copy of input string and pointer into input
      ! string on first call
      IF (source_string(1:1) .NE. CHAR(0)) THEN
          isaved_start = 1                 ! beginning of unprocessed data
          saved_string = source_string     ! save input string from first call in series
          isource_len = LEN(saved_string)  ! length of input string from first call
      ENDIF

      ibegin = isaved_start

      DO
         IF ( (ibegin .LE. isource_len) .AND. (INDEX(delimiters, saved_string(ibegin:ibegin)) .NE. 0)) THEN
             ibegin = ibegin + 1
         ELSE
             EXIT
         ENDIF
      ENDDO

      IF (ibegin .GT. isource_len) THEN
          strtok = CHAR(0)
          RETURN
      ENDIF

      ifinish = ibegin

      DO
         IF ((ifinish .LE. isource_len) .AND. (INDEX(delimiters, saved_string(ifinish:ifinish)) .EQ. 0)) THEN
             ifinish = ifinish + 1
         ELSE
             EXIT
         ENDIF
      ENDDO

      ! strtok = "["//saved_string(ibegin:ifinish-1)//"]"
      strtok = saved_string(ibegin:ifinish - 1)
      isaved_start = ifinish

END FUNCTION strtok
