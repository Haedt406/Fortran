	program fortranFun
	implicit none
	integer ::  uInput
	character(2) :: hour, minute, month, day, year*4, date*8, time*10
	call DATE_AND_TIME(date=date, time=time)	
	hour = time(1:2)
	minute = time(3:4)
	month = date(5:6)
	day = date(7:8)
	year = date(1:4)	
	write(*,*) month,'-', day,'-', year, ', ', hour, ':', minute

	do 
		write (*,*) 'Enter a number between 1 and 99'
		read  (*,*) uInput
		write (*,*) 'your input was', uInput
	if (uInput < 100 .and. uInput > 0) EXIT
		write(*,*) 'That was not a valid entry. Please try again'
	end do
	call coinCounter(uInput)
	contains	
	subroutine coinCounter(in)
		implicit none
		integer, intent(out) :: in
		integer :: lunkVal = 30, loonVal = 15, litVal = 5, poonVal = 1
		character :: litSTR*10, poonSTR*10, lunkSTR*8,  loonSTR*10
		lunkSTR = lunkFUN(in, lunkVal)
		loonSTR = loonFUN(in, loonVal)
		litSTR = litFUN(in, litVal)
		poonSTR = poonFUN(in, poonVal)
		write(*,*) trim(lunkSTR),trim(loonSTR),trim(litSTR),trim(poonSTR)

	end subroutine coinCounter

	character*8 function lunkFUN(in, lunkVal)
		implicit none
		integer, intent(out) :: in, lunkVal
		character*0 :: eString
		character*1 :: toString
		if ( in/lunkVal > 0) then
			write(toString,'(I1)') in/lunkVal
			in = MOD(in,lunkVal)
			lunkFUN = toString//' Lunker'
		else
			 lunkFUN = eString
		end if
	end function lunkFUN
		
	character*10 function loonFUN(in, loonVal)
		implicit none
		integer, intent(out):: in, loonVal
		character*0 :: eString
		character*1 :: toString
		if (in/loonVal > 0) then
			write(toString, '(I1)') in/loonVal
			in = MOD(in,loonVal)
			loonFUN = ','// toString//' Loonter'
			else
				loonFUN = eString
			end if
	end function loonFUN
		
	character*10 function litFUN(in, litVal)
		implicit none
		integer, intent(out) :: in, litVal
		character*0 :: eString
		character*1 :: toString
		if (in/litVal > 0) then
			write(toString,'(I1)') in/litVal
			in = MOD(in,litVal)
			litFUN = ', '//toString//' Little'
		else
			 litFUN = eString
		end if
	end function litFUN

	character*10 function poonFUN(in, poonVAL)
		implicit none
		integer, intent(out) :: in, poonVal
		character*0 :: eString
		character*1 :: toString
		if (in/poonVal > 0) then
			write(toString,'(I1)') in/poonVal
			in = MOD(in,poonVal)
			poonFUN = ', '//toString//' Pooney'
		else
			poonFUN = eString
		end if
	end function poonFUN

	end program fortranFun	
