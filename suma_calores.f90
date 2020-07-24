!****************************************************************************
!*																			*
!*      PROGRAMA suma_calores   VERSION 1.0        							*
!*																			*
!****************************************************************************
!*																			*
!*	Historia:																*
!*																			*
!*  Version    Programador     Fecha		 Cambios	    				*
!*    1.0      F.Arizmendi   13/11/2009                                     *
!*                                                        					*
!****************************************************************************
!*																			*
!*  Lenguaje:																*
!*																			*
!*	Version 1:	Fortran 90	Compilador CVF90 (Compaq Visual F90 ver 6.6b)	*
!*												            				*
!****************************************************************************
!*																			*
!*  Objetivo:								    							*
!*																			*
!*  Programa suma_calores                                  					*
!*												        					*
!*  A partir de la base de datos de calores de decaimiento obtenida del     *
!*  código CdD_OS.f90, interpola y suma los valores correspondientes a los  *
!*  valores de potencia y quemado de los manojos de combustibles de la CNE  *
!*                                                                          *
!****************************************************************************
!*																			*
!*  Descripción:                                                            *
!*                															*
!*  El programa calculo el calor de decaimiento en función del tiempo para  *
!*  valores arbitrarios de potencia y quemado. En este caso original, estos *
!*	valores se corresponden con una configuración típica de parada de el    *
!*  núcleo de la Central Nuclear Embalse                                    *
!*																			*
!****************************************************************************
!*                                                                          *
!*	Los archivos de entrada son:											*
!*																			*
!*	_ base_de_datos.dat:                                                    *
!*																			*
!*	Curvas de calor de decaimiento en función de potencia, quemado y tiempo *
!*																			*
!*	_ potyquem.dat:                                                         *
!*																			*
!*	Potencias y quemados correspondientes a la base de datos                *
!*																			*
!*  - gXXXX.prp:                                                            *
!*                                                                          *
!*  Archivo con la información de potencias y quemados correspondientes a   *
!*  una parada determinada del reactor de la CNE                            *
!*                                                                          *
!*  - suma_calores.inp:                                                     *
!*                                                                          *  
!*  Valores de potencia y quemado a interpolar y sumar. Caseo original:     *
!*  configuración típica de parada del núcleo de la CNE                     *
!*                                                                          *
!* - suma_calores.fil:                                                      *
!*                                                                          *  
!*  Lista con los archivos a utilizar y crear con sus respectivos números   *
!*  correspondientes al valor de las variables "iarch"                      *
!*
!*						                                                    *
!*                                                                          *
!*	Los archivos de salida son:												*
!*																			*
!*  - suma_calores.sal:                                                     *
!*                                                                          *  
!*  Breve descripción del programa. Lista de los archivos de entrada usados *
!*  con sus diferentes características                                      *
!*                                                                          *
!*  - suma_calores.dat:                                                     *
!*						                                                    *
!*  Suma de los calores de decaimiento en función del tiempo                *
!*                                                                          *
!****************************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
	module variables
	
	implicit none
	
	integer::i,j,numfile(20)
	character*72::status(20),action(20),filnme(20)
	character*600::linea
	real*8,allocatable::quem(:),pot(:),calores(:,:)
	integer*4::num_pot,num_quem,cant_tiempos

    contains

	
	subroutine abrir_archivos

		integer*4::nf
	
		open (unit=19,file='suma_calores.fil',status='old',action='read')
		nf=0
		do i=1,30
			
			read(19,*,END=222)numfile(i),filnme(i)
			nf=nf+1
			
			if(numfile(i).eq.2.or.numfile(i).eq.3.or.numfile(i).eq.4.or.numfile(i).eq.5) then
				status(i)='old'
				action(i)='read'
			end if
			
			if(numfile(i).eq.6.or.numfile(i).eq.7.or.numfile(i).eq.8) then
			
				status(i)='unknown'
				action(i)='write'
			end if
			
			open(numfile(i),file=trim(filnme(i)),status=status(i),action=action(i))
		end do
		
222		write(6,"(a)")'Programa suma_calores ver. 1.0: Suma los calores de decaimiento'
		write(6,"(a)")'en función del tiempo para distintas potencias y quemados.'
        write(6,"(a)")'En este caso, se ingresan las potencias y quemados se corresponden'
		write(6,"(a)")'con una configuración típica del núcleo de la CNE en una parada.'
		write(6,"(a)")' '
		write(6,"(/,a,/)")'Lista de archivos usados'
		
		do i=1,nf
			write(6,100)numfile(i), trim(filnme(i)), trim(status(i)), trim(action(i))
		end do
	
		return

100 format(i2,t5,a,t33,a,t50,a,t70,a)
	
	end subroutine abrir_archivos
 

!******************************************************************************
!*  La rutina ubicar ubica la primera vez que aparece el texto 'texto'
!*  en el archivo iarch a partir de la posicion en que esta la lectura
!******************************************************************************

	subroutine ubicar(iarch,texto)
	
		character*(*)texto
		integer*4::iarch,jj

		do i=1,1000000

			read(iarch,"(a)",end =2)linea
			if (index(linea,texto).gt.0) return

		end do

2		do j=1,20
			if(numfile(j).eq.iarch)jj=j
		end do

		write(6,"(a,a,a,a)")'ubicar no pudo encontrar el texto ', texto,'  &
							en el archivo ', filnme(jj)
		return

	end subroutine ubicar

!******************************************************************************
!  la subrutina separar separa por blancos el registro reg en num_pal
!  palabras (num_pal < 80), e identifica los puntos iniciales de cada
!  palabra ic(i) e if(i)
!******************************************************************************
	
	subroutine separar(reg,ic,ifi,num_pal)
	
	integer*4 ic(80),ifi(80),num_pal,m,n
	character*(*) reg
	
		m=len_trim(reg)

		num_pal=0
	
		ic=0
		ifi=0
		if(m.eq.0) return
	
		n=1
		do i=1,m

			if(reg(i:i).ne."".and. i.eq.1) ic(1) = i
			if(i.gt.1.and.reg(i:i).ne."".and.reg(i-1:i-1).eq."") ic(n) = i
			if(reg(i:i).eq."".and.reg(i-1:i-1).ne."".and.i.gt.1) then
		
				ifi(n)=i-1
				n=n+1
				num_pal=num_pal+1
			end if

			if(i.eq.m) then
		
				ifi(n)=m
				num_pal=num_pal+1
			end if

		end do

		return

	end subroutine separar

Subroutine Akima(iv,xin,yin,xx,yy,dunit)

!********************************************************
!*          Akima spline fitting subroutine             *
!* ---------------------------------------------------- *
!* The input table is X(i), Y(i), where Y(i) is the     *
!* dependant variable. The interpolation point is xx,   *
!* which is assumed to be in the interval of the table  *
!* with at least one table value to the left, and three *
!* to the right. The interpolated returned value is yy. *
!* n is returned as an error check (n=0 implies error). *
!* It is also assumed that the X(i) are in ascending    *
!* order.                                               *
!********************************************************
  integer*2 :: i,iv,n, dunit
  real*8	:: xx,yy,a,b
  real*8	:: xin (0:iv), yin (0:iv)
  real*8	:: XM(0:iv+3)
  real*8	:: Z (0:iv)
  
		n=1

		xin(0)=2.d0*xin(1)-xin(2)
!		Calculate Akima coefficients, a and b
		do i = 1, iv-1
			!Shift i to i+2
			XM(i+2)=(yin(i+1)-yin(i))/(xin(i+1)-xin(i))
		end do
		XM(iv+2)=2.d0*XM(iv+1)-XM(iv)
		XM(iv+3)=2.d0*XM(iv+2)-XM(iv+1)
		XM(2)=2.d0*XM(3)-XM(4)
		XM(1)=2.d0*XM(2)-XM(3)
		do i = 1, iv
			a=dabs(XM(i+3)-XM(i+2))
			b=dabs(XM(i+1)-XM(i))
			if (a+b.ne.0.d0) goto 100
			Z(i)=(XM(i+2)+XM(i+1))/2.d0
			goto 200
100			Z(i)=(a*XM(i+1)+b*XM(i+2))/(a+b)
200		end do
		if(dunit.gt.0) then
			write(dunit,"(/,a)")'Tabla de coeficientes Akima'
			write(dunit,"(a)")'i,XM(I),dabs(XM(i+3)),Z(i)'
			do i=1,iv-1
				write(dunit,"(i4,4es20.7)")i,XM(I),dabs(XM(i+3)),z(i)
			end do   
        endif

		!special case xx=xin
		if (xx.eq.xin(1)) then
			yy=yin(1); return
		end if
		!Check to see if interpolation point is correct
		if (xx<xin(1).or.xx>xin(iv-2)) then
			n=0 ; stop
!		elseif (xx.eq.xin(iv-2))then
!			yy=yin(iv-2); return
		end if
		!Find relevant table interval
		i=0
300		i=i+1
		if (xx>xin(i)) goto 300
		i=i-1
		!Begin interpolation
		b=xin(i+1)-xin(i)
		a=xx-xin(i)
		yy=yin(i)+Z(i)*a+(3.d0*XM(i+2)-2.d0*Z(i)-Z(i+1))*a*a/b
		yy=yy+(Z(i)+Z(i+1)-2.d0*XM(i+2))*a*a*a/(b*b)
		return

end subroutine Akima


!******************************************************************************
!  la subrutina interpola justamente interpola los valores correspondientes a 
!  calores de decaimiento respectivos para las potencias y quemados ingresados
!  a partir de los valores de la base de datos obtenidos previamente con el
!  programa CdD_OS.f90.
!******************************************************************************
	
	subroutine interpola(iarch,p,q,k)

	real*8::p,q,c1(cant_tiempos),c2(cant_tiempos),c3(cant_tiempos),c4(cant_tiempos)
	real*8::a1,a2,a3,a4
	integer*4::b,ip,iq,iarch,col(80),fil(80),N,k
	character*15::texto

	!primero busco en qué intervalos se encuentran "p" y "q"
	i=0
	b=0
	do while(b.eq.0)
	   i=i+1
	   if(p.ge.pot(i).and.p.lt.pot(i+1)) then
	      ip=i
		  b=1
	   endif
	   if(i.ge.num_pot) then
	      b=1
		  ip=0
	   end if
	end do
	i=0
	b=0
	do while(b.eq.0)
	   i=i+1
	   if(i.lt.num_quem.and.q.ge.quem(i).and.q.lt.quem(i+1)) then
	      iq=i
		  b=1
	   endif
	   if(i.ge.num_quem) then
	      b=1
		  iq=0
	   end if
	end do
	!acá arranca la interpolación
    if(ip.ne.0.and.iq.ne.0) then
	   write(texto,'(f4.0,1x,f6.0)')pot(ip),quem(iq)
	   call ubicar(iarch,trim(texto))
	   call separar(linea,col,fil,N)
	   do i=1,cant_tiempos
	      read(linea(col(i+2):fil(i+2)),*)c1(i)
	   end do
	   write(texto,'(f4.0,1x,f6.0)')pot(ip),quem(iq+1)
	   call ubicar(iarch,trim(texto))
	   call separar(linea,col,fil,N)
	   do i=1,cant_tiempos
	      read(linea(col(i+2):fil(i+2)),*)c2(i)
	   end do
	   write(texto,'(f4.0,1x,f6.0)')pot(ip+1),quem(iq)
	   call ubicar(iarch,trim(texto))
	   call separar(linea,col,fil,N)
	   do i=1,cant_tiempos
	      read(linea(col(i+2):fil(i+2)),*)c3(i)
	   end do
	   write(texto,'(f4.0,1x,f6.0)')pot(ip+1),quem(iq+1)
	   call ubicar(iarch,trim(texto))
	   call separar(linea,col,fil,N)
	   do i=1,cant_tiempos
	      read(linea(col(i+2):fil(i+2)),*)c4(i)
	   end do
	   a1=(quem(iq+1)-q)/(quem(iq+1)-quem(iq))
	   a2=(pot(ip+1)-p)/(pot(ip+1)-pot(ip))
	   a3=(q-quem(iq))/(quem(iq+1)-quem(iq))
	   a4=(p-pot(ip))/(pot(ip+1)-pot(ip))
	   do i=1,cant_tiempos
	      calores(k,i)=c1(i)*a1*a2+c2(i)*a2*a3+c3(i)*a1*a4+c4(i)*a3*a4
	   end do
	 else
	    if(iq.eq.0) then
		   write(texto,'(f4.0,1x,f6.0)')pot(ip),quem(num_quem)
	       call ubicar(iarch,trim(texto))
	       call separar(linea,col,fil,N)
	       do i=1,cant_tiempos
	          read(linea(col(i+2):fil(i+2)),*)c1(i)
	       end do
		   write(texto,'(f4.0,1x,f6.0)')pot(ip+1),quem(num_quem)
	       call ubicar(iarch,trim(texto))
	       call separar(linea,col,fil,N)
	       do i=1,cant_tiempos
	          read(linea(col(i+2):fil(i+2)),*)c3(i)
	       end do
		   a2=(pot(ip+1)-p)/(pot(ip+1)-pot(ip))
		   a4=(p-pot(ip))/(pot(ip+1)-pot(ip))
		   do i=1,cant_tiempos
	          calores(k,i)=c1(i)*a2+c3(i)*a4
	       end do
	    end if
	end if 

	end subroutine interpola



	end module variables

!******************************************************************************
!******************************************************************************
!******************************************************************************
!******************************************************************************
	
	program suma_calores
	use variables

	implicit none

	real*8::potencia,quemado
	integer*4::cont,fi(80),c(80),nn,l,m,aux

	call abrir_archivos
	!
	!Lectura del archivo de potencias y quemados
	!
	read(3,*)num_pot,num_quem
	allocate(pot(num_pot),quem(num_quem))
	read(3,*)(pot(i),i=1,num_pot)
	read(3,*)(quem(i),i=1,num_quem)
	cant_tiempos=51
	allocate(calores(1,cant_tiempos))
	read(5,*)potencia,quemado
	call interpola(4,potencia,quemado,1)
	write(7,'(51(es13.5,1x))') (calores(1,j),j=1,cant_tiempos) 
	
	end program suma_calores