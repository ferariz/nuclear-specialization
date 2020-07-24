!****************************************************************************
!*																			*
!*      PROGRAMA CdD_OS   VERSION 1.0           							*
!*																			*
!****************************************************************************
!*																			*
!*	Historia:																*
!*																			*
!*  Version    Programador     Fecha		 Cambios	    				*
!*    1.0      F.Arizmendi   06/11/2009                                     *
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
!*  Programa CdD_OS                                      					*
!*												        					*
!*  Extrae información de calores de decaimiento de salidas de ORIGEN_S     *
!*  previamente obtenidas del programa armar_entradas_O_S.f90               *
!*                                                                          *
!****************************************************************************
!*																			*
!*  Descripción:                                                            *
!*                															*
!*  El programa genera una base de datos de potencia, quemado y calores     *
!*  de decaimiento a partir de salidas de ORIGEN_S. Está pensado para 		*
!*	continuar el trabajo empezado por armar_entradas_O_S.f90                *
!*																			*
!****************************************************************************
!*                                                                          *
!*	Los archivos de entrada son:											*
!*																			*
!*	_ calor_decaimiento_O_S.fil:                                            *
!*																			*
!*	Lista con los archivos a utilizar                                       *
!*																			*
!*  - calor_decaimiento_O_S.inp:                                            *
!*                                                                          *  
!*  Primera linea: nro de valores de potencia, nro de valores de quemado    *
!*  a partir de la segunda linea, los P valores de potencia y, a            *
!*  continuación, los Q valores de quemado. Todos valores correspondientes  *
!*	a los casos de ORIGEN_S generados con armar_entradas_O_S.f90            *
!*                                                                          *
!* - caso_(paso de potencia)(paso de quemado).out:                          *
!*                                                                          *  
!*  Salidas de ORIGEN_S obtenidas con el programa amramar_entradas_O_S.f90. *
!*  A partir de éstas se genera la base de datos de calores de decaimiento  *
!*						                                                    *
!*                                                                          *
!*	Los archivos de salida son:												*
!*																			*
!*  - calor_decaimiento_O_S.sal:                                            *
!*                                                                          *  
!*  Breve descripción del programa. Lista de los archivos de entrada usados *
!*  con sus diferentes características                                      *
!*                                                                          *
!*  - calor_decaimiento_O_S.dat:                                            *
!*						                                                    *
!*  Extracto de información de las salidas de ORIGEN_S con información de   *
!*  portencia y quemado, con los respectivos calores de decaimiento en      *
!*  función del tiempo                                                      *
!*                                                                          *
!****************************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
	module variables
	
	implicit none
	
	integer::i,j,numfile(20)
	character*72::status(20),action(20),filnme(20)
	character*200::linea

    contains

	
	subroutine abrir_archivos

		integer*4::nf
	
		open (unit=19,file='calor_decaimiento_O_S.fil',status='old',action='read')
		nf=0
		do i=1,30
			
			read(19,*,END=222)numfile(i),filnme(i)
			nf=nf+1
			
			if(numfile(i).eq.1.or.numfile(i).eq.5) then
				status(i)='old'
				action(i)='read'
			end if
			
			if(numfile(i).eq.6.or.numfile(i).eq.7) then
			
				status(i)='unknown'
				action(i)='write'
			end if
			
			open(numfile(i),file=trim(filnme(i)),status=status(i),action=action(i))
		end do
		
222		write(6,"(a)")'Programa calor_decaimiento_O_S ver. 1.0: Analiza entradas de ORIGEN-S'
		write(6,"(a)")'y calcula el calor de decaimiento en función del tiempo '
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


     !*************************************************************
     ! La rutina ubicar ubica la primera vez que aparece el texto
     ! texto en el archivo iarch a partir de la posicion en que
     ! esta la lectura. Devuelve st=1 si encontro el string o st=0
	 ! si no lo encuentra. Tambien descarta lineas comentadas.
     !*************************************************************

subroutine ubicarb(iarch,texto,st)

     character*(*)::texto !,linea_min,texto_min
	 character*130::texto1
	 integer*2 :: iarch,j,jj,n,st
     integer*4 :: i
	 texto1=texto
     do i=1,1000000
        read(iarch,"(a)",end =2)linea
!       call upper_case(linea)
		n=len_trim(linea)
!       call upper_case(texto1)
!       write(6,*)'entro en ubicar,buscando '//trim(texto)
!       write(6,"(a)")linea
        if (index(linea,trim(texto1)).gt.0)then
		      if (linea(1:3).ne."***")then
			     st=1
                 return
			  endif
           endif
        end do
     2  do j=1,20
           if(numfile(j).eq.iarch)jj=j
        end do
!        write(6,"(a,a,a,a,a)")'ubicar'&
!         ,' no pudo encontrar el texto ',texto,' en el archivo ',&
!         filnme(jj)
		 st=0
!        stop
         return
     end subroutine ubicarb

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


	end module variables

!******************************************************************************
!******************************************************************************
!******************************************************************************
!******************************************************************************
	
	program calor_decaimiento_O_S
	use variables

	implicit none

	integer*2::bool
	integer*4::num_pot,num_quem,iquem,ipot,c(80),f(80),N
	integer*4::num_ciclos_decai,aux,N_calores,k,j_i,j_o
	real*8,allocatable::pot(:),quem(:),calor(:),calor_pf(:)
	character*13::file4,texto

	!HAY QUE VERIFICAR EL NÚMERO DE CICLOS DE DECAIMIENTO
	num_ciclos_decai=5
	N_calores=51
	call abrir_archivos
	!
	!Lectura del archivo de potencias y quemados
	!
	read(5,*)num_pot,num_quem
	allocate(pot(num_pot),quem(num_quem))
	read(5,*)(pot(i),i=1,num_pot)
	read(5,*)(quem(i),i=1,num_quem)
	allocate(calor(N_calores))
    allocate(calor_pf(N_calores))
	do ipot=1,num_pot
	   do iquem=1,num_quem
	      write(file4,"(a,i2.2,i2.2,a)")'caso_',ipot,iquem,'.out'
	      open(unit=4,file=file4,status='old',action='read')
		  call ubicarb(4,'discharge',bool)    !esto es para distinguir entra las dos salidas del O_S
		  rewind(4)
		  do k=1,num_ciclos_decai
		     write(texto,'(a,i1,a)')'cycle ',k,' down'
		     call ubicar(4,texto)
		     call ubicar(4,'nuclide thermal power')
		     read(4,*)linea
		     call ubicar(4,'nuclide thermal power')
		     read(4,*)linea
		     call ubicar(4,'nuclide thermal power')
		     call ubicar(4,'total')
		     call separar(linea,c,f,N)
		     if(k.eq.1.and.bool.eq.1) then
			    j_i=3
				j_o=1
			  else
			    j_i=2
				j_o=0
			 end if
			 if(k.ne.num_ciclos_decai) then
		        do j=j_i,N-1
			       aux=(k-1)*10
		           read(linea(c(j):f(j)),*)calor(aux+j-1-j_o)
			    end do
			  else
			    do j=j_i,N
			       aux=(k-1)*10
		           read(linea(c(j):f(j)),*)calor(aux+j-1-j_o)
			    end do
			 end if
			 call ubicar(4,'nuclide thermal power')
		     read(4,*)linea
		     call ubicar(4,'nuclide thermal power')
		     call ubicar(4,'total')
			 call separar(linea,c,f,N)
			 if(k.ne.num_ciclos_decai) then
			    do j=j_i,N-1
			       read(linea(c(j):f(j)),*)calor_pf(aux+j-1-j_o)
			    end do
			 else
			    do j=j_i,N
			       read(linea(c(j):f(j)),*)calor_pf(aux+j-1-j_o)
			    end do
			 end if
		  end do
		  write(7,'(f4.0,1x,f6.0,1x,51(es10.3,1x))')pot(ipot),quem(iquem),((calor(j)+calor_pf(j))/(pot(ipot)*1000),j=1,N_calores)
	   end do
	end do
    deallocate(pot,quem,calor,calor_pf)

	end program calor_decaimiento_O_S