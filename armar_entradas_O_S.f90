!****************************************************************************
!*																			*
!*      PROGRAMA ARMAR_ENTRADAS_O_S   VERSION 1.0							*
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
!*  Programa ARMAR_ENTRADAS_O_S                         					*
!*												        					*
!*  Genera entradas de ORIGEN_S para un elemento combustible de uranio      *
!*  natural tipo CANDU para el cálculo del calor de decaimiento a  		    *
!*	diferentes potencias y quemados.										*
!*                                                                          *
!****************************************************************************
!*																			*
!*  Descripción:                                                            *
!*                															*
!*  El programa genera archivos de entrada de ORIGEN_S a partir de		    *
!*  valores de entrada de potencia y quemado previamente fijados en 		*
!*	el archivo armar_entradas_O_S.inp									    *
!*																			*
!****************************************************************************
!*                                                                          *
!*	Los archivos de entrada son:											*
!*																			*
!*  - armar_entradas_O_S.inp:                                               *
!*                                                                          *  
!*  Primera linea: nro de valores de potencia, nro de valores de quemado    *
!*  a partir de la segunda linea, los P valores de potencia y, a            *
!*  continuación, los Q valores de quemado                                  *
!*						                                                    *
!*  - entrada_base_OS:                                                      *
!*                                                                          *  
!*  Entrada de ORIGEN_S a partir de la cual se generan las entradas         *
!*  correspondientes a los valores de potencia y quemado respectivos        *
!*						                                                    *
!*	- armar_entradas_O_S.fil:												*
!*                                      									*
!*	Variables y nombres correspondientes a los archivos                     *
!*  armar_entradas_O_S.inp; armar_entradas_O_S.sal; entrada_base_OS			*
!*                                                                          *
!*	Los archivos de salida son:												*
!*																			*
!*  - armar_entradas_O_S.sal:                                               *
!*                                                                          *  
!*  Breve descripción del programa. Lista de los archivos de entrada usados *
!*  con sus diferentes características                                      *
!*						                                                    *
!*	- caso_(paso de potencia)(paso de quemado).inp:                         *
!*                															*
!*  Entrada de ORIGEN_S con potencia correspondiente al 'paso de potencia'  *
!*  y quemado que se corresponde con el 'paso de quemado'. Son el objetivo  *
!*  principal de este programa.                                             *
!*                                                                          *
!****************************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
	module variables
	
	implicit none
	
	real*8,allocatable::pot(:),quem(:),t_int(:,:)
	real*8::q_lib(8)=(/480.d0, 960.d0,1920.d0,3840.d0,5760.d0,7680.d0,9600.d0,11520.d0/)
	integer::numfile(20),i,j
	character*72::linea,filnme(20),status(20),action(20)

    contains

	
	subroutine abrir_archivos

		integer*4::nf
	
		open (unit=19,file='armar_entradas_O_S.fil',status='old',action='read')
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
		
222		write(6,"(a)")'Programa armar_entradas_O_S ver. 1.0: Genera entradas de ORIGEN-S'
		write(6,"(a)")'para casos con distintas potencias y quemados '
		write(6,"(a)")' '
		write(6,"(/,a,/)")'Lista de archivos usados'
		
		do i=1,nf
			write(6,100)numfile(i), trim(filnme(i)), trim(status(i)), trim(action(i))
		end do
	
		return

100 format(i2,t5,a,t33,a,t50,a,t70,a)
	
	end subroutine abrir_archivos


!******************************************************************************
!*  La rutina ciclos calcula el número de ciclos de quemado correspondientes 
!*  para la entrada de origen a un quemado determinado. Para quem<16360 MWd/tU
!******************************************************************************

	subroutine numero_de_ciclos(q,num_ciclos)

	real*8::q
	integer*4::num_ciclos

		if(q<=q_lib(1)) then
			num_ciclos=1
			elseif(q<=q_lib(2)) then
				num_ciclos=2
				elseif(q<=q_lib(3)) then
					num_ciclos=3
					elseif(q<=q_lib(4)) then
						num_ciclos=4
						elseif(q<=q_lib(5)) then
							num_ciclos=5
							elseif(q<=q_lib(6)) then
								num_ciclos=6
								elseif(q<=q_lib(7)) then
									num_ciclos=7
			else
				num_ciclos=8
		end if

	end subroutine numero_de_ciclos


!******************************************************************************
!*  La rutina tiempos_ciclos calcula los tiempos de quemados correspondientes 
!*  a cada ciclo. Los datos de entrada son: pot, quem y num_ciclos y te devuel-
!*  ve en el arreglo t_int(num_ciclos x 10) los tiempos correspondientes a cada 
!*  ciclo en las columnas.
!******************************************************************************

	subroutine tiempos_ciclos(q,p,num_ciclos)

		real*8 :: t_aux,t_ciclo,tiempos,q_aux,q,p
		integer*4::num_ciclos
	
		q_aux=0
		t_aux=0
		do i=1,num_ciclos
			if(i.lt.num_ciclos) then
				t_ciclo=((q_lib(i)-q_aux)*0.019*1000)/p
				do j=1,10
					t_int(i,j)=t_aux+(t_ciclo*j)/10
				end do
				t_aux=t_int(i,10)
				q_aux=q_lib(i)
			else
				t_ciclo=((q-q_aux)*0.019*1000)/p
				do j=1,10
					t_int(i,j)=t_aux+(t_ciclo*j)/10
                end do 
            end if
		end do
    

	end subroutine tiempos_ciclos


!******************************************************************************
!*  La rutina agrega_ciclo agrega un ciclo en el cálculo del quemado del código
!*  ingresando el número de ciclo correspondiente y el quemado y potencia.
!*  Copia en el punto final donde se encuentra el archivo,
!******************************************************************************

   subroutine agrega_ciclo(iarch1,iarch2,num_ciclo,num_ciclos,p,q)

      character*72::texto
      real*8::t_aux,p,q
	  integer*4:: num_ciclo,num_ciclos,iarch1,iarch2


      rewind(iarch1)
      call ubicar(iarch1,'candu37 library')
      write(iarch2,'(a)')linea
      if(num_ciclo.eq.1) then
         write(texto,'(a,i1,a)')'3$$ 33 a3 ',num_ciclo,' 238 a11 2 a16 2 a33 44 e t'
      else 
         write(texto,'(a,i1,a)')'3$$ 33 a3 ',num_ciclo,' 238 a11 2 a33 44 e t'
      end if
      write(iarch2,'(a)')trim(texto)
      read(iarch1,'(a)')linea
      read(iarch1,'(a)')linea
      write(iarch2,'(a)')linea
      if(num_ciclo.eq.1) then
	     if(num_ciclos.eq.1) then
		    write(texto,'(a)')'56$$ 10 10 a10 0 a13 3 a15 3 a18 1 e'
			write(iarch2,'(a)')trim(texto)
          else   
			read(iarch1,'(a)')linea
            write(iarch2,'(a)')linea
		 end if
	     t_aux=t_int(num_ciclo,10)/t_int(num_ciclos,10)
         write(texto,'(a,f7.4,a)')'57** 0 a3 1e-05 ',t_aux,' e t'
       else
	     if(num_ciclo.eq.num_ciclos) then
	        write(texto,'(a)')'56$$ 10 10 a10 10 a15 3 a18 1 e'
			write(iarch2,'(a)')trim(texto)
		  else	
			write(texto,'(a)')'56$$ 10 10 a6 3 a10 10 a15 3 a18 1 e'
			write(iarch2,'(a)')trim(texto)
         end if   
		 t_aux=(t_int(num_ciclo,10)-t_int(num_ciclo-1,10))/t_int(num_ciclos,10)
         write(texto,'(a,f8.3,a,f10.7,a)')'57** ',t_int(num_ciclo-1,10),' a3 1e-05 ',t_aux,' e t'
      end if
      write(iarch2,'(a)')trim(texto)
	  if(q.lt.q_lib(num_ciclo)) then
	     write(texto,'(a,i2,a,f9.3,a,f9.3,a)')'cycle ',num_ciclo,' down - embalse un ',p,' kW hasta ',q,' MWd/tU'
	   else
	     write(texto,'(a,i2,a,f9.3,a,f9.3,a)')'cycle ',num_ciclo,' down - embalse un ',p,&
		  ' kW hasta ',q_lib(num_ciclo),' MWd/tU'
	  end if
      write(iarch2,'(a)')trim(texto)
      call ubicar(iarch1,'0.019 MTU')
      write(iarch2,'(a)')linea 
      write(texto,'(a,f5.3)')'58** f',p/1000
      write(iarch2,'(a)')trim(texto)
      if(num_ciclo.eq.1) then
	     write(texto,'(a,f8.3,a,f8.3,a,f8.3,a,f8.3,a,f8.3)')'60** ',t_int(num_ciclo,1),&
	      ' ',t_int(num_ciclo,2),' ',t_int(num_ciclo,3),' ',t_int(num_ciclo,4),' ',t_int(num_ciclo,5)+0.002
	   else
	     write(texto,'(a,f8.3,a,f8.3,a,f8.3,a,f8.3,a,f8.3)')'60** ',t_int(num_ciclo,1),&
	      ' ',t_int(num_ciclo,2),' ',t_int(num_ciclo,3),' ',t_int(num_ciclo,4),' ',t_int(num_ciclo,5)
	  end if 
	  write(iarch2,'(a)')trim(texto)
      write(texto,'(a,f8.3,a,f8.3,a,f8.3,a,f8.3,a,f8.3)')' ',t_int(num_ciclo,6),' ',t_int(num_ciclo,7),&
	   ' ',t_int(num_ciclo,8),' ',t_int(num_ciclo,9),' ',t_int(num_ciclo,10)
	  write(iarch2,'(a)')trim(texto)
      call ubicar(iarch1,'66$$')
	  if(num_ciclo.eq.1) then
         write(iarch2,'(a)')linea
	   else
	     write(texto,'(a)')'66$$ a1 2 a5 2 a9 2 e t'
         write(iarch2,'(a)')texto
	  end if
	  if(num_ciclo.eq.num_ciclos.and.num_ciclo.ne.1) then
	     read(iarch1,'(a)')linea
		 read(iarch1,'(a)')linea
		 read(iarch1,'(a)')linea
		 read(iarch1,'(a)')linea
	  endif

   end subroutine agrega_ciclo   
   

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

	end module variables

!******************************************************************************
!******************************************************************************
!******************************************************************************
!******************************************************************************
	
	program armar_entrada_O_S
	use variables
	use dfport
	implicit none
	
	character*13::file7
    integer*4::num_pot,num_quem,ipot,iquem,ic,nc,U,errnum

    call abrir_archivos
	!
	!Lectura del archivo de potencias y quemados
	!
	read(5,*)num_pot,num_quem
	allocate(pot(num_pot), quem(num_quem))
	read(5,*)(pot(i),i=1,num_pot)
	read(5,*)(quem(i),i=1,num_quem)
	!
	! Generacion de los casos
	!
    do ipot=1,num_pot
	   do iquem=1,num_quem
	      write(file7,"(a,i2.2,i2.2,a)")'caso_',ipot,iquem,'.inp'
	      open(unit=7,file=file7,status='unknown',action='write')
		  call numero_de_ciclos(quem(iquem),nc)
		  allocate(t_int(nc,10))
		  call tiempos_ciclos(quem(iquem),pot(ipot),nc)
		  rewind(1)
		  read(1,'(a)')linea
          if(index(linea,'=arp').gt.0)then
		     write(7,"(a)")linea
		     read(1,"(a)")linea
		     write(7,"(a)")linea
			 read(1,"(a)")linea
		     write(7,"(a)")linea
		     write(7,"(i1)")nc 	          
             do ic=1,nc
			    if(ic.eq.1)write(7,"(es12.6)")((q_lib(1))*0.019*1000)/pot(ipot)+t_int(ic,10)/10000
				if(ic.eq.2)write(7,"(es12.6)")t_int(ic,10)-t_int(ic-1,10)-t_int(ic-1,10)/10000
				if(ic.gt.2)write(7,"(es12.6)")t_int(ic,10)-t_int(ic-1,10)
             end do
			 do ic=1,nc
			    write(7,"(es11.5)")pot(ipot)/19d0
             end do
             do ic=1,nc
			    write(7,"(i1)")1
             end do
			 call ubicar(1,'0.8121')
			 write(7,"(a)")linea
			 do i=1,4
				read(1,"(a)")linea
				write(7,"(a)")linea
			 end do
			 do ic=1,nc
			    call agrega_ciclo(1,7,ic,nc,pot(ipot),quem(iquem))
				if(ic.eq.1) then
				   call ubicar(1,'73$$')
				   write(7,'(a)')linea
				   read(1,'(a)')linea
				   write(7,'(a)')linea
				   read(1,'(a)')linea
				   write(7,'(a)')linea
				   read(1,'(a)')linea
				   write(7,'(a)')linea
				endif
			 end do
			 do i=1,437
			    read(1,'(a)')linea
				write(7,'(a)')linea
			 end do
! 			 U=system("call runscale "//file7)
!			 if(U.eq.-1) then                  !Esto es para que se corran los casos
!			    errnum=ierrno( )               !de origen automáticamente
!                print *,'Error ',errnum
!             endif
          endif
		  deallocate(t_int)
		  close(7)
       end do
    end do

	end program armar_entrada_O_S
	
