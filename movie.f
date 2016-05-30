	PROGRAM MOVIE
         common/orderlimit/ c6min,c6max,rgmin,rgmax,psi6min,psi6max
        INTEGER, PARAMETER  ::  NMAX = 1000
	INTEGER SNAPSHOT, inisnap, rate, i, dum, N, col1, col2,
     +  numsnap,grainpaint,grainnum
	DOUBLE PRECISION dum2, xtemp(nmax), ytemp(nmax), ztemp(nmax),
     + conn6(nmax), avgconn6, psi6,interpsi6,outerc6,red, green, blue,
     + dist(nmax), distmax, px(10), py(10), phi(nmax), psiloc(nmax), 
     + rmin
	INTEGER polygon
	REAL X,Y,Z, radius, colin(3), colend(3),dg,
     +	col(3), kappa, glbq6, v1, v2, v3, val,  cav
	REAL,ALLOCATABLE:: DATARRAY(:,:)
	CHARACTER*60 LINE
	CHARACTER mcdyn*2, coordfile*30, conn6file*30, glbq6file*30, 
     +	kappafile*30, fileid*3, outfile*30, scrfile*30, jnum*3,
     +folderpath*150,tempf*3,filename*140,povfolder*150
      integer paintop,startframe,totalframe,skip,originframe,framecount
      double precision c6min,c6max, rgmin,rgmax, psi6min,psi6max,rgmean
      double precision, parameter :: pi=3.1415926

	CHARACTER*1 TEMP1
	CHARACTER*2 TEMP2
	CHARACTER*3	TEMP3
	CHARACTER*4	TEMP4
	CHARACTER*5	TEMP5
	

	INTEGER*1	DAT
	open(1,file='run.txt')
	read(1,*)
	read(1,*) N
	read(1,*)
	read(1,'(a120)') folderpath
	read(1,*)
	read(1,'(a150)') povfolder
	read(1,*)
	read(1,*) coordfile
	read(1,*)
	read(1,*) startframe
	read(1,*)
	read(1,*) totalframe
	read(1,*)
	read(1,*) skip
	read(1,*)
	read(1,*) cav
	read(1,*) 
      read(1,*) polygon 



	close(1)


	OPEN(30,FILE='xyz_0.dat')	!coordinate file



      framecount=0
      originframe=0
	DO while(framecount .le. totalframe)
	

        	DO K=1,N
		read(30,*, end=1001) dum,xtemp(k),ytemp(k), phi(k)
      ENDDO
      
      originframe=originframe+1
      
        

        if(originframe .ge. startframe .and. 
     +   mod(originframe,skip) .eq. 0) then
            framecount=framecount+1
		IF(framecount .LT.10)THEN
		
          write(tempf,'(i1)') framecount
        else if (framecount .lt. 100) then
           
           write(tempf, '(i2)') framecount
           
           else
           
           write(tempf, '(i3)') framecount  

		ENDIF

        filename=trim(povfolder)//trim(tempf)//'.pov'


		OPEN(20,FILE=FILENAME)	!POV-RAY OUTPUT FILE

	WRITE(20,*)'#declare cx=0;'
	WRITE(20,*)'#declare cy=0;'
	WRITE(20,*)'#declare cz=70;'
	WRITE(20,*)'#declare ca=',cav,';'

	WRITE(20,*)'camera{location<cx,cy,cz> look_at<0,0,0> angle ca}'
	WRITE(20,*)'light_source{<cx,cy,cz> color' 
	WRITE(20,*)'rgb<1.5,1.5,1.5> shadowless}'        


       
	WRITE(20,*)'#declare ball=sphere{<0,0,0> 1' 
	WRITE(20,*)'pigment{rgbt<1,1,1,0>}}'  
	
	WRITE(20,*)'#declare electrode=box{<-0.5,-0.5,-0.5>,<0.5,0.5,0.5>' 
	WRITE(20,*)'pigment{rgbt<1,1,1,0>}}'     
         


      rmin=2.2
       call calorder(psiloc, rmin, polygon, n, xtemp, ytemp, phi)
      do K=1,N
          
       
          
          
          do kk=1, polygon
          px(kk)=xtemp(k)+cos(phi(k)-(0.5-1.0/polygon)*pi+
     +     (kk-1)*2.0/polygon*pi);
          py(kk)=ytemp(k)+sin(phi(k)-(0.5-1.0/polygon)*pi+
     +     (kk-1)*2.0/polygon*pi);
        end do
              
      Red=1-dble(psiloc(k))
      Green=1-dble(psiloc(k))
!      Red = 0
!      Green = 0
      Blue= 1   
      		
		WRITE(20,*) 'polygon{', int(polygon),','
          
          do kk=1, polygon
          write(20,*) '<',px(kk),',', py(kk),'>'
          end do
          write(20,*)	'pigment{rgbt<',real(red),',',real(green),
     +     ',',real(blue),',0>}}'
      end do

	


		


      end if

	ENDDO
      
      

1001	CLOSE(20)


	END
