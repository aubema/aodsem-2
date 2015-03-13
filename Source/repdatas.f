c
c        prog pour fabriquer les fichier d assimilation
c        repetee pour AODSEM
c
        program repdatas
        real lcellx,lat0,lon0,avgbox
        real*8 hre(365),min(365),sec(365),jou(365),moi(365),ann(365)
        real*8 hren,minn,secn,joun,moin,annn
        real*8 jday1,jday2,timestep,assitm,nwtime
        integer buffer,srcsw,wavel,ncellx,ncelly,nasimil,filenb,nstep
        integer begtype,wetsw,aod3dsw
        character*4 caract
        character*60 file1,miefile,initfile
cJDGcD
        character assifile(365)*60
cJDGcF
        open(unit=1,file='repdatas.par',status='old')
           read(1,*) aod3dsw
           read(1,*) begtype
cJDGcD
           read(1,*) initfile
cJDGcF
           read(1,*) timestep
           read(1,*) buffer
           read(1,*) srcsw
           read(1,*) wetsw
           read(1,*) wavel
           read(1,*) ncellx
           read(1,*) ncelly
           read(1,*) lcellx
           read(1,*) lat0
           read(1,*) lon0
           read(1,*) avgbox
           read(1,*) miefile
           read(1,*) nasimil
           read(1,*) filenb
           read(1,*) hre(1),min(1),sec(1),jou(1),moi(1),ann(1)
           do i=2,nasimil+1
              read(1,*) hre(i),min(i),sec(i),jou(i),moi(i),ann(i)
              read(1,*) assifile(i-1)
           enddo
        open(unit=3,file='partrun',status='unknown')
        do i=1,nasimil
              
           call int2char(i,caract)
           write(3,*) 'datas < part-'//caract
           open(unit=2,file='part-'//caract,status='unknown')
             call julian(hre(i),min(i),sec(i),jou(i),moi(i),ann(i),
     +       jday1)
             call julian(hre(i+1),min(i+1),sec(i+1),jou(i+1),moi(i+1),
     +       ann(i+1),jday2)
            
             nstep=nint((jday2-jday1)*24.*60./timestep)
c JDGD
            assitm=real(nstep)*timestep/24./60.
            nwtime=jday1+assitm
            call timedate(hren,minn,secn,joun,moin,annn,nwtime)
            hre(i+1)=hren
            min(i+1)=minn
            sec(i+1)=secn
            jou(i+1)=joun
            moi(i+1)=moin
            ann(i+1)=annn
c JDGF
             write(2,*) aod3dsw
             write(2,*) wetsw
             if (i.eq.1) then
                write(2,*) begtype
                if (begtype.eq.0) then
c JDGR                   call int2char(filenb,caract)
c JDGR                   write(2,*) 'ana-'//caract
                        write(2,*) initfile
                else
c JDGR                   call int2char(filenb,caract)
c JDGR                   write(2,*) caract
                        write(2,*) initfile
                endif
             else
                write(2,*) '0'
                call int2char(filenb-1,caract)
                write(2,*) 'ana-'//caract
             endif
c JDGR             call int2char(filenb+nstep,caract)
c JDGR             write(2,*) caract
c JDGD
             write(2,*) assifile(i)
             write(2,*) filenb
c JDGF
             write(2,*) int(hre(i)),int(min(i)),int(sec(i)),
     +       int(jou(i)),int(moi(i)),int(ann(i))
             write(2,*) int(hre(i+1)),int(min(i+1)),int(sec(i+1)),
     +       int(jou(i+1)),int(moi(i+1)),int(ann(i+1))
             write(2,*) nint(timestep)
             write(2,*) buffer
             write(2,*) srcsw
             write(2,*) wavel
c JDGR             write(2,*) ncellx
c JDGR             write(2,*) ncelly
c JDGR             write(2,*) int(lcellx)
c JDGR             write(2,*) lat0
c JDGR             write(2,*) lon0
             write(2,*) int(avgbox)
             write(2,*) miefile
c JDGR             do n=1,nstep
c JDGR                call int2char(filenb+n,caract)
c JDGR                write(2,*) 'ana-'//caract
c JDGR                write(2,*) 'ana-'//caract             
c JDGR                write(2,*) 'bgd-'//caract
c JDGR             enddo
c JDGR             write(2,*) 'cor-'//caract
c JDGR             write(2,*) 'wet-'//caract
c JDGR             write(2,*) 'dry-'//caract
             filenb=filenb+nstep
           close(unit=2)     
        enddo
        close(unit=3)

        stop
        end
c
c -----------------------------------------------------
c
c  Routine pour la convertion d une nombre < 10000 vers caractere
c
c             
         subroutine int2char(value,caract)
         character*1 table(10)
         character*4 caract
         integer nm,nce,nd,nu,value
         data table /'0','1','2','3','4','5','6','7','8','9'/
         caract=''
         nm=int(value/1000)
         nce=(value-nm*1000)/100
         nd=(value-nm*1000-nce*100)/10
         nu=value-nm*1000-nce*100-nd*10
         nm=nm+1
         nce=nce+1
         nd=nd+1
         nu=nu+1
         caract=table(nm)//table(nce)//table(nd)//table(nu)
         return
         end
