      SUBROUTINE DFLUX(FLUX,SOL,KSTEP,KINC,TIME,NOEL,NPT,COORDS,
     1 JLTYP,TEMP,PRESS,SNAME)
C
      INCLUDE 'ABA_PARAM.INC'
C     FLUX(1)��ͨ����TIME(1)������ʱ�䣻TIME(2)��ʱ��;COORDS(3)��������ԭ��;
      DIMENSION FLUX(2), TIME(2), COORDS(3)
      CHARACTER*80 SNAME
      double precision t
      real::pi=3.141592654
C     wu,���ӵ�ѹ
C     wi,���ӵ���
C     effi,����Ч��ϵ��
C     q,�绡��Ч�ȹ���W
C     v,�����ٶ�mm/s
C     q,�绡��Ч�ȹ���W
      wu=30
      wi=120
      effi=0.8
      v=1.25
C     mm��λ���ڴ˴���Ҫ����1000
      q=wu*wi*effi*1000
C     ��ǰ��������
      x=COORDS(1)
      y=COORDS(2)
      z=COORDS(3)
C     ������x0,y0,z0��ʼ
      x0=-1.25
      y0=0
      z0=0.3
C     ��Դ�ƶ�������ٶȶ���
      t=TIME(2)
      dx=v*t
      dy=0
      dz=0
C     a1,a2,b,cΪ˫�������״����
      a1=0.9
      a2=1.8
      b=0.9
      c=1
C     f1Ϊ��Դ����ϵ��
      f1=1.0
      PI=3.1415926
      heat1=6.0*sqrt(3.0)*q/(a1*b*c*PI*sqrt(PI))*f1
      heat2=6.0*sqrt(3.0)*q/(a2*b*c*PI*sqrt(PI))*(2.0-f1)
      shape1=exp(-3.0*(x-x0-dx)**2/(a1)**2-3.0*(y-y0)**2/b**2
     $	-3.0*(z-z0)**2/c**2)
      shape2=exp(-3.0*(x-x0-dx)**2/(a2)**2-3.0*(y-y0)**2/b**2
     $	-3.0*(z-z0)**2/c**2)
C     JLTYP��1����ʾΪ����Դ
      JLTYP=1
C     �˴���һ���ж���Ϊ��������Դλ�ú�����ɵ���Դ�������
      IF(t.GE.1)then
C     ˫������Դǰ��������
      IF(x.GE.(x0+dx)) THEN
        FLUX(1)=heat1*shape1
      ELSE
        FLUX(1)=heat2*shape2
      ENDIF
      ELSE
        FLUX(1)=0
      ENDIF
        FLUX(2)=0
      RETURN
      END
