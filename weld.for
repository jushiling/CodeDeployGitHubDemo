      SUBROUTINE DFLUX(FLUX,SOL,KSTEP,KINC,TIME,NOEL,NPT,COORDS,
     1 JLTYP,TEMP,PRESS,SNAME)
C
      INCLUDE 'ABA_PARAM.INC'
C     FLUX(1)热通量；TIME(1)分析步时间；TIME(2)总时间;COORDS(3)三个坐标原点;
      DIMENSION FLUX(2), TIME(2), COORDS(3)
      CHARACTER*80 SNAME
      double precision t
      real::pi=3.141592654
C     wu,焊接电压
C     wi,焊接电流
C     effi,焊接效率系数
C     q,电弧有效热功率W
C     v,焊接速度mm/s
C     q,电弧有效热功率W
      wu=30
      wi=120
      effi=0.8
      v=1.25
C     mm单位制在此处需要乘以1000
      q=wu*wi*effi*1000
C     当前积分坐标
      x=COORDS(1)
      y=COORDS(2)
      z=COORDS(3)
C     从坐标x0,y0,z0开始
      x0=-1.25
      y0=0
      z0=0.3
C     热源移动方向和速度定义
      t=TIME(2)
      dx=v*t
      dy=0
      dz=0
C     a1,a2,b,c为双椭球的形状参数
      a1=0.9
      a2=1.8
      b=0.9
      c=1
C     f1为热源分配系数
      f1=1.0
      PI=3.1415926
      heat1=6.0*sqrt(3.0)*q/(a1*b*c*PI*sqrt(PI))*f1
      heat2=6.0*sqrt(3.0)*q/(a2*b*c*PI*sqrt(PI))*(2.0-f1)
      shape1=exp(-3.0*(x-x0-dx)**2/(a1)**2-3.0*(y-y0)**2/b**2
     $	-3.0*(z-z0)**2/c**2)
      shape2=exp(-3.0*(x-x0-dx)**2/(a2)**2-3.0*(y-y0)**2/b**2
     $	-3.0*(z-z0)**2/c**2)
C     JLTYP＝1，表示为体热源
      JLTYP=1
C     此处第一个判断是为了消除热源位置后移造成的热源过早出现
      IF(t.GE.1)then
C     双椭球热源前后分配语句
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
