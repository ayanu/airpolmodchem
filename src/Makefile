#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# User: Set here the F90 compiler and options
#       Pedefined compilers: INTEL, PGF, HPUX, LAHEY
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#COMPILER = G95
#COMPILER = LAHEY
#COMPILER = INTEL
#COMPILER = PGF
#COMPILER = HPUX
COMPILER = GFORTRAN

FC_G95     = g95
FOPT_G95   = -cpp -O -pg -fbounds-check -fimplicit-none  -Wall -ftrace=full

FC_LAHEY   = lf95
# More aggressive for production runs:
#FOPT_LAHEY = -Cpp --pca -O
# More checking for debugging:
FOPT_LAHEY = -Cpp --chk a,e,s,u --pca --ap -O0 -g --trap --trace --chkglobal

FC_INTEL   = ifort 
# More aggressive for production runs:
#FOPT_INTEL = -cpp -O -fp-model precise -pc80 -prec_div
# More checking for debugging:
FOPT_INTEL = -cpp -O0 -fp-model strict -implicitnone -ftrapuv \
              -debug all -check all -warn all

FC_PGF     = pgf90
# More aggressive for production runs:
FOPT_PGF   = -Mpreprocess -O -fast -pc 80 -Kieee
# More checking for debugging:
#FOPT_PGF   = -Mpreprocess -O0 -Mbounds -Mchkfpstk -Mchkptr -Mchkstk \
#             -Ktrap=fp -pc 80 -Kieee

FC_HPUX    = f90
FOPT_HPUX  = -O -u +Oall +check=on

FC_GFORTRAN     = gfortran
FOPT_GFORTRAN   = -cpp -O1 -fpic -shared

# define FULL_ALGEBRA for non-sparse integration
FC   = $(FC_$(COMPILER))
FOPT = $(FOPT_$(COMPILER)) # -DFULL_ALGEBRA

LIBS =
#LIBS = -llapack -lblas

# Command to create Matlab mex gateway routines 
# Note: use $(FC) as the mex Fortran compiler
MEX  = mex

GENSRC = CRI_5VOC_Precision.f90  \
	 CRI_5VOC_Parameters.f90     \
	 CRI_5VOC_Global.f90  

GENOBJ = CRI_5VOC_Precision.o    \
	 CRI_5VOC_Parameters.o       \
	 CRI_5VOC_Global.o     

FUNSRC = CRI_5VOC_Function.f90 
FUNOBJ = CRI_5VOC_Function.o 

JACSRC = CRI_5VOC_JacobianSP.f90  CRI_5VOC_Jacobian.f90
JACOBJ = CRI_5VOC_JacobianSP.o    CRI_5VOC_Jacobian.o

HESSRC = CRI_5VOC_HessianSP.f90   CRI_5VOC_Hessian.f90
HESOBJ = CRI_5VOC_HessianSP.o     CRI_5VOC_Hessian.o

STMSRC = CRI_5VOC_StoichiomSP.f90 CRI_5VOC_Stoichiom.f90 
STMOBJ = CRI_5VOC_StoichiomSP.o   CRI_5VOC_Stoichiom.o

UTLSRC = CRI_5VOC_Rates.f90 CRI_5VOC_Util.f90 CRI_5VOC_Monitor.f90 mcm_constants.f90
UTLOBJ = CRI_5VOC_Rates.o   CRI_5VOC_Util.o   CRI_5VOC_Monitor.o mcm_constants.o

LASRC  = CRI_5VOC_LinearAlgebra.f90 
LAOBJ  = CRI_5VOC_LinearAlgebra.o   

STOCHSRC = CRI_5VOC_Stochastic.f90 
STOCHOBJ = CRI_5VOC_Stochastic.o 

MAINSRC = CRI_5VOC_Main.f90   CRI_5VOC_Initialize.f90   CRI_5VOC_Integrator.f90 CRI_5VOC_Model.f90
MAINOBJ = CRI_5VOC_Main.o     CRI_5VOC_Initialize.o     CRI_5VOC_Integrator.o   CRI_5VOC_Model.o 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# User: modify the line below to include only the
#       objects needed by your application
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ALLOBJ = $(GENOBJ) $(FUNOBJ) $(JACOBJ) $(HESOBJ) $(STMOBJ) \
	 $(UTLOBJ) $(LAOBJ)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# User: modify the line below to include only the
#       executables needed by your application
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
all:    exe

exe:	$(ALLOBJ) $(MAINOBJ) 
	$(FC) $(FOPT) $(ALLOBJ) $(MAINOBJ) $(LIBS) -o libCRI_5VOC.so

stochastic:$(ALLOBJ) $(STOCHOBJ) $(MAINOBJ)
	$(FC) $(FOPT) $(ALLOBJ) $(STOCHOBJ) $(MAINOBJ) $(LIBS) \
	-o CRI_5VOC_stochastic.exe

mex:    $(ALLOBJ)
	$(MEX) FC#$(FC) -fortran -O CRI_5VOC_mex_Fun.f90     $(ALLOBJ)
	$(MEX) FC#$(FC) -fortran -O CRI_5VOC_mex_Jac_SP.f90  $(ALLOBJ)
	$(MEX) FC#$(FC) -fortran -O CRI_5VOC_mex_Hessian.f90 $(ALLOBJ)

clean:
	rm -f *.o *.mod libCRI_5VOC.so 

distclean:
	rm -f CRI_5VOC*.o CRI_5VOC*.mod \
	CRI_5VOC*.dat CRI_5VOC.exe CRI_5VOC.map \
	CRI_5VOC*.f90 CRI_5VOC_*.mexglx

CRI_5VOC_Precision.o: CRI_5VOC_Precision.f90 
	$(FC) $(FOPT) -c $<

CRI_5VOC_Parameters.o: CRI_5VOC_Parameters.f90 \
	            CRI_5VOC_Precision.o
	$(FC) $(FOPT) -c $<

CRI_5VOC_Monitor.o: CRI_5VOC_Monitor.f90 \
	             CRI_5VOC_Precision.o
	$(FC) $(FOPT) -c $<

CRI_5VOC_Global.o: CRI_5VOC_Global.f90 \
	            CRI_5VOC_Parameters.o CRI_5VOC_Precision.o
	$(FC) $(FOPT) -c $<

CRI_5VOC_Initialize.o: CRI_5VOC_Initialize.f90  $(GENOBJ) 
	$(FC) $(FOPT) -c $<

CRI_5VOC_Function.o: CRI_5VOC_Function.f90  $(GENOBJ) 
	$(FC) $(FOPT) -c $<

CRI_5VOC_Stochastic.o: CRI_5VOC_Stochastic.f90  $(GENOBJ) 
	$(FC) $(FOPT) -c $<

CRI_5VOC_JacobianSP.o: CRI_5VOC_JacobianSP.f90 $(GENOBJ)
	$(FC) $(FOPT) -c $<

CRI_5VOC_Jacobian.o: CRI_5VOC_Jacobian.f90  $(GENOBJ) CRI_5VOC_JacobianSP.o
	$(FC) $(FOPT) -c $<

CRI_5VOC_LinearAlgebra.o: CRI_5VOC_LinearAlgebra.f90 $(GENOBJ) CRI_5VOC_JacobianSP.o
	$(FC) $(FOPT) -c $<

CRI_5VOC_Rates.o: CRI_5VOC_Rates.f90  $(GENOBJ) mcm_constants.o
	$(FC) $(FOPT) -c $<

CRI_5VOC_HessianSP.o: CRI_5VOC_HessianSP.f90  $(GENOBJ)
	$(FC) $(FOPT) -c $<

CRI_5VOC_Hessian.o:  CRI_5VOC_Hessian.f90 $(GENOBJ) CRI_5VOC_HessianSP.o
	$(FC) $(FOPT) -c $<

CRI_5VOC_StoichiomSP.o: CRI_5VOC_StoichiomSP.f90 $(GENOBJ)
	$(FC) $(FOPT) -c $<

CRI_5VOC_Stoichiom.o: CRI_5VOC_Stoichiom.f90  $(GENOBJ) CRI_5VOC_StoichiomSP.o
	$(FC) $(FOPT) -c $<

CRI_5VOC_Util.o: CRI_5VOC_Util.f90  $(GENOBJ) CRI_5VOC_Monitor.o
	$(FC) $(FOPT) -c $<

CRI_5VOC_Main.o: CRI_5VOC_Main.f90  $(ALLOBJ) CRI_5VOC_Initialize.o CRI_5VOC_Model.o CRI_5VOC_Integrator.o
	$(FC) $(FOPT) -c $<

CRI_5VOC_Model.o: CRI_5VOC_Model.f90  $(ALLOBJ) CRI_5VOC_Integrator.o
	$(FC) $(FOPT) -c $<

CRI_5VOC_Integrator.o: CRI_5VOC_Integrator.f90  $(ALLOBJ)
	$(FC) $(FOPT) -c $<

mcm_constants.o: mcm_constants.f90  $(GENOBJ) 
	$(FC) $(FOPT) -c $<
