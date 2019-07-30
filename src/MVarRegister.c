 /* Este arquivo he nessario para rodar os arquivos em C no R, alem a mudanda em NAMESPACE*/
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

//extern void NaturalHermite(int *row, int *col, double Data[*col][*row], double *Index);
//extern void Hermite(int *row, int *col, double Data[*col][*row], double *Index);
//extern void LaguerreFourier(int *row, int *col, double Data[*col][*row], double *Index);
//extern void Legendre(int *row, int *col, double Data[*col][*row], double *Index);
//extern void Entropy(int *row, int *col, double Data[*col][*row], double *Index);
//extern void chi(int *row, int *col, double VecProj[2][*col], double *ck, double Data[*col][*row], double *Index);
//extern void Holes(int *row, int *col, double Data[*col][*row], double *Index);
//extern void Kurtosi(int *row, int *col, double Data[*col][*row], double *Index);
//extern void Moment(int *row, int *col, double Data[*col][*row], double *Index);
//extern void FriedmanTukey(int *row, int *col, double Data[*col][*row], double *Index);
//extern void IndexPCA(int *row, double *Data, double *Index);
//extern void MF(int *row, int *col, double Data[*col][*row], double *Index);

//static const R_CMethodDef CEntries[] = {
//    {"NaturalHermite",  (DL_FUNC) &NaturalHermite,  4},
//    {"Hermite",         (DL_FUNC) &Hermite,         4},
//    {"LaguerreFourier", (DL_FUNC) &LaguerreFourier, 4},
//    {"Legendre",        (DL_FUNC) &Legendre,        4},
//    {"Entropy",         (DL_FUNC) &Entropy,         4},
//    {"chi",             (DL_FUNC) &chi,             6},
//    {"Holes",           (DL_FUNC) &Holes,           4},
//    {"Kurtosi",         (DL_FUNC) &Kurtosi,         4},
//    {"Moment",          (DL_FUNC) &Moment,          4},
//    {"FriedmanTukey",   (DL_FUNC) &FriedmanTukey,   4},
//    {"IndexPCA",        (DL_FUNC) &IndexPCA,        3},
//    {"MF",              (DL_FUNC) &MF,              4},
//    {NULL, NULL, 0}
//};

void R_init_MVar(DllInfo* info) {
  R_registerRoutines(info, NULL, NULL, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}

