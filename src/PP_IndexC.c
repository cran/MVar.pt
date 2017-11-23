#include <R.h>     // Bibliotecas para integrar com as funcoes matematicas do R
#include <Rmath.h> // Bibliotecas para integrar com as funcoes matematicas do R
#include <math.h>  // Biblioteca matematica pardrao do C
#include <stdio.h>

#ifndef Number_PI
#define Number_PI 3.141592653589793238462643383279502884197169399375105820974944592307816406 /* Numero Pi */
#endif

#ifndef Number_Euler
#define Number_Euler 2.718281828459045235360287471353	/* numero de Euler */
#endif

#define Length(Vet) (sizeof(Vet)/sizeof((Vet)[0])) /* length vector */


void NaturalHermite(int *row, int *col, double Data[*col][*row], double *Index);
void Hermite(int *row, int *col, double Data[*col][*row], double *Index);
void LaguerreFourier(int *row, int *col, double Data[*col][*row], double *Index);
void Legendre(int *row, int *col, double Data[*col][*row], double *Index);
void Entropy(int *row, int *col, double Data[*col][*row], double *Index);
void chi(int *row, int *col, double VecProj[2][*col], double *ck, double Data[*col][*row], double *Index);
void Holes(int *row, int *col, double Data[*col][*row], double *Index);
void Kurtosi(int *row, int *col, double Data[*col][*row], double *Index);
void Moment(int *row, int *col, double Data[*col][*row], double *Index);
void FriedmanTukey(int *row, int *col, double Data[*col][*row], double *Index);
void IndexPCA(int *row, double *Data, double *Index);

double PolyLegendre(int i, double x); /* funcao usada no indice de Legendre par calcular os polinomios de Legendre */
double PolyLaguerre(int i, double x); /* funcao usada no indice de LaguerreFourier par calcular os polinomios de Laguerre */
double PolyHermite(int i, double x);  /*  funcao usada no indice de Hermite par calcular os polinomios de Hermite */
double PolyNaturalHermite(int i, double x); /* funcao usada no indice de Natural Hermite par calcular os polinomios de Hermite Natural  */
unsigned long int Factorial(int n); /* funcao usada no indice de Hermite e Natural Hermite */
double is_par(int num); /* funcao usada no indice Natural Hermite */


/* INICIO - Indices */
void NaturalHermite(int *row, int *col, double Data[*col][*row], double *Index) {
	int i, j, k, jt, n = *row;
    unsigned long int fact;
	double ya[n], rho, sum, sum1 = 0.0;
    double sumx = 0.0, sumy = 0.0;
    double meanx = 0.0, meany = 0.0, sdx = 0.0, sdy = 0.0;
    double sumsquarex = 0.0, sumsquarey = 0.0, sumsquarexy = 0.0; 
    double Term1, Term2, Term3, Term4;
    
    *Index = 0.0;
    
    for(i = 0; i < n; i++) {
       sumx += Data[0][i];
       sumy += Data[1][i];
	}
	
	meanx = sumx / n; /* media de x */
    meany = sumy / n; /* media de y */
    
    for(i = 0; i < n; i++) {
       sumsquarex  += pow((Data[0][i] - meanx), 2.0);
       sumsquarey  += pow((Data[1][i] - meany), 2.0);
       sumsquarexy += (Data[0][i] - meanx) *  (Data[1][i] - meany);
    }
    
    sdx = sqrt(sumsquarex / (n - 1)); /* desvio padrao de x */
    sdy = sqrt(sumsquarey / (n - 1)); /* desvio padrao de y */
	 
    rho = (sumsquarexy / n) / (sqrt(sumsquarex / n) * sqrt(sumsquarey / n)); /* correlacao entre x e y */

    /* calculo da funcao normal bivariada */
	Term1 = 1.0 / (2.0 * Number_PI * sdx * sdy * sqrt(1 - pow(rho, 2.0)));
    Term2 = -1.0 / (2.0 * (1.0 - pow(rho, 2.0)));
    for(i = 0; i < n; i++) {
       Term3 = ((Data[0][i] - meanx) / sdx);
       Term4 = ((Data[1][i] - meany) / sdy);
       ya[i] = Term1 * exp(Term2 * (pow(Term3, 2.0) + pow(Term4, 2.0) - 2.0 * rho * Term3 * Term4)); /* funcao normal bivariada */ 
    }

    jt = 11; /* numero de termos de polinomios de Natural Hermite */

    for(j = 0; j < jt; j++) {
    	
       sum = 0.0;
       fact = Factorial(j);
       for(k = 0; k < (jt-j); k++) {
       	
       	  sum1 = 0.0;
       	  for(i = 0; i < n; i++) {
        	 sum1 += 1.0 / sqrt(fact * Factorial(k)) *
                     PolyNaturalHermite(j, Data[0][i]) *  
			 		 PolyNaturalHermite(k, Data[1][i]) *
                     ya[i] - is_par(j) * is_par(k);
       	  }      
          sum += pow(1.0 / n * sum1, 2.0);
       }
 
       *Index += sum;
    }
}

double is_par(int num) { /* funcao usada no indice Natural Hermite */
	
	double value = 0.0; 
	
	if(num % 2 == 0) { /* se for numero par */
      value = pow(-1.0, (double) num) * sqrt(Factorial(2 * num)) / (sqrt(Number_PI) * 
	          Factorial(num) * pow(2.0, (2.0 * (double) num + 1.0)));
    }
 
    return value;
}

double PolyNaturalHermite(int i, double x) { /* funcao usada no indice de Natural Hermite par calcular os polinomios de Hermite Natural  */
    
	double vlr = 0.0;
    
	switch(i + 1) {
	   case 1: vlr = 1.0;
	   break;
	   case 2: vlr = x;
	   break;
	   case 3: vlr = pow(x, 2.0) - 1.0;
	   break;
	   case 4: vlr = pow(x, 3.0) - 3.0 * x;
	   break;
	   case 5: vlr = pow(x, 4.0) - 6.0 * pow(x, 2.0) + 3.0;
	   break;
	   case 6: vlr = pow(x, 5.0) - 10.0 * pow(x, 3.0) + 15.0 * x;
	   break;
	   case 7: vlr = pow(x, 6.0) - 15.0 * pow(x, 4.0) + 45.0 * pow(x, 2.0) - 15.0;
	   break;
	   case 8: vlr = pow(x, 7.0) - 21.0 * pow(x, 5.0) + 105.0 * pow(x, 3.0) - 105.0 * x;
	   break;
	   case 9: vlr = pow(x, 8.0) - 28.0 * pow(x, 6.0) + 210.0 * pow(x, 4.0) - 420.0 * pow(x, 2.0) + 105.0;
	   break;
	   case 10: vlr = pow(x, 9.0) - 36.0 * pow(x, 7.0) + 378.0 * pow(x, 5.0) - 1260.0 * pow(x, 3.0) + 945.0 * x;
	   break;
	   case 11: vlr = pow(x, 10.0) - 45.0 * pow(x, 8.0) + 630.0 * pow(x, 6.0) - 3150.0 * pow(x, 4.0) + 4725.0 * pow(x, 2.0) - 945.0;
	   break;
    }
    return vlr;
}


void Hermite(int *row, int *col, double Data[*col][*row], double *Index) {
	int i, j, k, jt, n = *row;
    unsigned long int fact;
	double ya[n], yb[n], sum, cte, sum1 = 0.0, sum2 = 0.0;;
    double sumx = 0.0, sumy = 0.0, sum3 = 0.0, sum4 = 0.0;
    double meanx = 0.0, meany = 0.0, sdx = 0.0, sdy = 0.0;
    double sumsquarex = 0.0, sumsquarey = 0.0; 
    double Term1 = 0.0, Term2 = 0.0;

    for(i = 0; i < n; i++) {
       sumx += Data[0][i];
       sumy += Data[1][i];
	}
	
	meanx = sumx / n; /* media de x */
    meany = sumy / n; /* media de y */
    
    for(i = 0; i < n; i++) {
       sumsquarex  += pow((Data[0][i] - meanx), 2);
       sumsquarey  += pow((Data[1][i] - meany), 2);
    }
    
    sdx = sqrt(sumsquarex / (n - 1)); /* desvio padrao de x */
    sdy = sqrt(sumsquarey / (n - 1)); /* desvio padrao de y */

    /* calculo da funcao normal */
    cte = sqrt(2.0 * Number_PI);
    for(i = 0; i < n; i++) {
       ya[i] = 1.0 / (sdx * cte) * exp(-0.5 * pow(Data[0][i] - meanx, 2.0) / pow(sdx, 2.0)); /* funcao normal */
       yb[i] = 1.0 / (sdy * cte) * exp(-0.5 * pow(Data[1][i] - meany, 2.0) / pow(sdy, 2.0)); /* funcao normal */

       sum3 += ya[i];
       sum4 += yb[i];
    }
    
    jt = 8; /* numero de termos de polinomios de Hermite */
    for(j = 0; j < jt; j++) {

       fact = Factorial(j);
       
       sum = 0.0;
       for(k = 0; k < (jt-j); k++) {
       	
       	  sum1 = 0.0;
       	  sum2 = 0.0;
       	  for(i = 0; i < n; i++) {
  	  	    sum1 += PolyHermite(j, Data[0][i]) * ya[i];
		    sum2 += PolyHermite(k, Data[1][i]) * yb[i];
       	  } 
          sum += pow(1.0/2.0, (j + k)) / (fact * Factorial(k)) * pow(1.0 / n * sum1, 2.0) * pow(1.0 / n * sum2, 2.0);
       }
       Term1 += sum;
    }

    Term2 = - 1.0 / pow(n, 2.0) * sum3 * sum4 + 1.0 / (4 * Number_PI);

	*Index = Term1 + Term2;
}

unsigned long int Factorial(int n) { /* funcao usada no indice de Hermite e Natural Hermite */
	
    int t;
	unsigned long int answer = 1.0;
    
	for(t = 1; t <= n; t++) { 
	    answer *= t;
	}
	
	return answer;
}
	   
double PolyHermite(int i, double x) { /*  funcao usada no indice de Hermite par calcular os polinomios de Hermite */
    
	double vlr = 0.0;
    
	switch(i + 1) {
	   case 1: vlr = 1.0;
	   break;
	   case 2: vlr = 2.0 * x;
	   break;
	   case 3: vlr = 4.0 * pow(x, 2.0) - 2.0;
	   break;
	   case 4: vlr = 8.0 * pow(x, 3.0) - 12.0 * x;
	   break;
	   case 5: vlr = 16.0 * pow(x, 4.0) - 48.0 * pow(x, 2.0) + 12.0;
	   break;
	   case 6: vlr = 32.0 * pow(x, 5.0) - 160.0 * pow(x, 3.0) + 120.0 * x;
	   break;
	   case 7: vlr = 64.0 * pow(x, 6.0) - 480.0 * pow(x, 4.0) + 720.0 * pow(x, 2.0) - 120.0;
	   break;
	   case 8: vlr = 128.0 * pow(x, 7.0) - 1344.0 * pow(x, 5.0) + 3360.0 * pow(x, 3.0) - 1680.0 * x;
	   break;
   }
    return vlr;
}


void LaguerreFourier(int *row, int *col, double Data[*col][*row], double *Index) {
 
    int i, j, k, n = *row;
	int jt = 8; /* numero de termos de polinomios de Laguerre */
    double rho[n], phi[n], sum1 = 0.0, sum2 = 0.0, sum;
    double Term1 = 0.0, Term2 = 0.0, Term3 = 0.0;
        
    for(i = 0; i < n; i++) {
    	rho[i] = pow(Data[0][i], 2.0) + pow(Data[1][i], 2.0);
    	phi[i] = atan(Data[1][i] / Data[0][i]);
    	
    	sum1 += exp( - rho[i] / 2.0) * cos(1.0 * phi[i]);
        sum2 += exp( - rho[i] / 2.0) * sin(1.0 * phi[i]);
	}
    sum = pow(1.0 / n * sum1, 2.0) + pow(1.0 / n * sum2, 2.0);
			 
    for(j = 1; j < jt; j++) {
	   
       for(k = 1; k < jt; k++) {
 	
          sum1 = 0.0;
          sum2 = 0.0; 
       	  for(i = 0; i < n; i++) {				     
	         sum1 += PolyLaguerre(j, rho[i]) * exp( - rho[i] / 2.0) * cos(k * phi[i]);
             sum2 += PolyLaguerre(j, rho[i]) * exp( - rho[i] / 2.0) * sin(k * phi[i]);     
          }
          sum += pow(1.0 / n * sum1, 2.0) + pow(1.0 / n * sum2, 2.0);
       }
    }
    Term1 = 1.0 / Number_PI * sum;    
       
    sum1 = 0.0;
	for(i = 0; i < n; i++) {
       	sum1 += exp( - rho[i] / 2.0);
    }
    sum2 = sum1;
    sum = pow(1.0 / n * sum1, 2.0);
    
    for(j = 1; j < jt; j++) {
       sum1 = 0.0;
       for(i = 0; i < n; i++) {
          sum1 += PolyLaguerre(j, rho[i]) * exp( - rho[i] / 2.0);
       }
       
       sum += pow(1.0 / n * sum1, 2.0);
    }
    
    Term2 = 1.0 / (2.0 * Number_PI) * sum;
      
    Term3 = - 1.0 / (2.0 * Number_PI * n) * sum2 + 1.0 / (8.0 * Number_PI);

    *Index = Term1 + Term2 + Term3;
}

double PolyLaguerre(int i, double x) { /* funcao usada no indice de LaguerreFourier par calcular os polinomios de Laguerre */
    
	double vlr = 0.0;
    
	switch(i + 1) {
//	   case 1: vlr = 1.0;
//	   break;
	   case 2: vlr = - x + 1.0;
	   break;
	   case 3: vlr = pow(x, 2.0) - 4.0 * x + 2.0;
	   break;
	   case 4: vlr = - pow(x, 3.0) + 9.0 * pow(x, 2.0) - 18.0 * x + 6.0;
	   break;
	   case 5: vlr = pow(x, 4.0) - 16.0 * pow(x, 3.0) + 72.0 * pow(x, 2.0) - 96.0 * x + 24.0;
	   break;
	   case 6: vlr = - pow(x, 5.0) + 25.0 * pow(x, 4.0) - 200.0 * pow(x, 3.0) + 600.0 * pow(x, 2.0) - 600.0 * x + 120.0;
	   break;
	   case 7: vlr = pow(x, 6.0) - 36.0 * pow(x, 5.0) + 450.0 * pow(x, 4.0) - 2400.0 * pow(x, 3.0) + 5400.0 * pow(x, 2.0) - 4320.0 * x + 720.0;
	   break;
	   case 8: vlr = - pow(x, 7.0) + 49.0 * pow(x, 6.0) - 882.0 * pow(x, 5.0) + 7350.0 * pow(x, 4.0) - 29400.0 * pow(x, 3.0) + 52920.0 * pow(x, 2.0) - 35280.0 * x + 5040.0;
	   break;
	   case 9: vlr = pow(x, 8.0) - 64.0 * pow(x, 7.0) + 1568.0 * pow(x, 6.0) - 18816.0 * pow(x, 5.0) + 117600.0 * pow(x, 4.0) - 376320.0 * pow(x, 3.0) + 564480.0 * pow(x, 2.0) - 322560.0 * x + 40320.0; 
	   break;
   }
	return vlr;
}


void Legendre(int *row, int *col, double Data[*col][*row], double *Index) {
 
    int i, j, k, n = *row;
    int jt = 8; /* numero de termos de polinomios de Legendre */
    double ya[n], yb[n], cte;
    double sumx = 0.0, sumy = 0.0, sum1 = 0.0, sum2 = 0.0;
    double meanx = 0.0, meany = 0.0, sdx = 0.0, sdy = 0.0;
    double sumsquarex = 0.0, sumsquarey = 0.0; 
    double Term1 = 0.0, Term2 = 0.0, Term3 = 0.0;

    for(i = 0; i < n; i++) {
       sumx += Data[0][i];
       sumy += Data[1][i];
	}
	
	meanx = sumx / n; /* media de x */
    meany = sumy / n; /* media de y */
    
    for(i = 0; i < n; i++) {
       sumsquarex  += pow((Data[0][i] - meanx), 2);
       sumsquarey  += pow((Data[1][i] - meany), 2);
    }
    
    sdx = sqrt(sumsquarex / (n - 1)); /* desvio padrao de x */
    sdy = sqrt(sumsquarey / (n - 1)); /* desvio padrao de y */

    /* calculo da funcao normal */
    cte = sqrt(2.0 * Number_PI);
    for(i = 0; i < n; i++) {
       ya[i] = 2.0 * 1.0 / (sdx * cte) * exp(-0.5 * pow(Data[0][i] - meanx, 2.0) / pow(sdx, 2.0)) - 1.0; /* funcao normal */
       yb[i] = 2.0 * 1.0 / (sdy * cte) * exp(-0.5 * pow(Data[1][i] - meany, 2.0) / pow(sdy, 2.0)) - 1.0; /* funcao normal */
    }

    cte =  pow(1.0/n, 2.0);
    for(j = 1; j < jt; j++) {
       
	   sum1 = 0.0;
	   sum2 = 0.0;
       for(i = 0; i < n; i++) {
	      sum1 += PolyLegendre(j, ya[i]);	     
	      sum2 += PolyLegendre(j, yb[i]);	     
	   }
	   
	   Term1 += (2.0 * j + 1) * cte * pow(sum1, 2.0); 
       Term2 += (2.0 * j + 1) * cte * pow(sum2, 2.0); 
       
       for(k = 1; k < (jt-j+1); k++) {
          sum1= 0.0;
          for(i = 0; i < n; i++) {
	         sum1 += PolyLegendre(j, ya[i]) * PolyLegendre(k, yb[i]);    
	      }
 
       Term3 += (2.0 * j + 1) * (2.0 * k + 1) * pow((1.0 / n * sum1), 2.0); //sum(PLeg(j,ya) * PLeg(k,yb)))^2
      
	  }
      
    }
  
    *Index = 1.0 / 4.0 * (Term1 + Term2 + Term3);
}

double PolyLegendre(int i, double x) { /* funcao usada no indice de Legendre par calcular os polinomios de Legendre */
    
	double vlr = 0.0;
    
	switch(i + 1) {
//	   case 1: vlr = 1.0;
//	   break;
	   case 2: vlr = x;
	   break;
	   case 3: vlr = 1.0/2.0 * pow(x, 2.0) - 1.0/2.0;
	   break;
	   case 4: vlr = 5.0/2.0 * pow(x, 3.0) - 3.0/2.0 * x;
	   break;
	   case 5: vlr = 35.0/8.0 * pow(x, 3.0) - 15.0/4.0 * pow(x, 2.0) + 3.0/8.0;
	   break;
	   case 6: vlr = 63.0/8.0 * pow(x, 5.0) - 35.0/4.0 * pow(x, 3.0) + 15.0/8.0 * x;
	   break;
	   case 7: vlr = 231.0/16.0 * pow(x, 6.0) - 315.0/16.0 * pow(x, 4.0) + 105.0/16.0 * pow(x, 2.0) - 5.0/16.0;
	   break;
	   case 8: vlr = 429.0/16.0 * pow(x, 7.0) - 693.0/16.0 * pow(x, 5.0) + 315.0/16.0 * pow(x, 3.0) - 35.0/16.0 * x;
	   break;
	}

    return vlr;
}


void Entropy(int *row, int *col, double Data[*col][*row], double *Index) {
 
    int i, j, n = *row;
    double ha = 0.0, hb = 0.0, sumx = 0.0, sumy = 0.0, sum = 0.0;
    double meanx = 0.0, meany = 0.0, sdx = 0.0, sdy = 0.0, rho, Sa = 0.0;
    double sumsquarex = 0.0, sumsquarey = 0.0, sumsquarexy = 0.0;
    double cte = 1.06 * pow(1.0/n, 0.2);
    double Term1, Term2, Term3, Term4; /* termos da normal bivariada */

    for(i = 0; i < n; i++) {
       sumx += Data[0][i];
       sumy += Data[1][i];
	}
	
	meanx = sumx / n; /* media de x */
    meany = sumy / n; /* media de y */
    
    for(i = 0; i < n; i++) {
       sumsquarex  += pow((Data[0][i] - meanx), 2.0);
       sumsquarey  += pow((Data[1][i] - meany), 2.0);
       sumsquarexy += (Data[0][i] - meanx) *  (Data[1][i] - meany);
    }
    
    sdx = sqrt(sumsquarex / (n - 1)); /* desvio padrao de x */
    sdy = sqrt(sumsquarey / (n - 1)); /* desvio padrao de y */
	 
    ha = cte * sdx;
    hb = cte * sdy;
    
    rho = (sumsquarexy / n) / (sqrt(sumsquarex / n) * sqrt(sumsquarey / n)); /* correlacao entre x e y */

    cte   = 1.0 / (n * ha * hb);  
    
    Term1 = 1.0 / (2.0 * Number_PI * sdx * sdy * sqrt(1.0 - pow(rho, 2.0)));
    Term2 = -1.0 / (2.0 * (1 - pow(rho, 2.0)));

    for(i = 0; i < n; i++) {
    	sum = 0.0;
    	for(j = 0; j < n; j++) {
           Term3 = ((((Data[0][i] - Data[0][j]) / ha) - meanx) / sdx);
           Term4 = ((((Data[1][i] - Data[1][j]) / hb) - meany) / sdy);
           sum += Term1 * exp(Term2 * (pow(Term3, 2.0) + pow(Term4, 2.0) - 2.0 * rho * Term3 * Term4)); /* funcao normal bivariada */
        }

        Sa += log(cte * sum);
    }
 
    *Index = Sa / n + log(2.0 * Number_PI * Number_Euler);
}


void chi(int *row, int *col, double VecProj[2][*col], double *ck, double Data[*col][*row], double *Index) {
 
    int i, j, k, m, ind, nr = 6, na = 9, n = *row;
    double ppi = 0.0, aj[*col], bj[*col], z[2][n], r[n];
    double th[n], rd[nr], eta[9], pk[48], angles[na];
    double delang = 45 * Number_PI / 180;
    double delr = sqrt(2 * log(6))/5;

	for(i = 0; i < 48; i++) {
        pk[i] = 0.0;
    }
      
    for (i = 0; i < 9; i++) {
       eta[i] = Number_PI * i / 36;
	}
	  
    for (i = 0; i < na; i++) {
        angles[i] = i * delang;
	}
      
    for (i = 0; i < nr; i++) {
        rd[i] = i * delr;
	}

    for(m = 0; m < 9; m++) {
    	
        /* rotaciona o plano */
        for(i = 0; i < *col; i++) {
           aj[i] = VecProj[0][i] * cos(eta[m]) - VecProj[1][i] * sin(eta[m]);
           bj[i] = VecProj[0][i] * sin(eta[m]) + VecProj[1][i] * cos(eta[m]);
		}
		
        /* projeta os dados */
        for(i = 0; i < n; i++) {
            z[0][i] = 0.0;
            z[1][i] = 0.0;
        }
        
        for(i = 0; i < n; i++) {
        	for(j = 0; j < *col; j++) {
               z[0][i] += Data[j][i] * aj[j];
               z[1][i] += Data[j][i] * bj[j];
            }
        }
       
        /* Converte coordenadas cardesianas em polares */
        for(i = 0; i < n; i++) {
           r[i]  = sqrt(pow(z[0][i], 2.0) + pow(z[1][i], 2.0));
           th[i] = atan2(z[1][i], z[0][i]);
        }

        /* Encontrar todos os angulos que sao negativos */
        for(i = 0; i < n; i++) {
           if (th[i] < 0) {
		      th[i] += 2 * Number_PI;
	       }
        }

        /* find points in each box */
        for(i = 0; i < (nr - 1); i++) {	// loop over each ring
          for(k = 0; k < (na - 1); k++) { // loop over each wedge
              ind = 0;
              for(j = 0; j < n; j++) {
              	if(r[j] > rd[i] && r[j] < rd[i+1] && th[j] > angles[k] && th[j] < angles[k+1]) {
              		ind += 1;
				}
			  }

			  pk[i*8+k] = pow((double) ind/n - ck[i*8+k], 2.0) / ck[i*8+k];

         }
        }

        /* find the number in the outer line of boxes */
        for(k = 0; k < (na - 1); k++) {
        	ind = 0;
            for(j = 0; j < n; j++) {
              	if(r[j] > rd[nr-1] && th[j] > angles[k] && th[j] < angles[k+1]) {
              		ind += 1;
				}
			}

          pk[40+k] = pow((double) ind / n - (1.0/48), 2.0) / (1.0/48.0);
        }

        for(i = 0; i < 48; i++) {
        	ppi += pk[i];
		}

    }

    *Index = ppi / 9.0;
}


void Holes(int *row, int *col, double Data[*col][*row], double *Index) {
 
    int i, j, n = *row, d = *col;
    
    double slin[*row]; /* soma das linhas */
	    
    /* Soma das linhas com cada elemento ao quadrado */
    for (i = 0; i < *row; i++ ) {
    	slin[i] = 0.0;
        for (j = 0; j < *col; j++) {	
            slin[i] += pow(Data[j][i], 2.0);
        } 
    }    
    
    for (i = 0; i<n; i++) {
    	*Index += exp(-0.5 * slin[i]);
    }
   
    *Index = (1.0 - 1.0/n * (*Index)) / (1.0 - exp( - d/2.0));
}


void Kurtosi(int *row, int *col, double Data[*col][*row], double *Index) {
 
    int i, j, n = (double) *row;
    
    double M4 = 0.0, M2 = 0.0, mean = 0.0;

    /* Calculo da media amostral */
    for (i = 0; i < *row; i++ ) {
        for (j = 0; j < *col; j++) {
		    mean += Data[j][i];	
        } 
    }    
    mean = mean / (n * (*col)); /* valor da media amostral */
    
    /* Centraliza os dados na media amostral */
    for (i = 0; i < *row; i++ ) {
        for (j = 0; j < *col; j++) {
		    Data[j][i] -= mean;	
        } 
    }    

    /* Encontra M4 e M2 - momentos 4 e 2 */
    for (i = 0; i < *row; i++ ) {
        for (j = 0; j < *col; j++) {
		    M4 += pow(Data[j][i], 4.0);	
            M2 += pow(Data[j][i], 2.0);
        } 
    }    
   
    *Index = pow((n - 1.0), 2.0) * M4 / ( n * pow(M2, 2.0));
}


void Moment(int *row, int *col, double Data[*col][*row], double *Index) {
    int i, n = *row;
    
    double *Zalpha = Data[0];
    double *Zbeta  = Data[1];
    
	double Za2[*row], Zb2[*row];
	double Za3[*row], Zb3[*row];
	double Za4[*row], Zb4[*row];
   
    double c1, c2, c3, c4;

    double k30 = 0.0, k03 = 0.0, k31 = 0.0;
	double k13 = 0.0, k04 = 0.0, k40 = 0.0;
	double k22 = 0.0, k21 = 0.0, k12 = 0.0;
    	
    double t1, t2;
    	
    /* Encontra as potencias necessarias */
    for (i = 0; i < *row; i++ ) {
    	Za2[i] = pow(Zalpha[i], 2.0);
        Zb2[i] = pow(Zbeta[i], 2.0);
        
        Za3[i] = pow(Zalpha[i], 3.0);
        Zb3[i] = pow(Zbeta[i], 3.0);

		Za4[i] = pow(Zalpha[i], 4.0);
        Zb4[i] = pow(Zbeta[i], 4.0);
    }   
    
    /* Adquire todos os coeficientes */
    c1 = n / ((n - 1.0) * (n - 2.0));
    c2 = (n * (n + 1.0)) / ((n - 1.0) * (n - 2.0) * (n - 3.0));
    c3 = 3.0 * pow((n - 1.0), 3.0) / (n * (n + 1.0));
    c4 = pow((n - 1.0),3.0) / (n * (n + 1.0));

    /* Adquire todos os termos */
    for (i = 0; i < *row; i++) {
    	k30 += Za3[i];
    	k03 += Zb3[i];
    	k31 += (Za3[i] * Zbeta[i]);
    	k13 += (Zb3[i] * Zalpha[i]);
    	k04 += Zb4[i];
    	k40 += Za4[i];
    	k22 += (Za2[i] * Zb2[i]);
        k21 += (Za2[i] * Zbeta[i]);
        k12 += (Zb2[i] * Zalpha[i]);
	}
	k30 *= c1;
	k03 *= c1;
	k31 *= c2;
	k13 *= c2;
    k04 = (k04 - c3) * c2;
    k40 = (k40 - c3) * c2;
    k22 = (k22 - c4) * c2;
    k21 *= c1;
    k12 *= c1;

    /* Encontra os valores */
    t1 = pow(k30, 2.0) + 3.0 * pow(k21, 2.0) + 3.0 * pow(k12, 2.0) + pow(k03, 2.0);
    t2 = pow(k40, 2.0) + 4.0 * pow(k31, 2.0) + 6.0 * pow(k22, 2.0) + 4.0 * pow(k13, 2.0) + pow(k04, 2.0);

    *Index = (t1 + t2 / 4.0) / 12.0; 
}


void FriedmanTukey(int *row, int *col, double Data[*col][*row], double *Index) {
 
    int i, j, n = *row;
    
    double R = 2.29 * pow(1.0/n, 0.2);
    
    double rij, Farg, x;

    *Index = 0.0; /* acumula valor para o indice */

    for (i = 0; i < n; i++ ) {
        for (j = 0; j < n; j++ ) {
            rij = pow(Data[0][i] - Data[0][j], 2.0) + pow(Data[1][i] - Data[1][j], 2.0);
        
            Farg = pow(R, 2.0) - rij;
    
            x = Farg > 0 ? 1.0 : 0.0;
    
            *Index += pow(Farg, 3.0) * x * Farg;
        }
    }

}


void IndexPCA(int *row, double *Data, double *Index) {

    int i;
   
    *Index = 0.0; /* acumula valor para o indice */

    for (i = 0; i < *row; i++ ) {
        *Index += pow(Data[i], 2.0);   
    }
    
    *Index = *Index / *row;
}

/* FIM - Indices */



