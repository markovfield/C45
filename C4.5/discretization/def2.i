/*************************************************************************/
/*									 			  												 */
/*		Definitions used in interface.c and conversion.c		  				 */
/*              -------------------------------------------------		    */
/*									 			      										 */
/*************************************************************************/


#include <stdio.h>
#include <math.h>
 
#define	 Eof			EOF             /*char read on end of file*/
#define	 Nil			0               /*null pointer*/
#define	 false		0
#define	 true			1 
#define	 None		-1
#define	 Epsilon               1E-3
 
#define	 Max(a,b)               ((a)>(b) ? a : b)
#define	 Min(a,b)               ((a)<(b) ? a : b)
#define	 Round(x)		((int) (x+0.5))
#define	 Log2			0.69314718055994530942
#define	 Log(x)			((x) <= 0 ? 0.0 : log(x) / Log2)

#define	 ForEach(v,f,l)		for(v=f ; v<=l ; ++v)
 
