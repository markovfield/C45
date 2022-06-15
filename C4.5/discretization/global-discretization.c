/**********************************************************************/
/*									 		*/
/* 	THIS IS GLOBAL DISCRETIZATION PROGRAM CREATED 		 */
/*             BY LIPING LIU IN MARCH OF 1993				 	 */
/*	 ILLEGAL COPY IS PROHIBITED BY ITS COPYRIGHT		*/
/*	-----------------------------------------------			 	 */
/*	Student ID#: 536529						 		 */
/*									 		*/
/*	Instruction: cc -o interface interface.c and                     		 */
/*	use interface and follow the menu.					 */
/*									 		*/
/**********************************************************************/
 
 
#include "def2.i"
#include "type2.i"
#include "ext2.i"
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
 
 
#define  Space(s)	(s == ' ' || s == '\n' || s == '\t')
         char Delimiter;		/* string reading stop sign */
	 short MaxAtt;			/* number of attributes */
	 float Aval[1500][150];		/* attribute value matrix */
	 String *AttName,*DDval; 	/* attribute names and class names */
	 short MaxItem=0;		/* number of examples */
	 int *record;			/* cutpoint numbers of each attribute */
	 float **cutpoint;		/* cutpoint matrix */
	 Boolean *Mark;			/* mark of chosen attributes */
	 Boolean MarkD;			/* true = decision is discretized */
	 String **Acurrent;		/* discretized table */
	 String *object;		/* reference vector of test */
	 char criterion[20];		/* test criterion */
	 char outfile[50];		/* file of discretized table */
	 char cutfile[50];		/* file of cutpoint information */
	 char filemerge[100];		/* file of merging information */
	 char outaftermerge[100];	/* file of output after merging */
	 String *Dval;			/* read in decision values */
	 char DecisionName[100];	/* decision name */
	 float *Dcutpoint;		/* cutpoints of decision values */
	 int DcutNo;			/* number of cutpoints of decision V */
	 float *conDval;		/* read in continuous decision values */
 
 
/*************************************************************************/
/*									 			 */
/* Read a string from file f character by character into string s        	 */
/*  - Embedded periods are permitted. Set delimiter                      	 */
/*									 			 */
/*************************************************************************/
 
 
Boolean ReadName(f, s)
/*      ---------  */
    FILE *f;
    String s;
{
    register char *Sp=s;
    register int c;
    String CopyString();
 
/* find the first nonspace character */
 
 	
	while ((c=getc(f))=='[' || Space(c) );
 
    /*  Return false if no data to read  */
 
    if ( c == EOF )
    {
	Delimiter = EOF;
	return false;
    }
 
    /*  Read in characters up to the next delimiter  */
 
    while (c != ']' && c != ',' && !Space(c) && c != EOF )
	{ 
        *Sp++=c;
	c=getc(f);
	}
 
    if ( c==EOF) Delimiter = EOF;
    else if (c == ']') Delimiter = ']';
    else Delimiter = ',';
 
    return true;
}
 
/*******************************************************************/
/* 	CREATE DISCRETE VALUE NAMES FOR DECISION		  */
/*******************************************************************/
 
char *makestring(s,attatch)
/* 	  	*/
int attatch;
char *s;
{
	char *sp;
	sp = (char *) calloc(strlen(s)+4, sizeof(char));
	sprintf(sp, "%s%d",s,attatch);
	return sp;
}
 

/*******************************************************************/
/* 	CREATE DISCRETE ATTRIBUTE VALUE NAMES			  */
/*******************************************************************/
 
char *makedisattname(pre,post)
/* 	  	*/
int pre, post;
 
{
	char *sp;
	sp = (char *) calloc(8, sizeof(char));
	sprintf(sp, "%d%s%d",pre+1,"a",post);
	return sp;
}
 
/*************************************************************************/
/*									 			 */
/*	Allocate space then copy string into it				 	 */
/*												 */
/*************************************************************************/
 
String CopyString(x)
/*     -----------  */
    String x;
{
    char *s;
 
    s = (char *) calloc(strlen(x)+1, sizeof(char));
    strcpy(s, x);
    return s;
}
 
 
/*******************************************************************/
/*	Find acurrent according to Acurrent			   	 */
/*******************************************************************/
 
void Findacurrent(A,acurrent,n)
 
String **A, *acurrent;
int n;
 
{
  int i,j,k,No;
  int end, end1;
  No=0;
  acurrent[0] = makestring("R", 1);
  for (j=0; j <= MaxItem-1; j++)
  {
    for (k=0; k <= j-1; k++)
    {
	for (i=0; i <= n-1; i++)
	{
	  if (strcmp(A[k][i], A[j][i])) 
	  goto end;
 	}
      	acurrent[j] = CopyString(acurrent[k]);
	goto end1;
	end: ;
    }
    No++;
    acurrent[j] = makestring("R",No);
    end1: ;
  }
}
/******************************************************************/
/*											*/
/*	Arrange an array of continuous data in the order 		*/
/*      from large to small, determine the frequency of               */
/*      of each number, and return the number of different data */
/*								 			*/
/******************************************************************/
 
 
int NoArrange(arraydim,s,Arrange,VF)
/*			*/
float *s, *Arrange;
int *VF, arraydim;
 
{
  int k, i;
  float Av;
 
/* Find the maximun value */
 
  Arrange[0] = -1.0E10;
  for (i=0; i <= arraydim-1; i++)
  {
    Av= -1E10;
    Av = s[i];
    if (Av > Arrange[0])
    {
     Arrange[0] = Av;
    }
  }
  VF[0] = 0;
  for (i=0; i <= arraydim-1; i++)
  {
    Av = 1E10;
    Av = s[i];
    if (Av == Arrange[0])
    VF[0]++;
  }
  for (k=1; k <= arraydim-1; k++)
  {
   float temp = -1.0E10;
   for (i=0; i <= arraydim-1; i++)
   {
     Av = -1E10;
     Av = s[i];
     if (Av > temp && Av < Arrange[k-1])
     {
	temp = Av;
     }
   }
  if (temp > -1.0E10)
  {
    Arrange[k] = temp;
    VF[k] = 0;
    for (i=0; i <= arraydim-1; i++)
    {
	Av = -1E10;
	Av = s[i];
	if (Av == Arrange[k])
	{
	  VF[k]++;
	}
    }
  }
  else 
  {
     return k;
  }
}
  return arraydim;
}
 
/*********************************************************************/
/*	Rearrange Discrete Values						     */
/*********************************************************************/
 
int DArrange(arraydim1,sd,ArrangeD,VFD)
 
int arraydim1;
String *sd, *ArrangeD;
float *VFD;
 
{
    int i,j,k,No,label1;
    String sp;
    ArrangeD[0] = CopyString(sd[0]);
    VFD[0] = 0.0;
    for (i=0; i<= arraydim1 - 1; i++)
    {
	if ( !strcmp(ArrangeD[0], sd[i]))
	VFD[0] += 1.0;
    }
    k=1;
    No=0;
    do
    {
	sp = CopyString(sd[k]);
	j=0;
	while (j <= No)
	{
	  if ( !strcmp(sp, ArrangeD[j])) goto label1;
	  else j++;
	}
	No++;
	ArrangeD[No] = CopyString(sp);
	VFD[No] = 0;
	for (i=0; i <= arraydim1 - 1; i++)
	{
	  if (!strcmp(ArrangeD[No], sd[i]))
	  VFD[No] +=1.0;
	}
 
	goto label1;
 
	label1: k++;
   } while (k <= arraydim1 - 1);
  return No+1;
}
 
/*********************************************************************/
/* make discrete attribute values according to cutpoint values	     */
/*********************************************************************/
 
void makedisattvalue(att,cutdim,cut,s)
     
int att,cutdim;
float *cut;
String *s;
 
{
  int cutNo, *Tempvalfreq;
  float *cutarrange;
  int i,k;
  Tempvalfreq = (int *) calloc(cutdim, sizeof(int));
  cutarrange = (float *) calloc(cutdim, sizeof(float));
  cutNo = NoArrange(cutdim, cut, cutarrange, Tempvalfreq);
  free(Tempvalfreq);
  for (i=0; i <= MaxItem-1; i++)
  {
    if (Aval[i][att] > cutarrange[0])
    {
	s[i] = makedisattname(att,1);
    }
  }
  for (k=1; k <=cutNo - 1; k++)
  {
    for (i=0; i <= MaxItem-1; i++)
    {
	if (Aval[i][att] > cutarrange[k] && Aval[i][att] <= cutarrange[k-1])
	s[i] = makedisattname(att, k+1);
    }
  }
  for (i=0; i <=MaxItem-1; i++)
  {
    if (Aval[i][att] <= cutarrange[cutNo-1])
    s[i] = makedisattname(att, cutNo+1);
  }
}
 
/*********************************************************************/
/*	Test stopping condition						    */
/*********************************************************************/
 
Boolean Test(s1,s2)
 
String *s1, *s2;
 
{
  int attvalno, dvalno, i,j,k;
  float *VFA, *VFD;
  String *ArrangeA, *ArrangeD;
  float sum, Av, entropy;
  VFD = (float *) calloc(MaxItem, sizeof(float));
  ArrangeD = (String *) calloc(MaxItem, sizeof(String));
  dvalno = DArrange(MaxItem,s2,ArrangeD,VFD);
  free(VFD);
  VFA = (float *) calloc(MaxItem, sizeof(float));
  ArrangeA = (String *) calloc(MaxItem, sizeof(String));
  attvalno = DArrange(MaxItem, s1, ArrangeA,VFA);
  entropy = 0.0;
  for (i=0; i <= attvalno-1; i++)
  {
    Av=0.0;
    for (j=0; j <= dvalno-1; j++)
    {
	sum = 0.0;
        for (k=0; k <= MaxItem-1; k++)
        {
	  if (!strcmp(s1[k], ArrangeA[i]) && !strcmp(s2[k], ArrangeD[j]))
	  {
		sum += 1.0;
	  }
	}
	Av += sum * Log(sum);
    }
    entropy += (VFA[i]*Log(VFA[i])- Av)/MaxItem;
  }
  if (entropy <= Epsilon) return true;
  else return false;
}
 
 
/*********************************************************************/
/*	Find reference array for testing stopping condition		    */
/*********************************************************************/
 
void Findobj(void)
 
{
  int i,j;
  String *sp;
  float *cut;
  String **DA;
  sp = (String *) calloc(MaxItem, sizeof(String));
  cut = (float *) calloc(MaxItem, sizeof(float));
  DA = (String **) calloc(MaxItem, sizeof(String *));
  for (j=0; j <= MaxItem-1; j++)
  {
    DA[j] = (String *) calloc(MaxAtt+1, sizeof(String));
  }
  for (i=0; i <= MaxAtt; i++)
  {
    for (j=0; j < MaxItem-1; j++)
    {
	cut[j] = Aval[j][i];
    }
    makedisattvalue(i,MaxItem,cut,sp);
    for (j=0; j <= MaxItem-1; j++)
    {
	DA[j][i] = CopyString(sp[j]);
    }
  }
  object = (String *) calloc(MaxItem, sizeof(String));
  Findacurrent(DA,object, MaxAtt+1);
  if (Test(object,DDval))
  {
    free(object);
    object = (String *) calloc(MaxItem, sizeof(String));
    for (j=0; j <=MaxItem-1; j++)
    {
	object[j] = CopyString(DDval[j]);
    }
  }
}
/***********************************************************************/
/*	Find information value						        */
/***********************************************************************/
 
float infovalue(s1,s2,n)
 
String *s1,*s2;
int n;
 
{
  int attvalno, dvalno, i,j,k;
  float *VFA, *VFD;
  String *ArrangeA, *ArrangeD;
  float sum, Av,Ainfo, Dinfo,info, entropy;
  float typec = n;
  VFA = (float *) calloc(n, sizeof(float));
  VFD = (float *) calloc(n, sizeof(float));
  ArrangeA = (String *) calloc(n, sizeof(String));
  ArrangeD = (String *) calloc(n, sizeof(String));
  attvalno = DArrange(n, s1, ArrangeA,VFA);
  dvalno = DArrange(n,s2,ArrangeD,VFD);
  Dinfo = 0.0;
  for (j=0; j <= dvalno-1; j++)
  {
    Dinfo += VFD[j]*Log(VFD[j]);
  }
  Dinfo = Log(typec) - Dinfo/n;
  Ainfo = 0.0;
  entropy = 0.0;
  for (i=0; i <= attvalno-1; i++)
  {
    Av=0.0;
    Ainfo += VFA[i] * Log(VFA[i]);
    for (j=0; j <= dvalno-1; j++)
    {
	sum = 0.0;
        for (k=0; k <= n-1; k++)
        {
	  if (!strcmp(s1[k], ArrangeA[i]) && !strcmp(s2[k], ArrangeD[j]))
	  {
		sum += 1.0;
	  }
	}
	Av += sum * Log(sum);
    }
    entropy += (VFA[i]*Log(VFA[i])- Av)/n;
  }
  Ainfo = Log(typec) - Ainfo/n;
  if ( !strcmp(criterion, "GainRatio")) 
  {
  info = (Ainfo <= Epsilon) ? 0.0 : (Dinfo-entropy)/Ainfo;
  return info;
  }
  else if ( !strcmp(criterion, "InfoGain"))
  {
  return Dinfo - entropy;
  }
}
 
/******************************************************************/
/*	Find the best attribute for a subtable			 	*/
/******************************************************************/
int Findatt(Asub, s,n)
 
int n;
float **Asub;
String *s;
 
{
  String *sp;
  int i,j,att;
  float maxinfo, info;
  char *stemp;
  sp = (String *) calloc(n, sizeof(String));
  maxinfo = 0.0;
  for (i=0; i <= MaxAtt; i++)
  {
    for (j=0; j <= n-1; j++)
    {
        stemp = (char *) calloc(100, sizeof(char));
	sprintf(stemp,"%4.2f", Asub[j][i]);
	sp[j] = stemp;
    }
    info = infovalue(sp, s, n);
    if (info > maxinfo)
    {
	maxinfo = info;
	att = i;
    }
  }
  return att;
}
/******************************************************************/
/*	Find the best cut point						*/
/******************************************************************/
 
float Findcut(s1,s2,n)
 
float *s1;
String *s2;
int n;
 
{
  int attvalno, *Tempvalfreq, k,low, high;
  float *ArrangeA, info, Maxinfo, cut;
  String *sp;
  int i;
  float Av;
  ArrangeA = (float *) calloc(n,sizeof(float));
  Tempvalfreq = (int *) calloc(n, sizeof(int));
  attvalno = NoArrange(n,s1,ArrangeA,Tempvalfreq);
  low = Max(1,attvalno/20);
  sp = (String *) calloc(n,sizeof(String));
  Maxinfo = 0.0;
  for (k=attvalno-1; k >=1; k -= low)
  {
    Av = ArrangeA[k];
    for (i=0; i <= n-1; i++)
    {
      if (s1[i] <= Av) 
        sp[i] = "a2";
      else
    	sp[i] = "a1";
    }
    info = infovalue(sp, s2, n);
    if (info > Maxinfo)
    {
	Maxinfo = info;
  	cut = Av;
    }
  }
    return cut;
}
 
/************************************************************************/
/*	Discretizie continuous decision values					*/
/************************************************************************/
 
void DiscretizeDval(CDval,DDval)
/*				*/
float *CDval;
String *DDval;
{
	int i,j,k,MaxDcut, MaxCDclass, *CDvalFreq;
	char choice[4], *s;
	float *Buffer,*Arrange;
	float *CDArrange;
	float cutvalue;
	int *Tempvalfreq;
 
/*	List and count continuous decision values	*/
 
  Arrange = (float *) calloc(MaxItem, sizeof(float));
  Tempvalfreq = (int *) calloc(MaxItem, sizeof(int));
  MaxCDclass = NoArrange(MaxItem,CDval, Arrange, Tempvalfreq);
  CDvalFreq = (int *) calloc(MaxCDclass, sizeof(int));
  CDArrange = (float *) calloc(MaxCDclass, sizeof(float));
  for (k=0; k <= MaxCDclass-1; k++)
  {
    CDvalFreq[k] = Tempvalfreq[k];
    CDArrange[k] = Arrange[k];
  }
    printf("\n\tThe following continuous values are read in:\n");
    for (k=0; k <= MaxCDclass-1; k++)
    {
	printf("\n\t%3.2f(%d)",CDArrange[k], CDvalFreq[k]);
    }
    printf("\n\n");
    printf("\t Do you want to discretize decision values?");
    scanf("%s",choice);
    if (! strncmp(choice, "yes", 1))
    {
 	int readno;
 	int Dcutno;
	MarkD = true;
	printf("\n\n\tHow many cupt-points do you want?");
	scanf("%d", &readno);
	Buffer = (float *) calloc(readno, sizeof(float));
	for (k=0; k <= readno- 1; k++)
	{
	  printf("\n\tPlease enter the %dth cutpoint vlaue:",k+1);
	  scanf("%f", &cutvalue);
	  Buffer[k]=cutvalue;
	}
        Arrange = (float *) calloc(readno, sizeof(float));
        Tempvalfreq = (int *) calloc(readno, sizeof(int));
	MaxDcut = NoArrange(readno,Buffer, Arrange, Tempvalfreq);
	DcutNo = MaxDcut;
	Dcutpoint = (float *) calloc(DcutNo, sizeof(float));
	printf("\n\n\t%d different cutpoint values are read in\n\n", MaxDcut);
	for (k=0; k <= MaxDcut-1; k++)
	{
	  printf("\t%5.2f(%d)", Arrange[k], Tempvalfreq[k]);
	}
	printf("\n");
	Dcutpoint[0] = Arrange[0];
	for (i=0; i <= MaxItem-1; i++)
	{
	  if (CDval[i] >= Dcutpoint[0])
	  {
	    DDval[i] = makestring("D",1);
	  }
	}
	for (k=1; k <= MaxDcut-1; k++)
	{
	  Dcutpoint[k] = Arrange[k];
	  for (i=0; i <= MaxItem-1; i++)
	  {
	    if (CDval[i] >= Dcutpoint[k] && CDval[i] <  Dcutpoint[k-1])
 
	    DDval[i] = makestring("D",k+1);
	  }
	}
	for (i=0; i <= MaxItem-1; i++)
	{
	  if (CDval[i] <  Dcutpoint[MaxDcut-1])
	  {
	    DDval[i] = makestring("D",MaxDcut+1);
	  }
	}
 
    }
  else
  {
    printf("\n\n\t Each decision value will be treated as an individual\n");
    printf("\t discretized value due to your choice\n");
 
    for (i=0; i <= MaxItem-1; i++)
    {
      DDval[i] = CopyString(Dval[i]);
    }
  }
}
 
 
/*************************************************************************/
/*									 			 */
/*  Read all attribute names and a decision names                        	 */
/*  On completion, these names are stored in:				 	 */
/*	AttName		-  attribute names				 	 */
/*  with:								 			 */
/*									 			 */
/*  Other global variables set are:					 		 */
/*	MaxAtt		-  maximum attribute number			 */
/*									  			 */
/*  Note:  until the number of attributes is known, the name		 */
/*	   information is assembled in local arrays			 	 */
/*									 			 */
/*************************************************************************/
 
void GetData(void)
/*  ---------  */
{
    	FILE *Nf, *fopen();
    	char Fn[100], *Buffer, Filename[100],*endname;
	int AttCeiling = 100, ReadNo, i, j, choice;
	String *ReadValue;
        int ValCeiling=1000;
	float Av;
	int end;
	float CDval[1500];
	char VtypeD[15];		/* type of decison values */
    
/*  Open data file  */
 
    printf("\n\t Please enter your data file name:");
    scanf("%s", Filename);
    strcpy(Fn, Filename);
    if ( ! ( Nf = fopen(Fn, "r") ) ) Error(0, Fn, "");
 
    /*  Get attribute names from names file  */
 
    AttName = (String *) calloc(AttCeiling, sizeof(String));
    ReadNo = -1;
    
    do
    {
    	Buffer = (char *) calloc(1000, sizeof(char));
	ReadName(Nf, Buffer);
 
	if (++ReadNo >= AttCeiling)
	{
	    AttCeiling   += 100;
	    AttName   = (String *) realloc(AttName, AttCeiling*sizeof(String));
	}
	AttName[ReadNo] = CopyString(Buffer);
    }   while ( Delimiter == ',');
	printf("\n");
	strcpy(DecisionName,AttName[ReadNo-1]);
	MaxAtt = ReadNo - 2;
	printf("\n\t%d attributes are read in\n",MaxAtt + 1);
	printf("\twhich are as follows:\n\n");
 
/* check and copy attribute names */
 
	for (j=0; j <= MaxAtt; j++)
      { 
       printf("\t%s", AttName[j]);
      }
	printf("\n\n\n");
 
 
/* READ VALUES OF ATTRIBUTES AND DECISION FROM DATA FILE */
 
	ReadValue = (String *) calloc(ValCeiling, sizeof(String));
    	ReadNo = 0-1;
	do 
	{
    	Buffer = (char *) calloc(1000, sizeof(char));
	ReadName(Nf, Buffer);
 
	if ( ++ReadNo >= ValCeiling )
	{
	 ValCeiling += 1000;
	 ReadValue = (String *) realloc(ReadValue, ValCeiling*sizeof(String));
	}
	ReadValue[ReadNo] = CopyString(Buffer);
	} while (Delimiter != EOF);
	fclose(Nf);
	if (ReadNo%(MaxAtt + 2) !=0)
	{
	printf("Values read in do not match the number of attributes \n");
	exit(1);
	}
	MaxItem = ReadNo/(MaxAtt + 2);
 
	/* copy ReadValue to Aval[item][Att] and Dval[item] */
 
	DDval = (String *) calloc(MaxItem, sizeof(String));
	Dval = (String *) calloc(MaxItem, sizeof(String));
	conDval = (float *) calloc(MaxItem,sizeof(float));
 
	for (j=0; j<= MaxItem - 1; j++)
       {  
	  for (i=0; i <= MaxAtt; i++)
	  {
	   Av = strtod(ReadValue[j*(MaxAtt + 2) + i], &endname);
	   if (endname == ReadValue[j*(MaxAtt+2)+i] || *endname !='\0')
	   Error(4,AttName[i], ReadValue[j*(MaxAtt+2)+i]);
	   Aval[j][i] = Av;
	  }
	   Dval[j] = CopyString(ReadValue[j*(MaxAtt+2)+MaxAtt+1]);
        }
	
/* Judge whether decision values are continuous  */
 
	for (j=0; j <= MaxItem-1; j++)
	{
	  Av = strtod(Dval[j],&endname);
	  if (endname == Dval[j] || *endname !='\0')
	  {
		strcpy(VtypeD, "Discrete");
	 	goto end;
	  }
	}
	strcpy(VtypeD,"Continuous");
	end: ;
	for (j=0; j <= MaxItem-1; j++)
	{  
	if (!strcmp(VtypeD, "Discrete"))
	  {
	   DDval[j] = CopyString(Dval[j]);
 	  }
	else if (!strcmp(VtypeD, "Continuous"))
	  { 
	   Av = strtod(Dval[j],&endname);
	   CDval[j] = Av;
	   conDval[j] = Av;
	  }
	}
	printf("\n\n");
	if (!strcmp(VtypeD, "Continuous"))
	DiscretizeDval(CDval, DDval);
     }
 
/*************************************************************************/
/*												 */
/*			Error messages					 		 */
/*									 			 */
/*************************************************************************/
 
    Error(n, s1, s2)
/*  -----  */
    short n;
    String s1, s2;
{
    static char Messages=0;
 
    printf("\nERROR:  ");
    switch(n)
    {
	case 0: printf("cannot open file %s%s\n", s1, s2);
		exit(1);
 
	case 1:	printf("[ expected before attribute name \n");
		exit(1);
 
	case 2:	printf("unexpected eof while reading attribute %s\n", s1);
		break;
 
	case 3: printf("attribute %s has only one value\n", s1);
		break;
 
	case 4: printf("Attribute %s value is illegal\n",s1);
		break;
 
	case 5: printf("Decision value %s is illegal\n", s2);
    }
 
    if ( ++Messages > 10 )
    {
	printf("Error limit exceeded\n");
	exit(1);
    }
}
 
 
/****************************************************************************/
/*THIS IS FOR WRITING OUT FINAL DISCRETIZED TABLE AND CUTPOINTS*/
/****************************************************************************/
 
void writeout(void)
 
{
  int i,j,k;
  int No, *VF, NoA,*VFA;
  float *cut, *cutarrange,*A,*Aarrange;
  float *ncut;
  FILE *fp1, *fp2;
  char Fn1[100], Fn2[100];
 /* printf("\n\n\t The discretized table is shown as follows:\n");
  printf("\n");
  for (i=0; i <= MaxAtt; i++)
  {
      printf("\t%s",AttName[i]);
  }
  printf("\t%s",DecisionName);
  printf("\n");
 
  for (i=0;i<=MaxItem-1;i++)
  { 
    printf("\n");
    for (j=0; j<= MaxAtt;j++)
    {
	if (Mark[j])
	{
          printf("\t%s",Acurrent[i][j]);
	}
        else if (!Mark[j])
        {
      	  printf("\t%5.2f",Aval[i][j]);
	}
    }
  printf("\t%s",DDval[i]);
  }
  printf("\n\n\t The cutpoints for each attribute are listed as follows:\n");  
  for (i=0;i<=MaxAtt;i++)
  { 
    if (Mark[i])
    {
      printf("\n attribute %s:",AttName[i]);
      for (j=0; j<= record[i]-1;j++)
      printf("\t%5.2f",cutpoint[i][j]);
    }
  }
*/
  printf("\n");  
  printf("\t Please enter your file name for storing the discretized table:");
  scanf("%s", outfile);
  strcpy(Fn1, outfile);
  fp1=fopen(Fn1,"w");
  fprintf(fp1,"%c",'[');
  for (i=0; i <= MaxAtt; i++)
  {
      fprintf(fp1,"\t%s", AttName[i]);
  }
  fprintf(fp1,"\t%s\t%c\n",DecisionName,']');
  for (j=0; j <= MaxItem-1; j++)
  {
    fprintf(fp1,"\n");
    for (i=0; i <= MaxAtt; i++)
    {
   	if (Mark[i])
	{
	  fprintf(fp1,"\t%s",Acurrent[j][i]);
        }
	else if (!Mark[i])
	{
	  fprintf(fp1,"\t%5.2f",Aval[j][i]);
	}
    }
    fprintf(fp1,"\t%s",DDval[j]);
    fprintf(fp1,"\n");
  }
  fclose(fp1);
  printf("\n\t Please enter your file name for storing cutpoint information:");
  scanf("%s",cutfile);
  strcpy(Fn2,cutfile);
  VFA = (int *) calloc(MaxItem,sizeof(int));
  A = (float *) calloc(MaxItem,sizeof(float));
  Aarrange = (float *) calloc(MaxItem,sizeof(float));
  VF = (int *) calloc(1,sizeof(int));
  cutarrange = (float *) calloc(1,sizeof(float));
  fp2=fopen(Fn2,"w");
  fprintf(fp2,"\n\t The following is cutpoint information:\n");
  for (i=0; i <= MaxAtt; i++)
  {
    if (Mark[i])
    {
	fprintf(fp2,"\n");
	fprintf(fp2,"For attribute %s:\n",AttName[i]);
  	for (j=0; j <= MaxItem-1; j++)
	{
	  A[j] = Aval[j][i];
	}
	NoA = NoArrange(MaxItem, A, Aarrange,VFA);
	VF = (int *) realloc(VF,record[i]*sizeof(int));
	cutarrange = (float *) realloc(cutarrange, record[i]*sizeof(float));
	No = NoArrange(record[i],cutpoint[i], cutarrange,VF);
	ncut = (float *) calloc(No, sizeof(float));
	for (k=0; k <=No-1; k++)
	{
	  for (j=0; j <=NoA-1; j++)
	  {
	    if (cutarrange[k] >=Aarrange[j+1] && cutarrange[k] < Aarrange[j])
	    {
	      ncut[k] = Aarrange[j];
	      break;
	    }
	  }
	}
	fprintf(fp2,"\t%s = [%5.2f, %5.2f]\n",makedisattname(i,1),ncut[0], Aarrange[0]);
	for (j=1; j<=No-1; j++)
	{
	  fprintf(fp2,"\t%s = [%5.2f, %5.2f)\n",makedisattname(i,j+1),ncut[j], ncut[j-1]);
	}
	if (cutarrange[No-1] >= Aarrange[NoA-1])
	{
	  fprintf(fp2,"\t%s = [%5.2f, %5.2f)\n",makedisattname(i,No+1), Aarrange[NoA-1],ncut[No-1]);
	}
	free(ncut);
    }
  }
  if (MarkD)
  {
	fprintf(fp2,"\n");
	fprintf(fp2,"For decision %s:\n",DecisionName);
	VFA = (int *) calloc(MaxItem-1,sizeof(int));
	Aarrange = (float *) calloc(MaxItem-1,sizeof(float));
	NoA = NoArrange(MaxItem, conDval, Aarrange,VFA);
	fprintf(fp2,"\t%s = [%5.2f, %5.2f]\n",makestring("D",1),Dcutpoint[0], Aarrange[0]);
	for (j=1; j<=DcutNo-1; j++)
	{
	  fprintf(fp2,"\t%s = [%5.2f, %5.2f)\n",makestring("D",j+1),Dcutpoint[j],Dcutpoint[j-1]);
	}
	if (Dcutpoint[DcutNo-1] > Aarrange[NoA-1])
	{
	  fprintf(fp2,"\t%s = [%5.2f, %5.2f)\n",makestring("D",DcutNo+1),Aarrange[NoA-1],Dcutpoint[DcutNo-1]);
	}
    }
 
  fclose(fp2);
}
/********************************************************************/
/*	Test cutability								   */
/********************************************************************/
 
Boolean cutable(reals,n)
 
float *reals;
int n;
 
{
  int *Tempvalfreq;
  float *sarrange;
  int Nos;
  Tempvalfreq = (int *) calloc(n,sizeof(int));
  sarrange = (float *) calloc(n,sizeof(float));
  Nos = NoArrange(n,reals,sarrange,Tempvalfreq);
  free(Tempvalfreq);
  free(sarrange);
 /* free(reals);*/
  if (Nos > 1) return true;
  else
  return false;
}
/********************************************************************/
/*	Test cutability of subtable						   */
/********************************************************************/

Boolean subcutable(att,sf,sp,n)
 
int att,n;
float *sf;
String *sp;
 
{  
  int i,j,No,Nos;
  int *VF,*Tempvalfreq;
  float *Buffer,*sarrange, *attval;
  float bestcut,upinterval;
  sarrange = (float *) calloc(n,sizeof(float));
  Tempvalfreq = (int *) calloc(n,sizeof(int));
  Buffer = (float *) calloc(MaxItem, sizeof(float));
  attval = (float *) calloc(MaxItem, sizeof(float));
  VF = (int *) calloc(MaxItem, sizeof(int));
  bestcut = Findcut(sf,sp,n);
  Nos = NoArrange(n,sf,sarrange,Tempvalfreq); 
  for (i=0; i <=Nos-1; i++)
  {
    if (sarrange[i] == bestcut) upinterval = sarrange[i-1];
  }
  for (i=0; i <= MaxItem-1; i++)
  {
    attval[i]=Aval[i][att];
  }
  No = NoArrange(MaxItem,attval, Buffer, VF);
   
  for (j=0; j <= No-1; j++)
  {
    if (Buffer[j] > bestcut && Buffer[j] < upinterval) return false;
  }
  return true;
}

/*********************************************************************/
/* THIS IS DISCRETIZATION ROUTINE FOR SUBTABLE		     */
/*********************************************************************/
 
Boolean subdiscretize(void)
 
{
  int L;
  int end1;
  String **Ar;
  int i,j,k,h,m,No;
  float *s1, **Asub;
  float bestcut;
  int NoDsub;
  int NoAcurrent,Noacurrent,Bestatt;
  float *VFa, *VFDsub;
  String *Arrangeac, *acurrent, *DA;
  String *ArrangeDsub, *Dsub;
  acurrent = (String *) calloc(MaxItem, sizeof(String));
    NoAcurrent =0;
      for (k=0; k <= MaxAtt;k++)
      {
	NoAcurrent += Mark[k];
      }
    Ar = (String **) calloc(MaxItem, sizeof(String *));
    for (j=0; j <= MaxItem-1; j++)
    {
      Ar[j] = (String *) calloc(NoAcurrent, sizeof(String));
      No = -1;
      for (k=0; k <= MaxAtt;k++)
      { 
	if (Mark[k])
	{
	No++;
        Ar[j][No] = CopyString(Acurrent[j][k]);
	}
      }
    }
    Findacurrent(Ar, acurrent, NoAcurrent);
    for (j=0; j <= MaxItem-1; j++)
    {
	Ar[j] = (String *) realloc(Ar[j], 2*sizeof(String));
        Ar[j][0] = CopyString(acurrent[j]);
    }
	Arrangeac = (String *) calloc(MaxItem, sizeof(String));
	VFa = (float *) calloc(MaxItem, sizeof(float));
	DA = (String *) calloc(MaxItem, sizeof(String));
  do
  {
	Noacurrent = DArrange(MaxItem, acurrent, Arrangeac, VFa);
	for (i=0; i <=Noacurrent-1; i++)
	{
	  L = VFa[i];
	  if (L > 1)
	 {
	  Asub = (float **) calloc(L,sizeof(float *));
	  Dsub = (String *) calloc(L,sizeof(String));
	  for (h=0; h <= L-1; h++)
	  {
	    Asub[h] = (float *) calloc(MaxAtt+1,sizeof(float));
	  }
	  k= -1;
	  for (j=0; j <= MaxItem-1; j++)
	  {
	    if ( !strcmp(acurrent[j], Arrangeac[i]))
	    {
	   	k++;
		Dsub[k] = CopyString(object[j]);
		for (m=0; m <=MaxAtt; m++)
		{
		  Asub[k][m] = Aval[j][m];
		}
	     }
	   }
	   VFDsub = (float *) calloc(L, sizeof(float));
	   ArrangeDsub = (String *) calloc(L, sizeof(String));
	   NoDsub = DArrange(L,Dsub,ArrangeDsub, VFDsub);
	   free(VFDsub);
	   free(ArrangeDsub);
	   if (NoDsub > 1)
	   {
	     goto end1;
	   }
	   else
	   {
             free(Dsub);
             for (h=0; h <= L-1; h++)
             {
	        free(Asub[h]);
             }
             free(Asub);
	   }
	 }
        }
       end1: ;
       Bestatt = Findatt(Asub, Dsub, L);
       s1 = (float *) calloc(L, sizeof(float));
	 for (j=0; j <=L-1; j++)
	 {
	    s1[j] = Asub[j][Bestatt];
	 }
       for (h=0; h <= L-1; h++)
       {
	 free(Asub[h]);
       }
       free(Asub);
	 if (!cutable(s1,L))
  	 {
	   printf("\n Not cutble\n");
	 for (j=0; j <=L-1; j++)
	 {
	    printf("\n%f\n",s1[j]);
	 }
	   exit(1);
	 }
	else 
	{
	  bestcut = Findcut(s1,Dsub,L);
	}
       free(Dsub);
       if (Mark[Bestatt])
       {
	 record[Bestatt]++;
	 cutpoint[Bestatt] = (float *) realloc(cutpoint[Bestatt], record[Bestatt]* sizeof(float));
	 cutpoint[Bestatt][record[Bestatt] - 1] = bestcut;
	 makedisattvalue(Bestatt,record[Bestatt],cutpoint[Bestatt],DA);
	 for (j=0; j <= MaxItem-1; j++)
	 {
	   Acurrent[j][Bestatt] = CopyString(DA[j]);
	   Ar[j][1] = CopyString(DA[j]);
  	 }
       }
       else
       {
	  Mark[Bestatt] = true;
	  cutpoint[Bestatt] = (float *) calloc(1, sizeof(float));
	  cutpoint[Bestatt][0] = bestcut;
	  record[Bestatt]++;
	  makedisattvalue(Bestatt,1, cutpoint[Bestatt], DA);
	  for (j=0; j <= MaxItem-1; j++)
	  {
	    Acurrent[j][Bestatt] = CopyString(DA[j]);
	    Ar[j][1] = CopyString(DA[j]);
	  }
       }
       Findacurrent(Ar, acurrent,2);
       for (j=0; j <= MaxItem-1; j++)
       {
         Ar[j][0] = CopyString(acurrent[j]);
       }
  } while (!Test(acurrent, object));
  return true;
}
 
/*********************************************************************/
/* THIS IS DISCRETIZATION FROM ALL ATTRIBUTES		     */
/*********************************************************************/
 
Boolean discretize1(void)
 
{
  String **Ar;
  int No;
  int i,j,no,no1;
  float *sp;
  String *da, *acurrent;
  int NoD, k;
  String *ArrangeD;
  float *VFD;
  printf("\t THIS VERSION OF DISCRETIZATION BEGINS FROM ALL ATTRIBUTES\n");
  Findobj();
  Mark = (Boolean *) calloc(MaxAtt+1, sizeof(Boolean));
  record = (int *) calloc(MaxAtt+1, sizeof(int));
  Acurrent = (String **) calloc(MaxItem, sizeof(String *));
  cutpoint = (float **) calloc(MaxAtt+1, sizeof(float *));
  acurrent = (String *) calloc(MaxItem, sizeof(String));
  for (j=0; j <= MaxItem-1; j++)
    {
        acurrent[j] = "same";
    	Acurrent[j] = (String *) calloc(MaxAtt+1, sizeof(String));
    }
  for (i=0; i <= MaxAtt; i++)
  {
    record[i] =0;
  }
  i=0;
  no=0;
  no1=0;
  Ar = (String **) calloc(MaxItem, sizeof(String *));
  for (j=0; j <= MaxItem-1; j++)
  {
      Ar[j] = (String *) calloc(2,sizeof(String));
  }
  sp = (float *) calloc(MaxItem, sizeof(float));
  da = (String *) calloc(MaxItem, sizeof(String));
  while (i <= MaxAtt && !Test(acurrent, object))
  {
    no1++;
    cutpoint[i] = (float *) calloc(1, sizeof(float));
    for (j=0; j <= MaxItem-1; j++)
    {
    	sp[j] = Aval[j][i];
	Ar[j][0] = CopyString(acurrent[j]);
    }
    if (!cutable(sp,MaxItem))
   {
    no++;
   }
   else if (cutable(sp,MaxItem))
  {
    cutpoint[i][0] = Findcut(sp,object, MaxItem);
    record[i]++;
    Mark[i] = true;
    makedisattvalue(i,1,cutpoint[i],da);
    for (j=0; j <= MaxItem-1; j++)
    {
      Acurrent[j][i] = CopyString(da[j]);
      Ar[j][1] = CopyString(da[j]);
    }
    Findacurrent(Ar, acurrent,2);
   }
    i++;
    printf("\nNow attribute %d\n",i);
  }
  if (no1==0)
  {
    printf("\n\t Your discretization reference vector has only one value\n");
    exit(1);
  }
  if (no == MaxAtt+1)
  {
    printf("\n\t The decision table is uncutable \n");
    exit(1);
  }
  if (!Test(acurrent,object))
  {
    return false;
  }
  else if (Test(acurrent,object))
  {
    return true;
  }
}
 


/**********************************************************************/
/* THIS IS DISCRETIZATION FROM ONE BEST ATTRIBUTE		      */
/**********************************************************************/
 
Boolean discretize2(void)
 
{
  int Bestatt;
  float bestcut;
  int i,j;
  float *s; 
  String *sp;
  float **A;
  printf("\t THIS VERSION OF DISCRETIZATION BEGINS WITH ONE ATTRIBUTE\n");
  Findobj();
  A = (float **) calloc(MaxItem, sizeof(float *));
  for (i=0; i <=MaxItem-1; i++)
  {
    A[i] = (float *) calloc(MaxAtt+1, sizeof(float));
    for (j=0; j <= MaxAtt; j++)
    {
	A[i][j]=Aval[i][j];
    }
  }
  Bestatt = Findatt(A,object, MaxItem);
  for (i=0; i <= MaxItem-1; i++)
  {
    free(A[i]);
  }
  free(A);
  record = (int *) calloc(MaxAtt+1, sizeof(int));
  for (i=0; i <= MaxAtt; i++)
  {
    record[i] =0;
  }
  Mark = (Boolean *) calloc(MaxAtt+1, sizeof(Boolean));
  Acurrent = (String **) calloc(MaxItem, sizeof(String *));
  s = (float *) calloc(MaxItem, sizeof(float));
  for (i=0; i <= MaxItem-1; i++)
  {
    Acurrent[i] = (String *) calloc(MaxAtt+1, sizeof(String));
    s[i] = Aval[i][Bestatt];
  }
  if ( !cutable(s,MaxItem))
  {
    printf("\n\t The decision table is uncutable\n");
    exit(1);
  }
  else
  {
  Mark[Bestatt] = true;
  bestcut = Findcut(s, object, MaxItem);
  free(s);
  sp = (String *) calloc(MaxItem, sizeof(String));
  cutpoint = (float **) calloc(MaxAtt+1, sizeof(float *));
  cutpoint[Bestatt] = (float *) calloc(1,sizeof(float));
  cutpoint[Bestatt][0] = bestcut;
  record[Bestatt]++;
  makedisattvalue(Bestatt,1,cutpoint[Bestatt],sp);
  for (i=0; i <=MaxItem-1; i++)
  {
    Acurrent[i][Bestatt] = CopyString(sp[i]);
  }
  }
  if ( !Test(sp, object)) return false;
  else return true;
}
 
/********************************************************************/
/*	Test whether an attribute can be wholy merged		    */
/********************************************************************/
 
Boolean wholemerge(att)
 
int att;
 
{
  String **B, *bcurrent;
  int i,j;
  int NoA = 0;
  int No;
  for (i=0; i <= MaxAtt; i++)
  {
    NoA += Mark[i];
  }
  bcurrent = (String *) calloc(MaxItem,sizeof(String));
  B = (String **) calloc(MaxItem, sizeof(String *));
  for (j=0; j <= MaxItem-1; j++)
  {
    No = -1;
    B[j] = (String *) calloc(NoA-1, sizeof(String));
    for (i=0; i <=MaxAtt; i++)
    {
	if (Mark[i] && i != att)
	{
	  No++;
	  B[j][No] = CopyString(Acurrent[j][i]);
 	}
    }
  }
  Findacurrent(B,bcurrent,NoA-1);
  for (j=0; j <= MaxItem-1; j++)
  {
    free(B[j]);
  }
  free(B);
  printf("\n I am here in whole merge \n");
  if (Test(bcurrent,object)) return true;
  else return false;
}
  
/********************************************************************/
/*  THIS IS FOR MERGING DISCRETIZED TABLE			    */
/********************************************************************/
 
void merge(void)
 
{
  int i,j,k,h,m;
  FILE *fp1,*fp;
  char Fn1[100],Fn2[100];
  int n;
  int NoA;
  float *cut1arrange, *cutarrange, *cut1;
  String *s1;
  int *VFcut1, *VFcut,*l;
  int nocuto, nocut1;
  int No2;
  int nocut;
  Boolean *cutmark;
  String **B, *sp, *s,*bcurrent;
  float *cut;
  int No;
  int No3;
  bcurrent = (String *) calloc(MaxItem,sizeof(String));
  s1= (String *) calloc(MaxItem, sizeof(String));
  B = (String **) calloc(MaxItem, sizeof(String *));
  for (k=0; k <= MaxItem-1; k++)
  {
  B[k] = (String *) calloc(1, sizeof(String));
  }
  sp = (String *) calloc(MaxItem, sizeof(String));
  cut = (float *) calloc(1,sizeof(float));
  l = (int *) calloc(1, sizeof(int));
  cutmark = (Boolean *) calloc(1,sizeof(Boolean));
  printf("\n\t Please enter the file name for storing merging information:");
  scanf("%s",filemerge);
  strcpy(Fn1, filemerge);
  fp = fopen(Fn1,"w");
  fprintf(fp,"\n\t The following is merging information:\n\n");
  for (i=0; i <= MaxAtt; i++)
  {
   if (Mark[i])
   {
    NoA = 0;
    for (n=0; n <= MaxAtt; n++)
    {
      NoA += Mark[n];
    }
    for (j=0; j <= MaxItem-1; j++)
    {
      B[j] = (String *) realloc(B[j],(NoA-1)*sizeof(String));
      No3 = -1;
      for (k=0; k <= MaxAtt;k++)
      { 
	if (Mark[k] && k != i)
	{
	No3++;
        B[j][No3] = CopyString(Acurrent[j][k]);
	}
      }
    }
    Findacurrent(B, bcurrent, NoA-1);
    for (j=0; j <= MaxItem-1; j++)
    {
	B[j] = (String *) realloc(B[j], 2*sizeof(String));
        B[j][0] = CopyString(bcurrent[j]);
    }
    fprintf(fp,"\n");
    if (Test(bcurrent,object))
    {
	Mark[i] = false;
	fprintf(fp,"\nThe attribute  %s is redundant\n",AttName[i]);
    }
    else if (!Test(bcurrent,object))
    {
	cutmark = (Boolean *) realloc(cutmark,record[i]*sizeof(Boolean));
	for (j=0; j <= record[i]-1; j++) cutmark[j] = false;
  	for (j=0; j <= record[i]-1; j++)
	{
	  nocut = 0;
	  for (k=0; k <=record[i]-1; k++)
	  {
	    nocut += !cutmark[k];
	  }
	  cut = (float *) realloc(cut,(nocut-1)*sizeof(float));
	  No = -1;
	  for (k=0; k <=record[i]-1; k++)
	  {
	    if (!cutmark[k] && k != j)
	    {
	      No++;
	      cut[No] = cutpoint[i][k];
	    }
	  }
	  makedisattvalue(i,nocut-1,cut,sp);
  	  for (k=0; k <= MaxItem-1; k++)
  	  {
/*
	    No3 = -1;
    	    B[k] = (String *) realloc(B[k],NoA*sizeof(String));
    	    for (h=0; h <=MaxAtt; h++)
    	    {
		if (Mark[h])
	        {
	  	  No3++;
	  	  if (h != i)
	  	  {
	    	    B[k][No3] = CopyString(Acurrent[k][h]);
	  	  }
	  	  else if (h==i)
	  	  {
		    B[k][1] = CopyString(sp[k]);
		  }
		}
	    }
*/
	   B[k][1] = CopyString(sp[k]);
	  }
  	  Findacurrent(B,bcurrent,2);
  	  if (Test(bcurrent,object)) cutmark[j] = true;
	 }
	 cut = (float *) realloc(cut,nocut*sizeof(float));
	 No2 = -1;
	 for (k=0; k <=record[i]-1; k++)
	  {
	    if (!cutmark[k])
	    {
	      No2++;
	      cut[No2] = cutpoint[i][k];
	    }
	  }
	 cut1arrange = (float *) calloc(nocut,sizeof(float));
	 cutarrange = (float *) calloc(record[i],sizeof(float));
	 VFcut1 = (int *) calloc(nocut, sizeof(int));
	 VFcut = (int *) calloc(record[i], sizeof(int));
	 nocut1 = NoArrange(nocut,cut,cut1arrange,VFcut1);
	 nocuto = NoArrange(record[i],cutpoint[i],cutarrange,VFcut);
	 free(VFcut);
	 free(VFcut1);
	 l = (int *) realloc(l,nocut1*sizeof(int));
	 for (m=0; m <= nocut1-1; m++)
	 {
	  for (h=0; h <= nocuto-1; h++)
	  {
	    if (cutarrange[h]==cut1arrange[m]) l[m] = h;
	  }
	 }
	 fprintf(fp,"\n\t%s = %s",makedisattname(i,1),makedisattname(i,1));
	 if (l[0] > 0)
	{
	 for (m=1; m <= l[0]; m++)
	 {
	   fprintf(fp," & %s",makedisattname(i,m+1));
	 }
	}
	 for (h=1; h <= nocut1-1; h++)
	 {
	   fprintf(fp,"\n");
	   fprintf(fp,"\t%s = %s",makedisattname(i,h+1), makedisattname(i,l[h-1]+2));
	   for (m=l[h-1]+2; m <= l[h]; m++)
	   {
		fprintf(fp," & %s", makedisattname(i,m+1));
     	   }
 	 }
	 fprintf(fp,"\n");
	 fprintf(fp,"\t%s = %s",makedisattname(i,nocut1+1),makedisattname(i,l[nocut1-1]+2));
	 if (l[nocut1-1] < nocuto-1)
	{
	 for (m=l[nocut1-1]+2; m <= nocuto; m++)
	 {
	   fprintf(fp," & %s", makedisattname(i,m+1));
	 }
	}
	 makedisattvalue(i, nocut, cut,s1);
	 for(m=0; m <=MaxItem-1; m++)
	 {
	   Acurrent[m][i] = CopyString(s1[m]);
	 }
    }
   }
  }
  fclose(fp);
  printf("\n\n\t Please enter file name for stroing after-mering\n");
  printf("\t discretized decision table:");
  scanf("%s",outaftermerge);
  strcpy(Fn2, outaftermerge);
  fp1=fopen(Fn2,"w");
  fprintf(fp1,"%c",'[');
  for (i=0; i <= MaxAtt; i++)
  {
      fprintf(fp1,"\t%s", AttName[i]);
  }
  fprintf(fp1,"\t%s\t%c\n",DecisionName,']');
  for (j=0; j <= MaxItem-1; j++)
  {
    fprintf(fp1,"\n");
    for (i=0; i <= MaxAtt; i++)
    {
   	if (Mark[i])
	{
	  fprintf(fp1,"\t%s",Acurrent[j][i]);
        }
	else if (!Mark[i])
	{
	  fprintf(fp1,"\t%5.2f",Aval[j][i]);
	}
    }
    fprintf(fp1,"\t%s",DDval[j]);
  }
    fprintf(fp1,"\n");
    fclose(fp1);
}
 
 
/*******************************************************************/
/*											 */
/*     This is main routine							 */
/*											 */
/*******************************************************************/
 
 
main()
{ 
  Boolean discrable;
  int i,j,k,No,*VF;
  int choice, choice1, choice2;
  char choice3[4];
  float *Buffer,*attval;
  printf("\n\n\n\n\n");
  printf("\t**************************************************************\n");
  printf("\t*								 		*\n");
  printf("\t*	     WELCOME TO GLOBAL-DISCRETIZATION 0.1	*\n");
  printf("\t*										*\n");
  printf("\t*THIS PROGRAM IS PRIMARILY DESIGEND FOR A COURSE	*\n");
  printf("\t*PROJECT BY Mr LIPING LIU AT UNIVERSITY OF KANSAS 	*\n");
  printf("\t*		IN MARCH 1993.					*\n");
  printf("\t*	ILLEGAL REPRODUCTION IS PROHIBITED 		*\n");
  printf("\t*		BY THE COPYRIGHT			 		*\n");.
  printf("\t*										*\n");
  printf("\t*	FOLLOW MENU INSTRUCTIONS AND ENJOY 		*\n");
  printf("\t*		THIS SMART PROGRAM. 				*\n"); 
  printf("\t*								 		*\n");
  printf("\t**************************************************************\n");
 
  do
  {
    printf("\n\n\n");  
    printf("\n\t	MAIN CHOICE MENU\n\n");
    printf("\t 1. Start Discretization of continuous decision table\n");
    printf("\t 2. Renew Discretization for another table \n");
    printf("\t 3. Quit\n\n");
    printf("\t Please enter your choice:");
    scanf("%d",&choice);
    if (choice !=1 && choice != 2 && choice != 3)
    {
	printf("\n\t Unknown choice\n");
	exit(1);
    }
    else if (choice==1 || choice == 2)
    {
      printf("\n\t You now begin a new disctretization\n");
      GetData();
      printf("\n\n\t%d items and %d attributes are read in\n", MaxItem, MaxAtt+1);
  printf("\n\t The read in decision table is as follows:\n\n");
      for (j=0; j <= MaxAtt; j++)
      {
	printf("%s\t",AttName[j]);
      }
      printf("Decision\n");
      for (i=0;i<=MaxItem-1; i++)
      { 
        for (j=0; j<= MaxAtt; j++)
	{
	  printf("%5.2f\t",Aval[i][j]);
	}
	printf("%s\n",DDval[i]);
     }
      printf("\n\n\n");  
      printf("\n\n\t Which test criterion do you use:\n\n");
      printf("\t 1. Gain Ratio\n");
      printf("\t 2. Information Gain\n");
      printf("\n\n\t Please enter your choice:");
      scanf("%d",&choice1);
      switch(choice1)
      {
        case 1: strcpy(criterion, "GainRatio");
		break;
        case 2: strcpy(criterion, "InfoGain");
		break;
        default: printf("\t Unknown Menu Choice\n");
		exit(1);
      }
 
      printf("\n\n\t Discretization Version Choice\n\n");
      printf("\t 1. From all attributes \n");
      printf("\t 2. From one best attribute\n");
      printf("\n\t Please enter your choice:");
      scanf("%d", &choice2);
      switch(choice2)
      {
	case 1: discrable = discretize1();
		break;
	case 2: discrable = discretize2();
		break;
	default: printf("\t Unknown Menu Choice\n");
		exit(1);
      }
      if (!discrable) discrable = subdiscretize();
      writeout();
      printf("\n\n\t Do you want to merge discretized values?\n");
      printf("\t Please answer yes or no:");
      scanf("%s", choice3);
      if ( !strcmp(choice3,"yes", 1))
      {
	merge();
	printf("\n\t Your final discretized table is in %s\n",outaftermerge);
	printf("\n\t Relevant cut information can be found in %s\n",cutfile);
	printf("\n\t You can find merging information in %s\n", filemerge);
      }
      else
      {
	printf("\n\t The discretized table is in %s\n", outfile);
	printf("\n\t You can find cut information in %s\n", cutfile);
      }
    }
  } while( choice != 3);
  exit(0);
}
