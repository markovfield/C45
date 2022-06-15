
/*************************************************************************/
/*												 												 */
/* 	THIS PROGRAM IS USED FOR CONVERTING LERS FORMAT 		 				 */
/*		INTO C4.5 FORMAT 							 										 */
/*               OF INPUT DATA FILES. IT IS  CREATED 			 	 			 */
/*               BY LIPING LIU IN MARCH OF 1993				 					 */
/*	    ILLEGAL COPY IS PROHIBITED BY ITS COPYRIGHT		 						 */
/*	  -----------------------------------------------		 	 				 */
/*	              Student ID#: 536529				 		 						 */
/*									 			 												 */
/*	Instruction: cc -o convert convert.c and 					 					 */
/*	use convert and follow the menu.				 		 							 */
/*									 			 												 */
/*************************************************************************/
  
#include "def2.i"
#include "type2.i"
#include "ext2.i"
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
 
	 char Delimiter;		/* string reading stop sign */
	 short MaxAtt;			/* number of attributes */
	 String *AttName; 		/* attribute names and class names */
	 short MaxItem=0;		/* number of examples */
	 Boolean *Mark;			/* mark of attributes value types */
	 Boolean MarkD;			/* decision value type*/
	 String **Acurrent;		/* discretized table */
	 char filestemN[100];		/* file of attribute value names*/
	 char filestemV[100];		/* file of attribute values */
	 char filename[100];		/* input data file */
	 String *Dval;			/* read in decision values */
	 char DecisionName[100];	/* decision name */
 
 
/*************************************************************************/
/*									 			 												 */
/* Read a string from file f character by character into string s 		 */
/*  - Embedded periods are permitted. Set delimiter			 				 */
/*												 												 */
/*************************************************************************/
 
#define  Space(s)	(s == ' ' || s == '\n' || s == '\t')
#define  SkipComment while( (c = getc(f)) != '\n')
 
Boolean ReadName(f, s)
/*      ---------  */
    FILE *f;
    String s;
{
    register char *Sp=s;
    register int c;
    String CopyString();
 
/* find the first nonspace character */
 
 	
	while ((c=getc(f))=='[' || Space(c) || c == '!' )
	{
	if (c == '!') SkipComment;
	}
 
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
 
/*************************************************************************/
/*												 */
/*	Allocate space then copy string into it					 */
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

/**********************************************************************/
/*											      */
/*	Arrange an array of continuous data in the order		      */
/*      from large to small, determine the frequency of 		      */
/*      of each number, and return the number of different data       */
/*											      */
/**********************************************************************/
 
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
/*	Rearrange Discrete Values				 		    */
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
 
 
/*************************************************************************/
/*									 			 */
/*  Read all attribute names and a decision names                       	 */
/*  On completion, these names are stored in:					 */
/*	AttName		-  attribute names					 */
/*  with:											 */
/*												 */
/*  Other global variables set are:							 */
/*	MaxAtt		-  maximum attribute number			 */
/*												 */
/*  Note:  until the number of attributes is known, the name		 */
/*	   information is assembled in local arrays				 */
/*												 */
/*************************************************************************/
 
void GetData(void)
/*  ---------  */
{
    	FILE *Nf, *fopen();
    	char Fn[100], *Buffer,*endname;
	int AttCeiling = 100, ReadNo, i, j, choice;
	String *ReadValue;
        int ValCeiling=1000;
	float Av;
	int end,end1;
    
/*  Open data file  */
 
    printf("\n\t Please enter your input data file name:");
    scanf("%s", filename);
    strcpy(Fn, filename);
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
 
/*  copy attribute names */
 
	for (j=0; j <= MaxAtt; j++)
      { 
       printf("\t%s", AttName[j]);
      }
	printf("\n\n\n");
 
 
/* READ VALUES OF ATTRIBUTES AND DECISION FROM DATA FILE */
 
	ReadValue = (String *) calloc(ValCeiling, sizeof(String));
    	ReadNo = -1;
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
	/*printf("\t%d %s\n",ReadNo,ReadValue[ReadNo]);*/
	if (ReadNo%(MaxAtt + 2) !=0)
	{
	printf("Values read in do not match the number of attributes \n");
	exit(1);
	}
	MaxItem = ReadNo/(MaxAtt + 2);
 
	/* copy ReadValue to Aval[item][Att] and Dval[item] */
 
	Dval = (String *) calloc(MaxItem, sizeof(String));
	Acurrent = (String **) calloc(MaxAtt+1, sizeof(String *));
	for (i=0; i<=MaxAtt; i++)
	{
	  Acurrent[i] = (String *) calloc(MaxItem, sizeof(String));
	}
 
	for (j=0; j<= MaxItem-1; j++)
       {  
	  for (i=0; i <= MaxAtt; i++)
	  {
	   Acurrent[i][j] = CopyString(ReadValue[j*(MaxAtt+2) + i]);
	  }
	   Dval[j] = CopyString(ReadValue[j*(MaxAtt+2)+MaxAtt+1]);
        }
	MarkD = false;
	
/* Judge whether decision and attribute values are continuous  */
 
	free(ReadValue);
	Mark = (Boolean *) calloc(MaxAtt+1, sizeof(Boolean));
 
	for (i=0; i <= MaxAtt; i++)
	{
	  for (j=0; j <= MaxItem-1; j++)
	  {
	   Av = strtod(Acurrent[i][j], &endname);
	   if (endname == Acurrent[i][j] || *endname !='\0')
	   {
	   Mark[i] = false;
	   goto end1;
	   }
	  }
	  Mark[i] = true;
	  end1:;
	}
 
	/*for (j=0; j <= MaxItem-1; j++)
	{
	  Av = strtod(Dval[j],&endname);
	  if (endname == Dval[j] || *endname !='\0')
	  {
	        MarkD = false;
	 	goto end;
	  }
	}
	MarkD = true;
	end: ;*/
     }

/*************************************************************************/
/*												 */
/*			Error messages							 */
/*												 */
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
 
 
/**********************************************************************/
/*  THIS IS FOR WRITING OUT converted value names and data files*/
/**********************************************************************/
 
void writeout(void)
{
  int i,j,k;
  int NoA,*VFA;
  String *Darrange;
  float *VFD;
  float av;
  float *A,*Aarrange;
  FILE *fp1, *fp2;
  char Fn1[100], Fn2[100];
  char *outfile;
  char *sp;
  char *endname;
  char *cc,*cc1;
  printf("\n"); 
  putchar(007);
  putchar(007);
  outfile = (char *) calloc(100,sizeof(char));
  cc = (char *) malloc(100,sizeof(char));
  printf("\n Please enter your filestem name for storing values and names:\n");
  gets(cc);
  /*printf("\n%d\n",strlen(outfile));*/
  if (!strcmp(gets(cc), ""))
  {
   sp = (char *) calloc(100,sizeof(char));
   i=0;
   while (i <= strlen(filename)-1 && filename[i] !='.')
   {
	sp[i]=filename[i];
	i++;
   }
   strcpy(outfile,sp);
  }
  else
  {
    outfile= CopyString(cc);
  }
  strcpy(filestemN, outfile);
  strcat(filestemN,".names");
  strcpy(filestemV, outfile);
  strcat(filestemV,".data");
  /*printf("\n%s\n",filestemN);
  printf("\n%s\n",filestemV);*/
  strcpy(Fn1,filestemN);
  A = (float *) calloc(MaxItem, sizeof(float));
  VFA = (int *) calloc(MaxItem,sizeof(int));
  Aarrange = (float *) calloc(MaxItem,sizeof(float));
  VFD = (float *) calloc(MaxItem, sizeof(float));
  Darrange = (String *) calloc(MaxItem, sizeof(String));
  fp1=fopen(Fn1,"w");
  fprintf(fp1,"\n\t");
  if (MarkD)
  {
    for (j=0; j <= MaxItem-1; j++)
    {
	av = strtod(Dval[j], &endname);
        A[j] = av;
    }
    NoA = NoArrange(MaxItem, A, Aarrange,VFA);
    for (i=0; i <= NoA-2; i++)
    {
      fprintf(fp1,"%5.2f, ", Aarrange[i]);
    }
    fprintf(fp1,"%5.2f.", Aarrange[NoA-1]);
  }
  else if (!MarkD)
  {
    NoA = DArrange(MaxItem,Dval,Darrange,VFD);
    for (i=0; i <= NoA-2; i++)
    {
      fprintf(fp1,"%s, ", Darrange[i]);
    }
    fprintf(fp1,"%s.", Darrange[NoA-1]);
  }
  fprintf(fp1,"\n\n");
  for (i=0; i <= MaxAtt; i++)
  {
    fprintf(fp1,"\t%s: ", AttName[i]);
    if (Mark[i])
    {
	fprintf(fp1,"continuous.\n");
    }
    else if (!Mark[i])
    {
	NoA = DArrange(MaxItem,Acurrent[i],Darrange,VFD);
        for (k=0; k <= NoA-2; k++)
    	{
      	  fprintf(fp1,"%s, ", Darrange[k]);
    	}
      	fprintf(fp1,"%s.\n ", Darrange[NoA-1]);
    }
  }
  fclose(fp1);
  strcpy(Fn2,filestemV);
  fp2=fopen(Fn2,"w");
  for (j=0; j <= MaxItem-1; j++)
  {
    for (i=0; i <= MaxAtt; i++)
    {
	fprintf(fp2,"%s, ",Acurrent[i][j]);
   /*	if (Mark[i])
	{
	  av = strtod(Acurrent[i][j], &endname);
	  fprintf(fp2,"%5.2f, ",av);
	}
	else if (!Mark[i])
	{
	  fprintf(fp2,"%s, ",Acurrent[i][j]);
	}
  */
    }
    if (MarkD)
    {
	  av = strtod(Dval[j], &endname);
	  fprintf(fp2,"%5.2f. ",av);
    }
    else if (!MarkD)
    {
	  fprintf(fp2,"%s. ",Dval[j]);
    }
    fprintf(fp2,"\n");
  }
  fclose(fp2);
}
 
/*************************************************************/
/*								   		  */
/*     This is main routine						  */
/*									 	  */
/*************************************************************/
 
main()
{ 
  Boolean discrable;
  int i,j,k,No,*VF;
  int choice, choice1, choice2;
  char choice3[4];
  float *Buffer,*attval;
  putchar(007);
  putchar(007);
  printf("\n\n\n\n\n");
  printf("\t********************************************************\n");
  printf("\t*									*\n");
  printf("\t*	     WELCOME TO CONVERSION 0.1			*\n");
  printf("\t*									*\n");
  printf("\t*	THIS PROGRAM IS PRIMARILY DESIGEND 	*\n");
  printf("\t*		FOR A COURSE PROJECT  			*\n");
  printf("\t*		BY Mr LIPING LIU 				*\n");
  printf("\t*	AT UNIVERSITY OF KANSAS IN MARCH 1993.	*\n");
  printf("\t*	ILLEGAL REPRODUCTION IS PROHIBITED 	*\n");
  printf("\t*		BY THE COPYRIGHT.				*\n");
  printf("\t*									*\n");
  printf("\t*	FOLLOW MENU INSTRUCTIONS AND 		*\n");
  printf("\t*	ENJOY THIS SMART PROGRAM.			*\n"); 
  printf("\t*								 	*\n");
  printf("\t********************************************************\n");
 
  do
  {
    printf("\n\n\n");  
    printf("\n\t	MAIN CHOICE MENU\n\n");
    printf("\t 1. Start converting input data file\n");
    printf("\t 2. Renew converting another data file\n");
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
      printf("\n\t You now begin a new conversion\n");
      GetData();
      printf("\n\n\t%d items and %d attributes are read in\n", MaxItem, MaxAtt+1);
 /*     printf("\n\t The read in decision table is as follows:\n\n");
      for (j=0; j <= MaxAtt; j++)
      {
	printf("%s\t",AttName[j]);
      }
      printf("Decision\n");
      for (i=0;i<=MaxItem-1; i++)
      { 
        for (j=0; j<= MaxAtt; j++)
	{
	  printf("%s\t",Acurrent[j][i]);
	}
	printf("%s\n",Dval[i]);
      }
*/
      printf("\n\n\n");  
      writeout();
      printf("\n\t The value name file is in %s\n", filestemN);
      printf("\n\t The value data file is in %s\n", filestemV);
    }
  } while( choice != 3);
  exit(0);
}
