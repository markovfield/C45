/*************************************************************************/
/*									 */
/*  Main routine for constructing sets of production rules from trees	 */
/*  -----------------------------------------------------------------	 */
/*									 */
/*************************************************************************/
 
 
#include "defns.i"
#include "types.i"
 
 
	/*  External data.  Note: uncommented variables have the same meaning
	    as for decision trees (see extern.i)  */
 
short		MaxAtt, MaxClass, MaxDiscrVal;
 
ItemNo		MaxItem;
 
Description	*Item;
 
DiscrValue	*MaxAttVal;
 
char		*SpecialStatus;
 
String		*ClassName,
		*AttName,
		**AttValName,
		FileName = "DF";
 
short		VERBOSITY = 0,
		TRIALS;
 
Boolean		UNSEENS	  = false;
		SIGTEST	  = false;	/* use significance test in rule pruning */
 
float		SIGTHRESH   = 0.05,
		CF	    = 0.25,
		REDUNDANCY  = 1.0;	/* factor that guesstimates the
					   amount of redundancy and
					   irrelevance in the attributes */
 
PR		*Rule;			/* current rules */
 
RuleNo		NRules = 0,		/* number of current rules */
		*RuleIndex;		/* rule index */
 
short		RuleSpace = 0;		/* space allocated for rules */
 
ClassNo		DefaultClass;		/* current default class */
 
RuleSet		*PRSet;			/* sets of rulesets */
 
float		AttTestBits,		/* bits to encode tested att */
		*BranchBits;		/* ditto attribute value */
 
 
 
    main(Argc, Argv)
/*  ----  */
    int Argc;
    char *Argv[];
{
    int o;
    extern char *optarg;
    extern int optind;
    Boolean FirstTime=true;
 
    PrintHeader("rule generator");
 
    /*  Process options  */
 
    while ( (o = getopt(Argc, Argv, "f:uv:c:r:F:")) != EOF )
    {
	if ( FirstTime )
	{
	    printf("\n    Options:\n");
	    FirstTime = false;
	}
 
	switch (o)
	{
	    case 'f':	FileName = optarg;
			printf("\tFile stem <%s>\n", FileName);
			break;
	    case 'u':	UNSEENS = true;
			printf("\tRulesets evaluated on unseen cases\n");
			break;
	    case 'v':	VERBOSITY = atoi(optarg);
			printf("\tVerbosity level %d\n", VERBOSITY);
			break;
	    case 'c':	CF = atof(optarg);
			printf("\tPruning confidence level %g%%\n", CF);
			CF /= 100;
			break;
	    case 'r':	REDUNDANCY = atof(optarg);
			printf("\tRedundancy factor %g\n", REDUNDANCY);
			break;
	    case 'F':	SIGTHRESH = atof(optarg);
			printf("\tSignificance test in rule pruning, ");
			printf("threshold %g%%\n", SIGTHRESH);
		 	SIGTHRESH /= 100;
			SIGTEST = true;
			break;
	    case '?':	printf("unrecognised option\n");
			exit(1);
	}
    }
 
    /*  Initialise  */
 
    GetNames();
    GetData(".data");
    printf("\nRead %d cases (%d attributes) from %s\n",
	   MaxItem+1, MaxAtt+1, FileName);
 
    GenerateLogs();
 
    /*  Construct rules  */
 
    GenerateRules();
 
    /*  Save current ruleset  */
 
    SaveRules();
 
    /*  Evaluations  */
 
    printf("\n\nEvaluation on training data (%d items):\n", MaxItem+1);
    EvaluateRulesets(true);
 
    if ( UNSEENS )
    {
	GetData(".test");
	printf("\nEvaluation on test data (%d items):\n", MaxItem+1);
	EvaluateRulesets(false);
    }
}
