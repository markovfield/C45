/*************************************************************************/
/*									 */
/*	Process sets of rules						 */
/*	---------------------					         */
/*								         */
/*************************************************************************/
 
 
#include "defns.i"
#include "types.i"
#include "extern.i"
#include "rulex.i"
 
 
ItemNo	*ClassFreq,	/* ClassFreq[c]	= no. items of class c  */
	*Covered,	/* Covered[i]	= no. included rules that cover item i */
	*FalsePos,	/* FalsePos[c]	= no. false positives from rules
					  selected for class c */
	*NoRule,	/* NoRule[c]	= no. items covered by no selected rule */
 
	*Right,		/* Right[r]	= no. correct rule firings */
	*Wrong;		/* Wrong[r]	= no. incorrect rule firings */
 
float	*Value,		/* Value[r]	= advantage attributable to rule r or
					  realisable if rule r included */
	SubsetValue,	/* value of best class subset so far */
	CodeWeight;	/* multiplying factor for rule encodings */
 
Boolean	*RuleIn,	/* RuleIn[r]	= true if rule r included */
	*Subset,	/* best class subset so far */
	**Match;	/* Match[r][i]	= true if rule r fires on item i */
 
RuleNo	*ClassRules;	/* list of all rules for current target class */
 
 
 
/*************************************************************************/
/*									 */
/*  Construct an ordered subset (indexed by RuleIndex) of the current	 */
/*  set of rules							 */
/*									 */
/*************************************************************************/
 
 
    ConstructRuleset()
/*  ----------------  */
{
    RuleNo r, OldNRules = NRules;
    ItemNo i;
    ClassNo c;
 
    /*  Allocate tables  */
 
    Right = (ItemNo *) calloc(NRules+1, sizeof(ItemNo));
    Wrong = (ItemNo *) calloc(NRules+1, sizeof(ItemNo));
 
    Value = (float *) calloc(NRules+1, sizeof(float));
 
    RuleIn = (Boolean *) calloc(NRules+1, sizeof(Boolean));
    Subset = (Boolean *) malloc((NRules+1) * sizeof(Boolean));
 
    ClassRules = (RuleNo *) malloc((NRules+1) * sizeof(RuleNo));
 
    ClassFreq = (ItemNo *) calloc(MaxClass+1, sizeof(ItemNo));
 
    Covered = (ItemNo *) calloc(MaxItem+1, sizeof(ItemNo));
 
    Match = (Boolean **) calloc(NRules+1, sizeof(Boolean *));
 
    FalsePos = (ItemNo *) calloc(MaxClass+1, sizeof(ItemNo));
 
    NoRule = (ItemNo *) calloc(MaxClass+1, sizeof(ItemNo));
 
    ForEach(r, 1, NRules)
    {
	Match[r] = (Boolean *) calloc(MaxItem+1, sizeof(Boolean));
    }
 
    /*  Cover each class, then order the classes to give an index of rules  */
 
    InitialiseTables();
 
    FindRuleCodes();
    CodeWeight = 0.5;
 
    ForEach(c, 0, MaxClass)
    {
	CoverClass(c);
    }
 
    MakeIndex();
    FindDefault();
 
    /*  Clear  */
 
    cfree(Value);
    cfree(RuleIn);
    cfree(ClassRules);
    cfree(Subset);
    cfree(Covered);
    cfree(FalsePos);
    cfree(NoRule);
    ForEach(r, 1, OldNRules)
	cfree(Match[r]);
    cfree(Match);
}
 
 
 
/*************************************************************************/
/*									 */
/*		Initialise all tables used in sifting			 */
/*									 */
/*************************************************************************/
 
 
    InitialiseTables()
/*  ----------------  */
{
    ItemNo i;
    RuleNo r;
    ClassNo c;
    float Strength();
 
    ForEach(r, 1, NRules)
    {
	RuleIn[r] = false;
	Rule[r].Used = Rule[r].Incorrect = 0;
    }
 
    ForEach(c, 0, MaxClass)
    {
	ClassFreq[c] = 0;
    }
 
    ForEach(i, 0, MaxItem)
    {
	ClassFreq[Class(Item[i])]++;
 
	ClearOutcomes();
 
	ForEach(r, 1, NRules)
	{
	    Match[r][i] = Strength(Rule[r], Item[i]) > 0.3;
 
	    if ( Match[r][i] )
	    {
		Rule[r].Used++;
		if ( Class(Item[i]) != Rule[r].Rhs ) Rule[r].Incorrect++;
	    }
	}
    }
}
 
 
 
/*************************************************************************/
/*								         */
/*	Select a subset of the rules for class FocusClass	         */
/*								         */
/*************************************************************************/
 
 
    CoverClass(FocusClass)
/*  ----------  */
    ClassNo FocusClass;
{
    RuleNo r, RuleCount=0;
    ItemNo i;
 
    Verbosity(1)
	printf("\nClass %s\n-----\nAction  Change  Value",
		ClassName[FocusClass]);
 
    ForEach(i, 0, MaxItem)
    {
	Covered[i] = 0;
    }
 
    ForEach(r, 1, NRules)
    {
	if ( Rule[r].Rhs == FocusClass )
	{
	    RuleCount++;
	    ClassRules[RuleCount] = r;
	}
    }
 
    if ( ! RuleCount )
    {
	return;
    }
 
    SubsetValue = 1E10;
 
    if ( RuleCount <= 10 )
    {
	AllCombinations(RuleCount, FocusClass);
    }
    else
    {
	SimAnneal(RuleCount, FocusClass);
    }
 
    memcpy(RuleIn, Subset, NRules+1);
    Verbosity(1) printf("\n\tBest value %.1f\n", SubsetValue);
}
 
 
 
/*************************************************************************/
/*									 */
/*    Try all combinations of rules to find best value			 */
/*									 */
/*************************************************************************/
 
 
    AllCombinations(NR, FocusClass)
/*  ---------------  */
    RuleNo NR;
    ClassNo FocusClass;
{
    float ThisValue, CalculateValue();
    RuleNo r;
 
    if ( ! NR )
    {
	ThisValue = CalculateValue(FocusClass);
	if ( ThisValue < SubsetValue )
	{
	    SubsetValue = ThisValue;
	    memcpy(Subset, RuleIn, NRules+1);
	}
    }
    else
    {
	r = ClassRules[NR];
 
	AllCombinations(NR-1, FocusClass);
 
	AddRule(r);
	AllCombinations(NR-1, FocusClass);
 
	DeleteRule(r);
	Verbosity(1) printf("\n");
    }
}
 
 
 
/*************************************************************************/
/*									 */
/*  Find a good subset by simulated annealing				 */
/*									 */
/*************************************************************************/
 
 
    SimAnneal(RuleCount, FocusClass)
/*  ---------  */
    RuleNo RuleCount;
    ClassNo FocusClass;
{
    RuleNo r, OutCount;
    ItemNo i;
    short ri, Tries;
    float Temp, Delta, ThisValue, CalculateValue();
    Boolean Changed;
 
    /*  Keep dropping and adding rules until can't improve  */
 
    for ( Temp = 1000 ; Temp > 0.001 ; Temp *= 0.95 )
    {
	ThisValue = CalculateValue(FocusClass);
	if ( ThisValue < SubsetValue )
	{
	    SubsetValue = ThisValue;
	    memcpy(Subset, RuleIn, NRules+1);
	}
 
	Verbosity(2)
	{
	    OutCount = 0;
 
	    ForEach(ri, 1, RuleCount)
	    {
		r = ClassRules[ri];
 
		if ( ! RuleIn[r] )
		{
		    if ( ! (OutCount++ % 3) ) printf("\n\t\t");
		    printf("%d<%d|%d=%.1f> ", r, Right[r], Wrong[r], Value[r]);
		}
	    }
 
	    printf("\n\n");
	}
 
	Changed = false;
 
	for ( Tries = 100 ; ! Changed && Tries > 0 ; Tries-- )
	{
	    /*  Choose a rule to add or delete  */
 
	    ri = RuleCount * Random + 1;
 
	    r = ClassRules[ri];
 
	    Delta = ( RuleIn[r] ? -Value[r] : Value[r] );
 
	    if ( Delta > 0 || Random < exp(Delta / Temp) )
	    {
		if ( RuleIn[r] )
		{
		    DeleteRule(r);
		}
		else
		{
		    AddRule(r);
		}
		
		Changed = true;
	    }
	}
 
	if ( ! Changed ) break;
    }
}
 
 
 
/*************************************************************************/
/*								         */
/*  Find the number of correct and incorrect rule firings for rules      */
/*  for class FocusClass and hence determine the Value of the rules      */
/*								         */
/*************************************************************************/
 
 
float CalculateValue(FocusClass)
/*    --------------  */
    ClassNo FocusClass;
{
    RuleNo r, Selected=0, InCount;
    ItemNo i, Times, FPos=0, FNeg=0, SumUsed=0, SumCover=0;
    float SumErrors=0, BaseBits, RuleBits=0, NewBits, ExceptionBits();
    ClassNo ThisClass;
 
    /*  Initialise Right and Wrong  */
 
    ForEach(r, 1, NRules)
    {
	if ( Rule[r].Rhs == FocusClass )
	{
	    Right[r] = Wrong[r] = 0;
 
	    if ( RuleIn[r] )
	    {
		SumUsed += Rule[r].Used;
		SumErrors += Rule[r].Error * Rule[r].Used;
		RuleBits += Rule[r].Bits;
		Selected++;
	    }
	}
    }
 
    RuleBits -= LogFact[Selected];	/* allow for reordering of rules */
 
    /*  For each rule for the FocusClass, set Right to the number
	of items that correctly fire the rule and aren't covered by
	any other included rules, and Wrong to the number of items
	that incorrectly do so.  */
 
    ForEach(i, 0, MaxItem)
    {
	ThisClass = Class(Item[i]);
 
	if ( Times = Covered[i] )
	{
	    SumCover++;
	    if( ThisClass != FocusClass ) FPos++;
	}
	else
	if ( ThisClass == FocusClass )
	{
	    FNeg++;
	}
 
	ForEach(r, 1, NRules)
	{
	    if ( Rule[r].Rhs == FocusClass &&
		 Match[r][i] &&
		 ( ! Times || Times == 1 && RuleIn[r] ) )
	    {
		if ( ThisClass == FocusClass )
		{
		    Right[r]++;
		}
	  	else
		{
		    Wrong[r]++;
		}
	    }
	}
    }
 
    BaseBits = CodeWeight * RuleBits + ExceptionBits(SumCover, FPos, FNeg);
 
    /*  From the Right and Wrong of each rule, calculate its Value  */
 
    Verbosity(1)
    {
        printf("\t");
    	InCount = -1;
    }
 
    ForEach(r, 1, NRules)
    {
	if ( Rule[r].Rhs == FocusClass )
	{
	    if ( RuleIn[r] )
	    {
		NewBits = ExceptionBits(SumCover-Right[r]-Wrong[r],
					FPos-Wrong[r], FNeg+Right[r]) +
			  CodeWeight *
			      (RuleBits - Rule[r].Bits + LogItemNo[Selected]);
		Value[r] = NewBits - BaseBits;
	    }
	    else
	    {
		NewBits = ExceptionBits(SumCover+Right[r]+Wrong[r],
					FPos+Wrong[r], FNeg-Right[r]) +
			  CodeWeight *
			      (RuleBits + Rule[r].Bits - LogItemNo[Selected+1]);
		Value[r] = BaseBits - NewBits;
	    }
 
	    Verbosity(1)
	    {
	        if ( RuleIn[r] )
	        {
		    if ( ++InCount && ! (InCount % 3) ) printf("\n\t\t");
		    printf("%d[%d|%d=%.1f]  ", r, Right[r], Wrong[r], Value[r]);
	        }
	    }
 
 
	}
    }
 
    Verbosity(1)
    {
	printf("\n\t\t%d rules, %d firings: F+=%d, F-=%d, %.1f bits (rules=%.1f)\n",
		Selected, SumCover, FPos, FNeg, BaseBits, RuleBits);
    }
 
    return BaseBits;
}
 
 
 
/*************************************************************************/
/*								         */
/*  Add rule r to the set of included rules and increase the number of	 */
/*  rules covering each of the items that fire the rule		         */
/*								         */
/*************************************************************************/
 
 
    AddRule(r)
/*  -------  */
    RuleNo r;
{
    ItemNo i;
 
    RuleIn[r] = true;
 
    ForEach(i, 0, MaxItem)
    {
	if ( Match[r][i] )
	{
	    Covered[i]++;
	}
    }
 
    Verbosity(1) printf("%5d+  %6.1f", r, Value[r]);
}
 
 
 
/*************************************************************************/
/*								         */
/*  Delete rule r from the included rules and decrease the number of	 */
/*  rules covering each of the items covered by the rule	         */
/*								         */
/*************************************************************************/
 
 
    DeleteRule(r)
/*  ----------  */
    RuleNo r;
{
    ItemNo i;
 
    RuleIn[r] = false;
 
    ForEach(i, 0, MaxItem)
    {
	if ( Match[r][i] )
	{
	    Covered[i]--;
	}
    }
 
    Verbosity(1) printf("%5d-  %6.1f", r, -Value[r]);
}
 
 
 
/*************************************************************************/
/*								         */
/*  Make an index of included rules in RuleIndex.  Select first those    */
/*  classes whose rules have the fewest false positives.  Within a	 */
/*  class, put rules with higher accuracy ahead.			 */
/*								         */
/*************************************************************************/
 
 
    MakeIndex()
/*  ---------  */
{
    ClassNo c, BestC, Pass;
    RuleNo r, BestR, NewNRules = 0;
    ItemNo i;
    Boolean *Included;
 
    Included = (Boolean *) calloc(MaxClass+1, sizeof(Boolean));
    RuleIndex = (RuleNo *) calloc(NRules+1, sizeof(RuleNo));
 
    Verbosity(1) printf("\nFalsePos  Class\n");
 
    ForEach(i, 0, MaxItem)
    {
	Covered[i] = 0;
    }
 
    /*  Select the best class to put next  */
 
    ForEach(Pass, 0, MaxClass)
    {
	ForEach(c, 0, MaxClass)
	{
	    if ( Included[c] ) continue;
 
	    FalsePos[c] = 0;
 
	    ForEach(i, 0, MaxItem)
	    {
		if ( Covered[i] || Class(Item[i]) == c ) continue;
 
		ForEach(r, 1, NRules)
		{
		    if ( Rule[r].Rhs == c && RuleIn[r] && Match[r][i] )
		    {
			FalsePos[c]++;
			break;
		    }
		}
	    }
	}
 
	BestC = -1;
	ForEach(c, 0, MaxClass)
	{
	    if ( ! Included[c] &&
	         ( BestC < 0 || FalsePos[c] < FalsePos[BestC] ) )
	    {
		BestC = c;
	    }
	}
	Included[BestC] = true;
 
	Verbosity(1)
	    printf("%5d     %s\n", FalsePos[BestC], ClassName[BestC]);
 
	/*  Now grab the rules for this class  */
 
	do
	{
	    BestR = 0;
 
	    /*  Find the best rule to put next  */
 
	    ForEach(r, 1, NRules)
	    {
		if ( RuleIn[r] && Rule[r].Rhs == BestC &&
		     ( ! BestR || Rule[r].Error < Rule[BestR].Error ) )
		{
		    BestR = r;
		}
	    }
 
	    if ( BestR )
	    {
		RuleIndex[++NewNRules] = BestR;
		RuleIn[BestR] = false;
 
		ForEach(i, 0, MaxItem)
		{
		    Covered[i] |= Match[BestR][i];
		}
	    }
	} while ( BestR );
    }
 
    NRules = NewNRules;
    cfree(Included);
}
 
 
 
/*************************************************************************/
/*								         */
/*  Find the default class as the one with most items not covered by	 */
/*  any rule.  Resolve ties in favour of more frequent classes.		 */
/*  (Note: Covered has been set by MakeIndex.)				 */
/*								         */
/*************************************************************************/
 
 
    FindDefault()
/*  -----------  */
{
    ClassNo c;
    ItemNo i;
    RuleNo r;
 
    /*  Determine uncovered items  */
 
    ForEach(c, 0, MaxClass)
    {
	NoRule[c] = 0;
    }
 
    ForEach(i, 0, MaxItem)
    {
	if ( ! Covered[i] )
	{
	    NoRule[Class(Item[i])]++;
	}
    }
 
    Verbosity(1)
    {
	printf("\nItems: Uncovered   Class\n");
	ForEach(c, 0, MaxClass)
	{
	    printf("%5d %7d      %s\n", ClassFreq[c], NoRule[c], ClassName[c]);
	}
	printf("\n");
    }
 
    DefaultClass = 0;
    ForEach(c, 1, MaxClass)
    {
	if ( NoRule[c] > NoRule[DefaultClass] ||
	     NoRule[c] == NoRule[DefaultClass] &&
	     ClassFreq[c] > ClassFreq[DefaultClass] )
	{
	    DefaultClass = c;
	}
    }
}
 
 
 
/*************************************************************************/
/*								         */
/*  Given a rule and a case (ClearOutcomes performed), determine	 */
/*  the strength with which we can conclude that the case belongs	 */
/*  to the class specified by the rule's right-hand side.	         */
/*								         */
/*  If the case doesn't satisfy all the conditions of the rule,		 */
/*  then this is 0.						         */
/*								         */
/*************************************************************************/
 
 
float Strength(ThisRule, Case)
/*    --------  */
    PR ThisRule;
    Description Case;
{
    short d;
    Boolean Satisfies();
 
    if ( ThisRule.Error > 0.7 ) return 0.0;
 
    ForEach(d, 1, ThisRule.Size)
    {
	if ( ! Satisfies(Case, ThisRule.Lhs[d]) )
	{
	    return 0.0;
	}
    }
 
    return ( 1 - ThisRule.Error );
}
 
 
 
/*************************************************************************/
/*									 */
/*  Determine the number of bits to encode exceptions: FP false +ves	 */
/*  out of Fires firings, and FN false negatives in the remaining items	 */
/*									 */
/*************************************************************************/
 
 
#define nCrBits(E,N)  (LogFact[N] - (LogFact[E] + LogFact[N-(E)]))
 
 
float ExceptionBits(Fires, FP, FN)
/*    -------------  */
    int Fires, FP, FN;
{
    return nCrBits(FP, Fires) +  nCrBits(FN, MaxItem+1-Fires);
}
 
 
 
/*************************************************************************/
/*									 */
/*  Find encoding lengths for all rules					 */
/*									 */
/*************************************************************************/
 
 
    FindRuleCodes()
/*  -------------  */
{
    RuleNo r;
    short d, NCond;
    float Bits, CondBits();
 
    ForEach(r, 1, NRules)
    {
	NCond = Rule[r].Size;
	Bits = 0;
 
	ForEach(d, 1, NCond)
	{
	    Bits += CondBits(Rule[r].Lhs[d]);
	}
 
	/*  Must encode the number of conditions, but credit the total
	    encoding by the ways conditions can be reordered  */
 
	Rule[r].Bits = Bits + LogItemNo[NCond] - LogFact[NCond];
    }
}
 
 
 
/*************************************************************************/
/*									 */
/*  Determine the number of bits required to encode a condition		 */
/*									 */
/*************************************************************************/
 
 
float CondBits(C)
/*    --------  */
    Condition C;
{
    Test t;
    Attribute a;
 
    t = C->CondTest;
    a = t->Tested;
 
    switch ( t->NodeType )
    {
	case BrDiscr:		/* test of discrete attribute */
	case ThreshContin:	/* test of continuous attribute */
 
	    return AttTestBits/REDUNDANCY + BranchBits[a];
 
	case BrSubset:		/* subset test on discrete attribute  */
 
	    return AttTestBits/REDUNDANCY + MaxAttVal[a];
    } 
}
