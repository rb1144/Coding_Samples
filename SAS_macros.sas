/*
br 2011-06-15
I finally got fed up with maintaining a bunch of SAS files, each containing one single
macro, and concatenated them into a single file. This cleans things up a bit, and makes
it easier to ensure that the newest versions are used in production and potentially
shared with others, without keeping track of the time stamps for a series of files, and
without having to overwrite files one by one.

Table of contents:
%macro addall(lib, pattern, by, out);
%macro addpct(dset, col);
%macro addrank(dset,out,var,rank,groups=10, dir=descending);
%macro addWeek(dset, date);
%macro anyword(dset, var, words, flag, fun=indexw);
%macro appendnew(master, dset, date, overwrite=N);
%macro bankHoliday(date);
%macro chartonum(dset, out, informat=20.);
%macro collapselevels(dataset,tvar,cvar,use=R);
%macro collapsevars(dataset,tvar,collvars);
%macro coltomacvars(dset,col,distinct=NO);
%macro columns(dset, out=temp_tb_cont, ord=varnum, incl=, excl=);
%macro columns_schema(lib, out);
%macro combineds(out, lib, pattern, by=, fname=N, keep=, report=Y);
%macro csvimp(file, dset, guess=32000);
%macro csvpct(in,out);
%macro cut(dset, out, var, newvar, breaks);
%macro cwords(w);
%macro dailyTablesUpdate(minutes, extra=Y);
%macro dbcont(lib);
%macro dbcontSS(lib, server, db);
%macro dedupe(dset, by, keep, out);
%macro delglobal;
%macro diff(tab1,tab2,by1,by2,out);
%macro distinct(dset, var);
%macro dnull(dset,vars,num);
%macro dt2d(dset, fields);
%macro dtime2dt(dt);
%macro eom(mth, var);
%macro explore(dset, sortby=freq, folder=none, num=N);
%macro export(dset, fname, fld=\\NYCB\corp$\DBMarketing\Bogdan\data, csv=N, zip=N);
%macro extremeobs(dset, var, rows=20);
%macro finddup(table, var, print=50, short=N);
%macro freq(ds, vars, ttl=N, order=);
%macro gcdf(lat1, lon1, lat2, lon2);
%macro geodist(dset1,dset2,lat1,lon1,lat2,lon2,id1);
%macro getLatestFile(fld, prefix, fmt, suffix, daysback=360);
%macro gfreq(dset, var);
%macro goback(date, N, period, place, var, fmt=date9., adj=0);
%macro gplot(dset, by, vars, log=, labels=N, ylab=, range=Y, rangeLab=, join=Y,
%macro head(dset,r=50);
%macro holidays;
%macro import(file, dset, csv=Y, fld=\\NYCB\corp$\DBMarketing\Bogdan\data, guess=32767);
%macro importDlmAsChar(file, fld, out, names=yes, guess=max, dlm=",");
%macro importWideCharFields(file, dset, codeFld=\\NYCB\corp$\DBMarketing\Bogdan\out, csv=Y);
%macro innerjoin(tab1,tab2,by1,by2,out);
%macro jobinfo;
%macro libcompress(lib,compression=CHAR);
%macro libcont(lib, out=, message=, like=);
%macro locf(dset, var, out, BY=);
%macro loopingvalues(dset, var);
%macro lst(list, name, log=Y);
%macro mfreq(dset, vars, label=Count, ttl=, order=, bal=, N=, where=, pct=Y);
%macro missinfo(dsname, out);
%macro missing(dsname, out=MissingCounts);
%macro missingsql(dsname);
%macro missperobs(dsname);
%macro movingavg(dset, out, by, var, lag, ema, type=SIM);
%macro newRunFile(jobID, fld=\\&SASserver.\BI\0.CommonFiles\BI_code\recurring_tasks\job_monitor);
%macro nolabels(dset);
%macro now(message, out=N);
%macro OracleAllColumns(out);
%macro OracleConn(tb);
%macro OracleTOC(schema, conn=);
%macro OracleTOC(schema, conn=);
%macro pr(dset, r=, ttl=);
%macro qpl(dset, var, log=);
%macro quarterDate(dset, date);
%macro randomSample(dset, N, out);
%macro range(dset, var, ttl=);
%macro rangerecode(dset, vars, sep, del);
%macro replacevalue(dsname, out, type, replace, with, except=(''));
%macro rowCounts(tb, columns, out=rep, order=, where=);
%macro sampleimpute(dset);
%macro saslogtime(logfile,out=);
%macro sendEmail(from, sender, to, subj, body, cc=, att=, append=@flagstar.com, send=Y);
%macro sendemail(from, to, subject, body, attach, logfile);
%macro sendHTMLemail(from, sender, to, subj, body, dset, body2=, dset2=, cc=, att=, append=@flagstar.com);
%macro shadedRep(tb, BY, excl=, columns=, hC1=, hC2=, hC3=, hC4=, hC5=, hC6=, hC7=, rowShadeField=, rowShadeValue=, rowBoldField=, rowBoldValue=);
%macro sink(file, fld=\\NYCB\corp$\DBMarketing\Bogdan\out, style=Y, stylename=normal);
%macro sleep(minutes);
%macro sleepUntil(time);
%macro sqlstring(words, vname=sqlstring, quotes=Y);
%macro stack(ds, out);
%macro stats(dset, var, by=, print=Y, tall=Y, ttl=, fmt=, subset=, wait=0.03);
%macro tail(dset,r=20, ttl=);
%macro timenowlog(message);
%macro toExcel(filename, sheet=Report, opt=);
%macro top(tb, N=200, cols=);
%macro transp(dset, by, id, vars, out, reverse=N, label=Y);
%macro ts(dset, date, dtime=N, period=, v=count(*), vlab=freq, ttl=, plot=Y, fmt=);
%macro univ(dset,vars);
%macro varclus(dset, vars, prefix, coef, scored, folder);
%macro variableclass(dset, var);
%macro varInfo(dset, v);
*/

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2009-03*/
/*
o Given a library, a pattern and a variable name, this macro stacks all the datasets whose
  names match the given pattern, and adds up all numerics by the variable provided.
o Purpose: compute cumulative figures given a series of weekly reports.
o Arguments:
  - lib = the library where the weekly reports are saved
  - pattern = common string in each file name
  - by = the grouping variable (say Region)
  - out = the name of the output dataset
*/

%macro addall(lib, pattern, by, out);
%combineds(dsetalltemp, &lib, &pattern, fname=Y, report=N)  /*stack all datasets*/
proc sql noprint;
	/*all numeric variables ...*/
	create table numvars_all as
	select libname, memname, memtype, name, type, varnum, label
	from dictionary.columns
	where libname="WORK" 
		and memname="DSETALLTEMP" 
		and type="num"
		and upcase(name) not in (%upcase("&by"))
	order by varnum;
	/*... into macro variables*/
	select trim(left(put(count(*),8.))) into :nvars from numvars_all;
	select name into :varname1-:varname&nvars from numvars_all;
quit;
/*add everything up*/
%local i;
proc sql;
	create table &out as
	select &by, 
		%do i = 1 %to %eval(&nvars - 1); sum(&&varname&i) as &&varname&i, %end; 
		sum(&&varname&nvars) as &&varname&nvars
	from dsetalltemp
	group by &by
	order by &by;
quit;
proc sql;
	drop table dsetalltemp;
	drop table numvars_all;
quit;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2006-04*/
/*
o Given a dataset and the name of a numeric variable, this macro adds 
  a variable with the % distribution of that variable.
o Usage:
  %addpct(mytable, Households)
*/

%macro addpct(dset, col);
proc sql noprint;
	select sum(&col) into :total
	from &dset;
quit;
data &dset;
	set &dset;
	Pct&col = &col / &total;
	format Pct&col percent8.2;
run;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2006-09*/
/*
o This macro saves some typing when computing the rank for a given modeled probability.
o Arguments:
  - dset/out = the input/output data set
  - var = the variable (predicted probability) for which the rank is computed
  - rank = the name of the computed rank variable
  - groups = the number of ranks to compute
o NB: the ranks are assigned by DESCENDING values. The output is intended to be used for classifing 
  customers (their predicted scores) into deciles/ranks. For example, rank 1 indicates higher 
  propensities (probabilities of response) than rank 6.
o Usage:
%addrank(list1,list2,probability,Rank)
%addrank(list1,list2,probability,Rank,groups=20)
*/

%macro addrank(dset,out,var,rank,groups=10, dir=descending);
proc rank data = &dset out = &out &dir groups = &groups;
	var &var;
	ranks &rank;
	/*label &rank = ' ';*/
run;
data &out;
	set &out;
	&rank = &rank + 1;
run;
%mend;

/*----------------------------------------------------------------------------------*/

*br 2018-03;
*This macro saves some typing when adding the Week Starting to a given date field;

%macro addWeek(dset, date);
data &dset;
	set &dset;
	*Move all dates to the next Mon; 
	dow = put(&date, downame3.); 
	if dow = 'Mon' then WeekEnding = &date; 
		else if dow = 'Tue' then WeekEnding = &date + 6; 
		else if dow = 'Wed' then WeekEnding = &date + 5; 
		else if dow = 'Thu' then WeekEnding = &date + 4; 
		else if dow = 'Fri' then WeekEnding = &date + 3; 
		else if dow = 'Sat' then WeekEnding = &date + 2; 
		else if dow = 'Sun' then WeekEnding = &date + 1; 
	format WeekEnding date9.; 
	drop dow;
run;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2013-06*/
/*
o This macro is meant to save some typing. It generates a sum using a function such as indexw
  which will be greater than 0 if any of the words given is found. If any word is found, the
  flag will have the value 'Y'.
o Note that the comparisons are case-insensitive.
o Usage:
  %anyword(mcode2, PRODUCT_DESC, ESCROW INTERNAL MUNICIPAL IOLTA LANDLORD, Exclude)
*/

%macro anyword(dset, var, words, flag, fun=indexw);
%lst(&words, word)
%local i;
data &dset;
	set &dset; 
		if %do i = 1 %to &nw; &fun.(lowcase(&var), %trim(lowcase("&&word&i"))) + %end; 0 > 0 then &flag = 'Y';
			else &flag = 'N';
run;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2012-11*/
/*
o This macro saves some typing when appending new data to a given table.
o To make sure that the same data is not appended more than once, a given
  date field is requested and used to keep only the new rows beyond the
  max date in the master table.
o 2015-10: Added a new argument, overwrite, which allows the new data to overwrite
  older data from the master table. The issue is that in some cases older data
  needs to be revised.
*/

%macro appendnew(master, dset, date, overwrite=N);
%if &overwrite = N %then %do;
proc sql noprint;
	*NB: it is important to NOT format this as a date, because that allows relying on
	regular numeric values in the &date field. Just saw a case where the conversion to date
	could have produced incorrect results because the so called date column was in fact
	in YYYYMM format, which was literally interpreted as a date, 23NOV2510...;
	select max(&date) into :upto
	from &master;
quit;
data tempappend;
	set &dset;
	where &date > &upto;
run;
proc append base=&master data=tempappend force; run;
%end;
%else %do;
*overwrite older data with new data;
/*2019-07: Replaced the one-step SQL with two steps, to get away from these Warnings in the log:
WARNING: This CREATE TABLE statement recursively references the target table. 
A consequence of this is a possible data integrity problem. */
/*
proc sql;
	create table &master as
	select * from &master where &date not in (select &date from &dset)
	union all
	select * from &dset;
quit;
*/
proc sql;
	delete 
	from &master 
	where &date in (select &date from &dset);
quit;
proc append base=&master data=&dset force; run;
%end;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2013-02*/
/*
o This macro provides a quick way of checking whether a given date is a bank holiday.
o The output is a global macro variable with the value Y or N, which can be used
  in subsequent logic.
o Usage: %bankHoliday(18FEB2013)
*/

%macro bankHoliday(date);
%global bankHoliday;
%holidays
proc sql noprint;
	select count(*) into :holi
	from holidays
	where HolidayDate = "&date"d;
quit;
%if &holi = 1 %then %do;
	%let bankHoliday = Y;
%end;
%else %do;
	%let bankHoliday = N;
%end;
%put Created the global variable bankHoliday = &bankHoliday..;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2007-12*/
/*
o This macro examines all character variables from a given dataset and determines which
  ones contain numeric values. The purpose is to help convert to numeric the variables 
  received from Equifax, where appropriate. A very large number of variables (~2800) need 
  to be scanned quickly and efficiently.
o Figuring out which values represent valid numbers is trickier than simply determining
  whether any characters are present. For example the values '10 24', '15-20', '153,32' 
  contain only characters that are valid in number fields, but do not actually represent
  numbers. To get around these issues, use the logic implemented behind the input()
  function and keep track of the conversion errors.
o For efficiency, loop through the values of each variable, then as soon as an input error 
  occurrs, flag the variable as non-numeric and go to the next var.
o 2012-04: It turns out that the %abort statements were causing some real issues (error
  messages followed by terminating all other statements, even chunks of code in a macro
  that was hosting a %chartonum call. This is unacceptable, and at the same time I didn't
  find a sane way of terminating the execution of a single macro, so I gave up on the
  %abort and nested everything into huge %if %then %else constructs. 
o Arguments:
  - dset = the name of the data set
  - out = the output data set; leave blank if the intention is to get the conversion syntax
  - informat = the informat to use with input() to determine whether the values are numeric
o Output:
  - a data set with appropriate character vars converted to numeric; or generated SAS code
    for the conversion.
*/

%macro chartonum(dset, out, informat=20.);
%local lib dsname isnumeric;
/*---get the names of the library and data set*/
%if %index(&dset,.)=0 %then %do; 
	%let lib=WORK;
	%let dsname=%upcase(&dset);
%end;
%else %do; 
	%let lib=%upcase(%scan(&dset,1,"."));
	%let dsname=%upcase(%scan(&dset,2,"."));
%end;
proc sql noprint;
	/*the character column names ...*/
	create table vars as
	select name, type
	from dictionary.columns
	where libname = "%upcase(&lib)" and memname = "%upcase(&dsname)" and upcase(type) = 'CHAR';
	/*... into macro variables*/
	select trim(left(put(count(*),8.))) into :nvars from vars;
	select name into :v1-:v&nvars from vars;
quit;
%if &nvars = 0 %then %do;
	data _null_;
		title;
		file print;
		put "No character variables were found in &dset .";
	run;
	data &out; set &dset; run;
	/*%abort;*/
%end;
%else %do;
/*---determine which character vars can be converted to numeric*/
data toconvert;
	length convert $ 50;
	set _null_;
run;
%local i;
%do i = 1 %to &nvars;
	%let isnumeric = TRUE;
	data _null_;
		set &dset (keep = &&v&i);
		numtest = input(&&v&i, &informat);
		if _ERROR_ = 1 then do; 
			call symputx('isnumeric', 'FALSE');
			stop;
		end;
	run;
	%if &isnumeric = TRUE %then %do;
		data add;
			length convert $ 50;
			convert = "&&v&i";
			vnum = &i;
		run;
		data toconvert;
			set toconvert add;
		run;
	%end;
%end;
/*---if no character variables can be converted, abort*/
proc sql noprint;
	select trim(left(put(count(*),8.))) into :nvars from toconvert;
quit;
%if &nvars = 0 %then %do;
	data _null_;
		title;
		file print;
		put "Checked all character variables in &dset., but did not find any to convert to numeric.";
	run;
	data &out; set &dset; run;
	/*%abort;*/
%end;
%else %do;
/*continue with the conversion*/
proc sql noprint;
	select convert into :c1-:c&nvars from toconvert;
quit;
%if &nvars = 0 %then %do;
	data _null_;
		title;
		file print;
		put "No character variables containing valid numeric values were found in &dset .";
	run;
	%abort;
%end;
/*---print the conversion syntax*/
%if &out eq %then %do;
	data _null_;
		title;
		file print;
		put '===Output data set not specified; here is the generated DATA STEP code===';
	run;
	%do i = 1 %to &nvars;
		data _null_;
			title;
			file print;
			put "tempvar&i = input(&&c&i, &informat);";
			put "drop &&c&i;";
			put "rename tempvar&i = &&c&i;";
		run;
	%end;
%end;
/*---or do the conversion*/
%if &out ne %then %do;
	data &out;
		set &dset;
		%do i = 1 %to &nvars;
			tempvar&i = input(&&c&i, &informat);
			drop &&c&i;
			rename tempvar&i = &&c&i;
		%end;
	run;
%end;

%end;  /*end of char vars to convert check*/
%end;  /*end of character variable presence check*/

%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2003-02*/
/*
o This macro collapses the levels of a categorical variable using Greenacre's method
  ('Clustering Rows and Columns of a Contingency Table' 1988, 'Correspondence Analysis
  in Practice' 1993). The intention is to reconfigure a predictor to maximize its predictive
  ability (= cheating: fine for predictive analytics but not for biostatistics).
o The algorithm below (PROC CLUSTER with method=WARD on the frequency table from PROC FREQ)
  is not in fact Greenacre's method but produces identical results.
o The levels are clustered based on the significance of the relationship between the target 
  and categorical variable (as given by the chi-square statistic). At each step, the two levels
  that give the least reduction in chi-square statistic are merged.
o Arguments:
  - tvar = the target
  - cvar = the categorical variable with levels to be clustered
  - use = where the collapse information will be used (R or SAS); in R, K-1 dummies are automatically
    generated for an unordered factor with K categories, so to keep things simple only one variable
    will be created; in SAS (at least for PROC LOGISTIC with some options the last time I checked)
    dummies have to be added manually (ugh!)
o Usage:
%collapselevels(mytable,Responded,clustername)

bogdan romocea 2006-10
o In light of the new vars from Claritas and Equifax (+ their values and the number of clusters
  in the final solutions), automated the generation of the DATA STEP code for new variable creation.
  The syntax varies depending on whether the model will be fit in R (highly recommended) or SAS.
*/

%macro collapselevels(dataset,tvar,cvar,use=R);
/*First, a data set that contains the number of cases and the proportion of the target 
variable by each level is created.*/
proc means data=&dataset noprint nway;
	class &cvar;
	var &tvar;
	output out=levels mean=prop;
run;
proc print data=levels;
	title "Levels of &cvar to collapse";
run;
title;
/*method=ward collapses categorical levels based on chi-square statistic reduction.
RSquared measures the proportion of chi-square remaining in the contingency table 
after each level collapse.
At each step, the levels that give the smallest decrease in chi-square are collapsed.
Semipartial RSq shows the % reduction in chi-square after each category collapse.
outtree creates a data set than can be used by the tree procedure to draw a dendogram*/
ods listing close;
ods output clusterhistory=cluster;
proc cluster data=levels method=ward outtree=fortree;
	freq _freq_;
	var prop;
	id &cvar;
run;
ods listing;
/*proc print data=cluster;run;*/
/*This computes the chi-square from the whole contingency table (before any collapse)*/
proc freq data=&dataset noprint;
	tables &cvar*&tvar / chisq;
	output out=chi(keep=_pchi_) chisq;
run;
/*proc print data=chi;run;*/
/*To compute the optimal number of clusters, the chi-square statistic and the associated 
p-value needs to be computed for each collapsed contingency table. This can be obtained by 
multiplying the chi-square from the whole table (above) with the proportion of chi-square 
remaining after the levels are collapsed.*/

/*This data step computes the chi-square statistic for each collapsed contingency table.
The _n_ variable is used to put the overall chi-square value in each observation for the 
data set cutoff. logsdf computes the log of the probability that an observation from a 
specified distribution is greater than or equal to a specified value. The arguments for 
logsdf are the specified distribution in quotes, the numeric random variable and the degrees 
of freedom. The log of the p-value is calculated in order to produce a more visually 
appealing graph.*/
data cutoff;
	if _n_ = 1 then set chi;
	set cluster;
	chisquare=_pchi_*rsquared;
	degfree=numberofclusters-1;
	logpvalue=logsdf('CHISQ',chisquare,degfree);
run;
/*This plots the log of the p-value by the number of clusters. vpos specifies the number of 
print positions on the vertical axis. The lowest points on the graph (those with the smallest 
logpvalue) show the cluster solutions with the lowest p-values.*/
/*in case SAS/GRAPH is not available:
proc plot data=cutoff;
	plot logpvalue*numberofclusters/vpos=30;
run;quit;
*/
proc gplot data=cutoff;
	title 'Choose the # of clusters associated with the lowest log p value';
	plot logpvalue*numberofclusters;
run;quit;
title;
/*The minid option in the means procedure finds the value of numberofclusters associated with 
the minimum value of logpvalue and assigns that value to the variable ncl.*/
proc means data=cutoff noprint;
	var logpvalue;
	output out=small minid(logpvalue(numberofclusters))=ncl;
run;
*This creates a local macro variable = the optimal # of clusters/levels;
data _null_;
	set small;
	call symputx('ncl',ncl);
run;
/*This produces a dendogram which shows that several branches (categories) 
can be combined with a minuscule reduction in chi-square. The out= data set shows which 
levels of &cvar are associated with each cluster (provided the nclusters= option is 
correctly specified). h= specifies the variable to be used as the height axis.*/
proc tree data=fortree nclusters=&ncl out=clus h=rsq;
	id &cvar;
run;
/*This is the final report*/
proc sort data=clus;
	by clusname;
run;
proc print data=clus noobs;
	title "====== &cvar level groupings (&ncl cluster solution) ======";
	/*by clusname;*/
	var clusname &cvar;
run;

/*---------------------*/
/*---CODE GENERATION---*/
/*---------------------*/
options ls=max;  /*for obvious reasons... if you try otherwise*/
data _null_;
	title;
	file print;
	put '===Generated DATA STEP code===';
run;
/*the root name for the new vars = up to 20 characters from the name of the original variable*/
proc sql noprint;
	select distinct substr("&cvar",1,20) into :newvname
	from clus;
quit;
/*enclose the values between quotes*/
data clus;
	length level $ 100;
	set clus;
	level = cats("'",&cvar,"'");
run;
/*now loop*/
proc sql;
	create table forloop as
	select distinct cluster
	from clus
	order by cluster;
quit;
data forloop;
	set forloop;
	varnum = _N_;
run;
%dnull(forloop,cluster varnum,vn)
%do cg = 1 %to &vn;
proc sql noprint;
	select level into :levels separated by " , "
	from clus
	where cluster = &&cluster&cg;
quit;
data _null_;
	title;
	file print;
	%if &use = R %then %do;
		put "if trim(left(&cvar)) in (&levels) then &newvname._coll = 'L&&varnum&cg' ;";
	%end;
	%if &use = SAS %then %do;
		put "&newvname._coll&&varnum&cg = (trim(left(&cvar)) in (&levels));";
	%end;
	put ' ';
run;
%end;
options ls=80;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2003-06*/
/*
o This macro runs the collapselevels.sas macro for each of a list of categorical variables.
o Arguments:
  - tvar = the target variable
  - collvars = the categorical variables to collapse, in the format var1 var2 var3
*/

%macro collapsevars(dataset,tvar,collvars);
data collvars1;
	put &collvars;
run;
proc transpose data=collvars1 out=collvars2 (drop=col1);
run;
data _null_;
	set collvars2 nobs=ncollvars;
	call symputx('coll'||trim(left(put(_n_,8.))),_name_);
	call symputx('ncollvars',trim(left(ncollvars)));
run;
%local i;
%do i=1 %to &ncollvars;
	%collapselevels(&dataset,&tvar,&&coll&i);
%end;
%mend;


/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2006-02*/
/*macro that, given the names of a table and a column, puts the distinct values
of 'column' from 'table' into global macro variables
  - The resulting global macro variables are named 'column1' - 'columnT', where T
is the number of distinct values in 'column' (T is available via &nval)
  - &distinct specifies whether &dset is already a summary table sorted in a particular
order that should be retained for processing*/

/*NB: beware of macro variable collisions*/

%macro coltomacvars(dset,col,distinct=NO);
%local i;
%if %upcase(&distinct) = NO %then %do;
proc sql;
	create table temp as
	select distinct &col
	from &dset
	order by &col;
quit;
%end;
%if %upcase(&distinct) = YES %then %do;
data temp;
	set &dset;
	keep &col;
run;
%end;
data _null_;
	call execute('%global nval'||';');
run;
proc sql noprint;
	select trim(left(put(count(*),8.))) into :nval
	from temp;
quit;
data _null_;
	%do i=1 %to &nval;
		call execute('%global &col.'||trim(left(&i))||';');
	%end;
run;
proc sql noprint;
	select &col into :&col.1-:&col&nval
	from temp;
	drop table temp;
quit;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2012-05*/
/*
o Print all the columns names from a given table. This is proving helpful when searching
  for things in very wide Miser tables.
o 2020-05: Added some additional logic for the column names to include or exclude.
  Because SAS is unable to take the keyword NOT as part of a macro variable to be 
  compared with blank, I had to use 2 arguments, one for inclusion and one for exclusion. 
  A valid LIKE call is expected, such as like "BAL%".
o Arguments:
    ord = sort the contents by this field
    incl, excl = LIKE calls with the pattern in ALL CAPS
*/

%macro columns(dset, out=temp_tb_cont, ord=varnum, incl=, excl=);
/*
proc contents data=&dset noprint out=&out; run;
proc sort data=&out; by varnum; run;
proc print data=&out;
	title "Columns in &dset";
	var LIBNAME MEMNAME NAME TYPE LENGTH VARNUM LABEL FORMAT FORMATL FORMATD INFORMAT INFORML ;
run;
title;
*/
/*get the names of the library and data set*/
%if %index(&dset,.) = 0 %then %do; 
	%let lib=WORK;
	%let dsname=%upcase(&dset);
%end;
%else %do; 
	%let lib=%upcase(%scan(&dset,1,"."));
	%let dsname=%upcase(%scan(&dset,2,"."));
%end;

proc sql;
	create table &out as
	select *
	from dictionary.columns
	where libname = "&lib" and memname = "&dsname"
		 %if &incl ne %then %do; and upcase(name) &incl %end; 
		 %if &excl ne %then %do; and upcase(name) not &excl %end; 
	order by &ord ;
quit;

	/*... into macro variables*/
/*
	select trim(left(put(count(*),8.))) into :nvars from vars;
	select name into :v1-:v&nvars from vars;
quit;
*/
proc print data=&out;
	title "Columns in &dset";
run;
title;

/*
proc sql;
	drop table &out;
quit;
*/
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2013-06*/
/*
o Create a table with all the column names from all tables in a given schema.
  This is meant to help search for particular variables when not even the table
  name is known for sure.
o Usage:
  %columns_schema(DIMDW, allcolumns)
  proc print data=allcolumns noobs;
    where index(NAME, 'MATURITY') > 0;
  run;
  Note how PROC PRINT retrieves only the rows of interest, given that the resulting
  table can be rather massive (e.g., 14.6k columns currently in all of DIMDW).
*/

%macro columns_schema(lib, out);
%local i;
/*get all tables from that schema*/
proc sql;
	create table temp_lib_cont as
	select libname, memname, nvar
	from dictionary.tables
	where libname=upcase("&lib")
		and memtype = 'DATA'
	order by memname;
quit;
%loopingvalues(temp_lib_cont, memname)
%do i = 1 %to &Nmemname;
	proc contents data=&lib..&&memname&i noprint out=temp_tb_cont; run;
	proc sort data=temp_tb_cont; by varnum; run;
	data temp_columns_&i;
		set temp_tb_cont;
		keep LIBNAME MEMNAME NAME TYPE LENGTH VARNUM LABEL FORMAT FORMATL FORMATD INFORMAT INFORML ;
	run;
%end;
data &out;
	set %do i = 1 %to &Nmemname; temp_columns_&i %end;;
run;
proc sql;
	%do i = 1 %to &Nmemname; drop table temp_columns_&i; %end;
quit;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2007-10*/
/*
o This macro combines all the datasets whose names match a given pattern, from a given library,
  in one of 2 ways: stacking or interleaving. 
o It is assumed that the datasets are appropriately sorted for interleaving (if requested).
o Arguments:
  - out = the name of the output dataset
  - lib = a library containing the datasets to be combined
  - pattern = a pattern present the file names that robustly identifies the datasets to combine
  - by = an optional argument which, if used, causes the datasets to be interleaved by this (these)
    column(s); if not used, the datasets will be stacked
  - fname = whether to include the file names in an additional column
  - keep = how many datasets to stack (all if blank); if provided, the last &keep datasets (as sorted
    by name) are stacked. The intention is to combine the most recent X files, where the file names
    are assumed to contain the dates in a format guaranteed to lead to correct sorting (e.g. yyyy-mm-dd).
    Proper file names are better for identifying the most recent files, given that the created and
    modified datetimes can be changed  by subsequently modifing or recreating some datasets.
  - report = Y to print a report, anything else to not print it
o Usage:
...first some code that creates a bunch of datasets, e.g.
  hhsumm_20070813.sas7bdat, hhsumm_20070820.sas7bdat, hhsumm_20070827.sas7bdat etc...
%combineds(mydata, mylib, hhsumm)  *stack the datasets;
%combineds(mydata, mylib, hhsumm, by=HSHLD_ID Date)  *interleave the datasets by HH id and Date;
*/

%macro combineds(out, lib, pattern, by=, fname=N, keep=, report=Y);
/*get the names of the datasets to combine*/
proc sql;
	create table temp_lib_cont as
	select *
	from dictionary.tables
	where libname=upcase("&lib")
		and memtype = 'DATA'
		and index(memname, upcase("&pattern")) > 0
	order by memname;
quit;
%if &keep ne %then %do;
	proc sort data=temp_lib_cont;
		by descending memname;
	run;
	data temp_lib_cont;
		set temp_lib_cont;
		if _N_ <= &keep;
	run;
	proc sort data=temp_lib_cont;
		by memname;
	run;
%end;
proc sql noprint;
	select trim(left(put(count(*),8.))) into :Nds from temp_lib_cont;  /*total number of datasets*/
	select memname into :cds1-:cds&Nds from temp_lib_cont;  /*the names of all datasets*/
	select nobs into :N1-:N&Nds from temp_lib_cont;  /*number of rows in each dataset*/
quit;
/*combine*/
%local i;
data &out;
	set %do i=1 %to &Nds; &lib..&&cds&i %end;;
	%if &by ne %then %do;
		by &by;
	%end;
run;
/*add the file names, if requested*/
/*NB: do this only in the case of stacking, not merging*/
%if %upcase(&fname) = Y and &by eq %then %do;
	%do i=1 %to &Nds;
		data fnames&i;
			length fname $ %length(&&cds&i);
			do f = 1 to &&N&i;
				fname = "&&cds&i";
				output;
			end;
			drop f;
		run;
	%end;
	data allfname;
		set %do i=1 %to &Nds; fnames&i %end;;
	run;
	proc sql;
		%do i=1 %to &Nds; drop table fnames&i; %end;
	quit;
	data &out;
		merge &out allfname;
	run;
%end;
/*a report*/
%if &report = Y %then %do;
	proc sql;
		%if &by ne %then %do;
			title "Interleaved datasets (by &by) from library %upcase(&lib):";
		%end;
		%else %do;
			title "Stacked datasets from library %upcase(&lib):";
		%end;
		select memname label 'Dataset',
			nobs label 'Rows' format comma20.0, 
			modate label 'Modified',
			/*user-friendly file size (reported value is bytes)*/
			filesize / (1024*1024) as filesize label 'Size (MB)' format comma20.1,
			pcompress label '% Compression'
		from temp_lib_cont
		order by memname;
		drop table temp_lib_cont;
	quit;
	proc contents data=&out varnum;
		title 'The output dataset:';
	run;
	title;
%end;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2020-06
o Import CSV files regardless of whether a file extension is present in the file name.
o Arguments:
  file = the full file name, including the path
  guess = how many records to scan. Set to a lower for speed, set to MAX for safety (scanning
          large files may take a long time) 
*/

%macro csvimp(file, dset, guess=32000);
proc import datafile="&file" out=&dset replace dbms=CSV;
	guessingrows=&guess;
	getnames=yes;
	delimiter=",";
run;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea sep04*/
/*This macro automates the import of CSV files exported through an Excel macro. 
In particular, it takes care of the percentage values.
First, the CSV file is imported as usual, using PROC IMPORT for automation. 
Then, the initial CSV file is scanned for the % symbol. If the % symbol is found,
the variable is divided by 100.*/

%macro csvpct(in,out);
%local i;
/*regular proc import*/
proc import datafile="&in" out=&out replace;
	delimiter=',';
	getnames=yes;
run;
/*the vars*/
proc sql noprint;
	create table vars as
	select distinct name,varnum
	from dictionary.columns
	where libname=upcase("work") and memname=upcase("&out")
	order by varnum;
	select trim(left(put(count(*),8.))) into :nvars
	from vars;
	select name into :vars separated by ' $ '
	from vars;
	select name into :vrs separated by ' '
	from vars;
	select name into :v1-:v&nvars
	from vars;
quit;
/*import a few rows and look for '%' */
data pctimp;
	length &vrs $ 50;
	infile "&in" delimiter = ',' missover dsd lrecl=32000 firstobs=2 obs=11;
	input &vars $;
run;
data pctscan;
	set pctimp end=last;
	%do i=1 %to &nvars;
		idx&i + index(&&v&i,'%');
	%end;
	if last then output;
run;
/*who has the '%' ?*/
proc transpose data=pctscan out=pctscan2;run;
data pctscan2;
	set pctscan2;
	if col1 gt 0;
	varnum = compress(_name_,"idx");
run;
/*how many % vars?*/
/*&npct=0 doesn't cause problems*/
proc sql noprint;
	select trim(left(put(count(*),8.))) into :npct
	from pctscan2;
	select varnum into :pct1-:pct&npct
	from pctscan2;
	drop table vars;
	drop table pctimp;
	drop table pctscan;
	drop table pctscan2;
quit;
/*divide the % vars by 100*/
data &out;
	set &out;
	%do i=1 %to &npct;
		&&&&v&&pct&i = &&&&v&&pct&i / 100;
		format &&&&v&&pct&i percent10.5;
	%end;
run;
proc print data=&out;run;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2006-01*/
/*
o This macro, similar to the function cut() from R, divides a continuous variable into
  specified intervals.
o Only the middle break points need to be specified: the min and max are computed.
o 2011-06: added 2-digit numbers in front of each interval to ensure that the tiers can
  be properly sorted. This is required given that there's no such thing as ordered factors
  in SAS.
o 2013-07: Added the generated SAS code in the log, for confirmation and clarity.
o Usage: 
  %cut(table, newtable, Age, AgeInterval, 25 45 65 85)
*/

%macro cut(dset, out, var, newvar, breaks);
%local i j id last vmin vmax;
%lst(&breaks,sep)
proc sql noprint;
	select round(min(&var)) into :vmin from &dset; 
	/*select round(max(&var)) into :vmax from &dset;*/
quit;
/*2016-01: Replaced max(&var) with max, to avoid very large values, strangely formatted*/
%let vmax = max;
data &out;
	length &newvar $ 40;
	set &dset;
	if &var = . then &newvar = ' ';
	%put ===The following SAS code was generated:;
	%put if &var = . then &newvar = ' ';
		else if &var < &sep1 then &newvar = compress("_01_[&vmin, &sep1)");
		%put else if &var < &sep1 then &newvar = compress("_01_[&vmin, &sep1)");
		%do i = 1 %to %eval(&nw-1);
			%let j = %eval(&i+1);
			%if %eval(&j) < 10 %then %do; %let id = 0&j; %end;
			%if %eval(&j) >= 10 %then %do; %let id = &j; %end;
			else if &var >= &&sep&i and &var < &&sep&j then &newvar = compress("_&id._[&&sep&i, &&sep&j)");
			%put else if &var >= &&sep&i and &var < &&sep&j then &newvar = compress("_&id._[&&sep&i, &&sep&j)");         
		%end;
	%let last = %eval(&nw+1);
	%if %eval(&last) < 10 %then %do; %let id = 0&last; %end;
	%if %eval(&last) >= 10 %then %do; %let id = &last; %end;
	else &newvar = compress("_&id._[&&sep&nw, &vmax]");
	%put else &newvar = compress("_&id._[&&sep&nw, &vmax]");
run;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea May 2004*/
/*This macro counts the number of words in a string.*/

%macro cwords(w);
(compress("&w") ne '') * (length(left(compbl("&w")))-length(compress("&w"))+1)
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2006-05*/
/*
o This macro queries the daily DDA, SAV and CD tables every &minutes minutes to
  check if they have been updated. The update criterion is AS_OF_DATE = yesterday. 
  If update=TRUE, the macro sends an email from &from to &to.
o 2008-07: discontinued the email, given that after the CBS hardware upgrade all loads
  are finished before 8 am. So run this macro prior to your main program, to make sure
  that the daily tables are current.
o 2012-06: Added a global variable with the latest date

o 2012-11: changes are required here, given the failure to determine an update in the week of
Nov 12, 2012. One program ran for 3 days before i noticed the infinite loop. add a new check:
if an update is not found within X hours, either give up, or use the latest data found.

o Usage:
%dailyTablesUpdate(10, "bromocea@ohiosavings.com", "bromocea@ohiosavings.com")
*/

%macro dailyTablesUpdate(minutes, extra=Y);
%local updated latest expected today minbal apparently;
%let minbal = 24;  *minimum balance in billion $ that is considered to indicate a successful update;
%global latestDailyData;
%let updated = N;
%do %until (&updated = Y); 
	proc sql;
		create table latest as
		select max(AS_OF_DATE) as latdt, 'DDA' as Source
		from FDWHH.FDW_DAILY_CHECKING_ACCT_BAL
		union
		select max(AS_OF_DATE) as latdt, 'SAV' as Source
		from FDWHH.FDW_DAILY_SAVING_ACCT_BAL
		union
		select max(AS_OF_DATE) as latdt, 'CD' as Source
		from FDWHH.FDW_DAILY_CD_ACCT_BAL;
	quit;
	proc sql noprint;
		select count(distinct latdt) into :dates
		from latest;
	quit;
	%if &dates = 1 %then %do;	/*no  update in progress*/
		data _null_;
			set latest;
			if _N_ = 1;
			latest = put(datepart(latdt),date9.);
			*2014-02: Relax the expected date: if today is Sun or Mon, then Fri data is fine;
			if put("&sysdate"d, downame3.) = 'Sun' then expected = put("&sysdate"d - 2,date9.);
				else if put("&sysdate"d, downame3.) = 'Mon' then expected = put("&sysdate"d - 3,date9.);
				else expected = put("&sysdate"d - 1,date9.);
			today = put("&sysdate"d,date9.);
			call symputx('latest',trim(left(latest)));
			call symputx('expected',trim(left(expected)));
			call symputx('today',trim(left(today)));
		run;
		data _null_;
			*determine if the data appears to have been updated;
			if "&latest"d >= "&expected"d then apparently = 'Y';
				else apparently = 'N';
			call symputx('apparently',trim(left(apparently)));
		run;
		%put Latest date = &latest, expected date = &expected, apparently updated = &apparently;
		%if &apparently = Y %then %do;	/*the tables have been updated*/
			/*2012-05 It turns out an additional check is needed here due to partial table updates
			(such as loading CBS data but missing the Miser data). This actually happened
			a few times, and once caused hugely incorrect results for one ERM metric (balance
			depletion in the last 30 days) due to 100% missing Miser accounts.*/
			proc sql;
				create table dailysumm as
				select 'DDA' as Source, count(*) as Accts, sum(DAILY_END_BALANCE) as Balance
				from FDWHH.FDW_DAILY_CHECKING_ACCT_BAL where AS_OF_DATE = dhms("&latest"d,0,0,0)
				union
				select 'SAV' as Source, count(*) as Accts, sum(DAILY_END_BALANCE) as Balance
				from FDWHH.FDW_DAILY_SAVING_ACCT_BAL where AS_OF_DATE = dhms("&latest"d,0,0,0)
				union
				select 'CD' as Source, count(*) as Accts, sum(DAILY_END_BALANCE) as Balance
				from FDWHH.FDW_DAILY_CD_ACCT_BAL where AS_OF_DATE = dhms("&latest"d,0,0,0);
			quit;
			proc sql noprint;
				create table dailysummtest as
				select sum(Balance)/1000000000 as Billions
				from dailysumm;
				select Billions into :billions from dailysummtest;
			quit;
			/*How utterly stupid %eval is... it can't be used with any numbers, only integers. So run
			the comparison in a data step instead...*/
			data dailysummtest;
				set dailysummtest;
				if Billions < &minbal then Incomplete = 'Y';
					else Incomplete = 'N';
				call symputx('Incomplete', trim(left(Incomplete))); 
			run;
			%if &Incomplete = Y %then %do;
				%timenowlog(WARNING the daily tables are only partially updated:
					the total balance for &latest is $&billions billion when at least $&minbal billion are expected)
			%end;
			%else %do;
				%let updated = Y;
				%timenowlog(daily tables are up2date   latest AS_OF_DATE is &latest   total balance = $&billions billion)
			%end;
		%end;
		%else %do;	/*no update*/
			%timenowlog(no update yet   latest AS_OF_DATE is &latest)
		%end;
	%end;
	%else %do;	/*update in progress*/
		%timenowlog(update in progress   found &dates max dates in the 3 daily tables)
	%end;
	%if &updated = N %then %do;
		%sleep(&minutes)
	%end;
%end;
proc sql;
	drop table latest;
	drop table dailysumm;
	drop table dailysummtest;
quit;
/*Sleep an extra 5 minutes as insurance against tables that perhaps were only partially updated.*/
%if &extra = Y %then %do;
	%sleep(5)
%end;
%let latestDailyData = &latest;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2010-02*/
/*
o Given a SAS libref pointing to a DBMS schema/database, this macro produces
  a report showing the number of columns and observations in each table. One
  query needs to be run for each table, because with DBMS tables the number of
  rows is unknown to SAS and doesn't show up in dictionary.tables.
o lib = a libref pointing to a DBMS schema (not a regular library / folder)
*/

%macro dbcont(lib);
%local i;
proc sql;
	/*put all tables into a dataset*/
	create table temp_lib_cont as
	select libname, memname, nvar
	from dictionary.tables
	where libname=upcase("&lib")
		and memtype = 'DATA';
quit;
/*now get the number of rows in each table*/
proc sql noprint;
	select trim(left(put(count(*),8.))) into :nfiles
	from temp_lib_cont;
	select memname into :file1-:file&nfiles
	from temp_lib_cont;
quit;
%do i = 1 %to &nfiles;
	proc sql noprint;
		select count(*) into :nrow
		from &lib..&&file&i;
	quit;
	data temp_rep_&i;
		set temp_lib_cont;
		where memname = "&&file&i";
		nobs = &nrow;
	run;
%end;
data librep;
	set %do i = 1 %to &nfiles; temp_rep_&i %end;;
	format nobs comma20.0;
run;
proc sql;
	drop table temp_lib_cont;
	%do i = 1 %to &nfiles;
		drop table temp_rep_&i;
	%end;
quit;
proc print data=librep; sum _numeric_; run;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2015-12
o The SAS PROC SQL was very slow on some SQL Server tables so I converted the syntax to pass-through.
*/

%macro dbcontSS(lib, server, db);
%local i;
proc sql;
	/*put all tables into a dataset*/
	create table temp_lib_cont as
	select libname, memname, nvar
	from dictionary.tables
	where libname=upcase("&lib")
		and memtype = 'DATA';
quit;
/*now get the number of rows in each table*/
proc sql noprint;
	select trim(left(put(count(*),8.))) into :nfiles
	from temp_lib_cont;
	select memname into :tb1-:tb&nfiles
	from temp_lib_cont;
quit;
*use just one connection;
proc sql noprint;
	connect to OLEDB (provider=SQLOLEDB.1
		PROVIDER_STRING="Server=&server;Database=&db;Trusted_Connection=yes");
	%do i = 1 %to &nfiles;
		create table querytmp_&i as select * from connection to OLEDB
		(select count(*) as Rows from &&tb&i);
	%end;
	disconnect from OLEDB;
quit;
%do i = 1 %to &nfiles;
	proc sql;
		create table temp_rep_&i as
		select a.*, Rows
		from temp_lib_cont a, querytmp_&i b
		where memname = "&&tb&i";
	quit;
%end;
data librep;
	set %do i = 1 %to &nfiles; temp_rep_&i %end;;
	format Rows comma20.0;
run;
proc sql;
	drop table temp_lib_cont;
	%do i = 1 %to &nfiles;
		drop table querytmp_&i;
		drop table temp_rep_&i;
	%end;
quit;
proc print data=librep; sum _numeric_; run;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2006-10*/
/*
o Given a dataset sorted by a variable &by, this macro dedupes it by keeping only the first
  or last observation from each BY group.
o Arguments:
  - dset/out = the input/output datasets
  - by = the BY variable
  - keep = first or last (data step key words that determine which row is kept)
*/

%macro dedupe(dset, by, keep, out);
data &out;
	set &dset;
	by &by;
	if &keep..&by;
run;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea jun04*/
/*This macro deletes global macro variables.*/

%macro delglobal;
data macrovars;
	set sashelp.vmacro;
run;
data _null_;
	set macrovars;
	if scope='GLOBAL' and name not in ('P','O','SYSDBMSG','SYSDBRC','SYSODSPATH') 
		then call execute('%symdel'||' '||trim(left(name))||';');
run;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea aug05*/
/*this macro outputs the difference between two tables: rows that 
are in the first but not in the second table

tab1 = the table to subtract from
tab2 = the table to subtract
out = the name of the output data set
by1, by2 = the fields used to do the subtraction (join) ; if only by1 is provided,
	then the join is done on the same field (&by1), otherwise the join is done 
	on &by1 = &by2

USAGE: 
%diff(dset1,dset2,SSN,,extra)
%diff(dset1,dset2,SSN,HSHLDMBR_TAX_ID,extra)
*/

%macro diff(tab1,tab2,by1,by2,out);
proc sql;
	create table &out as
	select distinct a.*
	from &tab1 a left join &tab2 b
	%if &by2 = %then %do;
			on a.&by1 = b.&by1
		where b.&by1 is null
	%end;
	%else %do;
			on a.&by1 = b.&by2
		where b.&by2 is null
	%end;
	order by &by1;
quit;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2004-07*/
/*
o This macro is useful for seeing if a variable from a data set has duplicate values.
o An enhanced (and more expensive) alternative is finddup.sas.
o 2010-03: Overhauled the previous version to query several variables at the same time.
o 2013-06: Added the capability to get distinct counts for all variables, which happens
  if no variable names are specified.
*/

%macro distinct(dset, var);
%local j;
%if &var = %then %do;  /*run all variables*/
	proc contents data=&dset noprint out=vars_profiler; run;
	proc sort data=vars_profiler; by varnum; run;
	proc sql noprint;
		select trim(left(put(count(*),8.))) into :nvar from vars_profiler;
		select name into :univar1-:univar&nvar from vars_profiler;
		/*drop table vars_profiler;*/
	quit;
%end;
%else %do;  /*run select variables*/
	data _null_;
		nvar=%cwords(&var);
		call symputx('nvar',trim(left(nvar)));                      
	run;
	data _null_;
		%do j=1 %to &nvar;
			var=scan("&var", &j, ' ');
			call symputx("univar&j" ,trim(left(var)));
		%end;
	run;
%end;

/*now query each variable*/
%do j=1 %to &nvar;
	proc sql;
		create table unirep&j as
		select "&dset" as Dataset, "&&univar&j" as Variable,
			count(distinct &&univar&j) as Unique_Values format comma20.0
		from &dset;
	quit;
%end;
proc sql noprint;
	select count(*) into :rows
	from &dset;
quit;
/*report & cleanup*/
data unirep;
	length Variable $ 32;
	set %do j=1 %to &nvar; unirep&j %end;;
	Rows = &rows;
	format rows comma20.0;
run;
proc print data=unirep noobs;
	title "Distinct values in data set [&dset]";
run;
title;
proc sql;
	%do j=1 %to &nvar;
		drop table unirep&j;
	%end;
	drop table unirep;
quit;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2004-05*/
/*
o I wrote *enough* DATA _NULL_ steps for macro variable creation. With this macro, any number of
  values from any number of columns from a dataset can be very easily put into global macro vars.
o Arguments: 
  - dset = the data set with the variables whose values will be available as macro vars
  - vars = a list with the names of the columns of interest (separated by spaces; these names
    will be the root names of the created macro vars)
  - num = the name of the macro variable that will store the total number of macro vars in each series
o Usage:
%dnull(mytable,branch brnum,nbr)
o Output (given the example above):
  - two series of global macro vars (branch1,branch2,...,brnum1,brnum2,...)
  - plus a global macro var (nbr) that stores the total # of vars in each series (so the last
    vars are &&branch&nbr, &&brnum&nbr).
o Warning
  - Pay close attention to macro variable collisions.
o 2011-10: Added a module at the end that reports the macro variables that were created. The
  syntax is tremendously ugly, courtesy of SAS, but it works.
*/

%macro dnull(dset,vars,num);
%global &num;
%local i;  /*this will avoid looping surprises, in case %dnull is run in a loop with the index named i*/
data _null_;
  nwords=%cwords(&vars);
  call symputx('nw',trim(left(put(nwords,5.))));
run;
data _null_;
  %do i=1 %to &nw;
    w&i=scan("&vars",&i);
    call symputx("v&i",trim(left(w&i)));
  %end;
run;
%do i=1 %to &nw;
  data _null_;
    set &dset nobs=n;
    call execute('%global &&v&i..'||trim(left(put(_n_,6.)))||';'); 
    call symputx("&&v&i"||trim(left(put(_n_,6.))),trim(left(&&v&i)));
  run;
%end;  
/*create &num only once*/
data _null_; 
  if 0 then set &dset nobs=n;
  call symputx("&num",trim(left(put(n,6.))));
  stop;
run;
/*report the results in the log*/
%put === The following macro variables were created: === ;
%put &num = &&&num;
%do i=1 %to &nw;
	/*2017-09: Limit the number of values reported for each variable. The intention is to
	keep the SAS log smaller, as confirmations are not really needed for say all 200 values
	of every macro variable.*/
	%if %eval(&&&num > 30) %then %do;
		%do j=1 %to 10;
			%put &&v&i..&j = &&&&&&v&i..&j ;
		%end;
		%put ................... ;
		%do j=%eval(&&&num - 9) %to &&&num;
			%put &&v&i..&j = &&&&&&v&i..&j ;
		%end;
	%end;
	/*With fewer values it is ok to print all of them*/
	%if %eval(&&&num <= 30) %then %do;
		%do j=1 %to &&&num;
			%put &&v&i..&j = &&&&&&v&i..&j ;
		%end;
	%end;
%end;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2019-10
o This macro saves some typing when converting datetime fields to date.
o My first preference would have been to do so directly in Oracle, but it turned out
  Oracle really does not like converting datetime to date (unless the date is actually a text
  string). Then, for efficiency reasons, I did not want to overwrite the large datasets pulled
  from DNA.
o As a compromise, use this macro to convert selected fields from datetime to date. Since this
  is meant to be run on much smaller datasets / reports, efficiency is much less of an issue.
o fields = list of datetime fields separated by spaces, to be converted to date
*/

%macro dt2d(dset, fields);
%lst(&fields, word)
%local i;
data &dset;
	set &dset; 
	%do i = 1 %to &nw; 
		TmpField&i = datepart(&&word&i);
		format TmpField&i date9.;
		drop &&word&i;
		rename TmpField&i = &&word&i;
	%end;
run;
%mend;

/*br 2020-06
o This variant is meant to be used directly within a SAS data step, and is thus more efficient 
  because it avoids writing another (potentially large) file.
*/
%macro dtime2dt(dt);
if &dt ne . then o&dt = datepart(&dt);
drop &dt;
rename o&dt = &dt;
format o&dt date9.;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2012-10*/
/*
o I finally lost the patience to repeatedly specify the end of month for macro vars,
  which requires first figuring out the number of days in the given month. This macro
  is meant to simplify that process.
o Usage: %eom(AUG2012, to) will create the variable &to with the value 31AUG2012.
o 2013-02: Also added another variable with the name &var.iso and ISO-compliant format
  (YYYYMMDD). This is meant to be used in file names.
*/

%macro eom(mth, var);
%global &var &var.iso;
data _null_;
	call symputx("&var", trim(left(put(intnx('month', "15&mth"d, 0, 'end'), date9.))));
	call symputx("&var.iso", trim(left(put(intnx('month', "15&mth"d, 0, 'end'), yymmddn8.))));
run;
/*report the results in the log*/
%put === The following macro variables were created: === ;
%put &var = &&&var , &var.iso = &&&var.iso;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2006-08*/
/*
o This macro is intended to quickly profile large SAS datasets. The report includes:
  - # of columns in the data set
  - # of distinct values in each column
  - the first X rows from the frequency table of each column, sorted alphabetically or by frequency.
o Since the datasets aren't supposed to be well known, the variable type is ignored: nominal variables may have
  been coded with digits and imported as numeric, and numeric variables may have been imported as character.
o If you want to profile a DW table, it may be better to save the table as a SAS dataset first.
o Arguments:
  - dsname = the name of the dataset, including the library (mylib.mytable)
  - sortby (alpha or freq) = when printing the first 'printobs' from each distribution
    table, start in alphabetical order (default) or with the most frequent
  - folder = if not null, print the report to the file &folder\&dset.htm
  - num = whether to focus only on numeric variables and produce their summary statistics
o 2010-12: Added a new argument, num, which if set to Y leads to producing summary statistics for
  the numeric variables only. This is meant to help explore the numerics whose frequency distributions
  are not dominated by a few values, in which case the mean and the percentiles are much more helpful.
o 2011-02: I had to modify the piece that gets the column names and their types. The existing approach
  relying on querying DICTIONARY.COLUMNS began to produce errors with Oracle tables, and especially with
  SQL Server tables where it just wouldn't work. As a workaround, I switched to PROC CONTENTS.
o 2011-10: Tweaked the output rules as follows. If the variable contains less than X unique values
  then produce the frequency distribution, regardless of numeric or character. If the variable contains
  more than X unique values, then: if character, produce a trimmed frequency distribution; if numeric,
  produce its summary statistics.
o 2019-09: Streamlined the code (use the table of contents from the missing macro), and stop printing and
  running queries for fields which are 100% missing.
o Usage:
%explore(lib.mytable) - profile & print the first 20 distinct values of each variable
%explore(mytable, folder=\\pplzapp05\bi\bogdan) - create the report \\pplzapp05\bi\bogdan\mytable.htm
%explore(work.mytable, printobs=40, sortby=freq) - profile & print the 40 most frequent values of each variable
*/

%macro explore(dset, sortby=freq, folder=none, num=N);
%local i;
%if &folder ne none %then %do;
	%sink(&dset..html, fld=&folder)
%end;

/*show a missing values report first*/
%missing(&dset, out=MissingCounts)

/*put the column names...*/
/*this just wouldn't work with SQL Server tables, so I replaced it with PROC CONTENTS*/
/*
proc sql noprint;
	create table vars_profiler as
	select *
	from dictionary.columns
	where libname="%upcase(&lib)" and memname = "%upcase(&dset)";
quit;
*/
/*2019-09: Better use the report from %missing - which is already available.
proc contents data=&dset noprint out=vars_profiler; run;
data vars_profiler;
	length ttype $ 4;
	set vars_profiler;
	if type = 1 then ttype = 'NUM';
		else if type = 2 then ttype = 'CHAR';
	drop type;
	rename ttype = type;
run;
*/
*2019-09: Stop running fields which are 100% missing;
data vars_profiler;
	set MissingCounts;
	if PctMissing = 1 then delete;
	type = upcase(type);
run;
proc sort data=vars_profiler; by varnum; run;

/*... into macro variables*/
proc sql noprint;
	select trim(left(put(count(*),8.))) into :nvars from vars_profiler;
	select name into :v1-:v&nvars from vars_profiler;
	select varnum into :vnum1-:vnum&nvars from vars_profiler;
	select type into :ty1-:ty&nvars from vars_profiler;
	select format into :fm1-:fm&nvars from vars_profiler;
	select distinct N format comma20. into :totalobs from vars_profiler;
quit;

%if &num = N %then %do;  /*examine all variables*/
/*notes*/
data _null_;
	title;
	file print;
	put "[%upcase(&dset)] profile";
	put "  - There are &nvars columns (excluding any with 100% missing) and &totalobs rows.";
	%if %upcase(&sortby) = ALPHA %then %do;
		put '  - The values are printed in alphabetical order.';
	%end;
	%if %upcase(&sortby) = FREQ %then %do;
		put '  - The values are printed in descending order of frequency.';
	%end;
run;
/*a report for each variable*/
%do i=1 %to &nvars;
	proc sql noprint;
		create table temp_profiler as
		select &&v&i,count(*) as Freq format comma20.0
		from &dset
		group by &&v&i
		%if %upcase(&sortby) = ALPHA %then %do; order by &&v&i %end;
		%if %upcase(&sortby) = FREQ %then %do; order by calculated Freq desc %end;;
		select count(*) format comma20. into: DistinctVal from temp_profiler;
		select count(*) into: DistVal from temp_profiler;
	quit;
	%addpct(temp_profiler,Freq)
	%if &DistVal <= 20 %then %do;
		proc print data=temp_profiler (obs=20) noobs;
			title "&&vnum&i) &&v&i: &DistinctVal distinct values, showing all";
			sum freq pctfreq;
		run;
	%end;
	%if &DistVal > 20 and &DistVal <= 50 %then %do;
		proc print data=temp_profiler (obs=20) noobs;
			title "&&vnum&i) &&v&i: &DistinctVal distinct values, showing 20";
			sum freq pctfreq;
		run;
	%end;
	/*if there are more than 50 unique values, customize the report for numeric and character*/
	%if &DistVal > 50 %then %do;
		%if &&ty&i = NUM %then %do;
			proc means data=&dset noprint;
				var &&v&i;
				output out=summstats(drop=_type_ _freq_) N=N Nmiss=Nmiss Min=Min P1=P1 P5=P5
					P25=P25 Median=Median Mean=Mean P75=P75 P95=P95 P99=P99 Max=Max;
			run;
			proc print data=summstats noobs;
				title "&&vnum&i) &&v&i: &DistinctVal distinct values";
			run;
		%end;
		%if &&ty&i = CHAR %then %do;	
			proc print data=temp_profiler (obs=10) noobs;
				title "&&vnum&i) &&v&i: &DistinctVal distinct values, showing 10";
				sum freq pctfreq;
			run;
		%end;
	%end;
%end;
%end;

%else %do;  /*look only at the numeric vars and produce summary stats*/
proc sql noprint;
	select name into :numvars separated by ' ' from vars_profiler where upcase(type) = 'NUM';
	select trim(left(put(count(*),8.))) into :nvars from vars_profiler where upcase(type) = 'NUM';
	select count(*) format comma20. into :totalobs from &dset;
quit;
data _null_;
	title;
	file print;
	put "[%upcase(&dset)] profile";
	put "  - There are &nvars numeric variables and &totalobs rows.";
run;
options nolabel;
proc means data=&dset nmiss min p1 p5 p25 mean median p75 p95 p99 max;
	var &numvars;
	/* format &numvars best10.; */
run;
%end;

title;
%if &folder ne none %then %do;
	%sink()
%end;
proc sql;
	drop table vars_profiler;
	drop table temp_profiler;
	drop table summstats;
quit;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2006-05, updated 2010-09*/
/*
o This macro exports a SAS data set in pipe-delimited (or CSV) format.
o Why |: because PROC EXPORT to CSV format isn't robust. I've seen a couple of cases
  where PROC EXPORT with dbms=csv created (slightly) defective files - such as missing
  column names, and of course the mess with the double quotes which can't be controlled
  through PROC EXPORT syntax (see below for details).
o fname = the file name, fld = the folder
o sep = the separator to use, csv for CSV or | for pipe-delimited
o zip = whether to compress the file; this is supposed to be used only on ADM, where
  the 
*/

%macro export(dset, fname, fld=\\NYCB\corp$\DBMarketing\Bogdan\data, csv=N, zip=N);
%local outfile;
%if %upcase(&csv) = Y %then %do;
	%let outfile = &fname..csv;
	proc export data=&dset outfile="&fld.\&outfile" dbms=csv replace; run;
%end;
%else %do;
	%let outfile = &fname..txt;
	proc export data=&dset outfile="&fld.\&outfile" dbms=dlm replace; delimiter='|'; run;
%end;
/*This was applicable only to the need to move data between platforms at Key.
%if %upcase(&zip) = Y %then %do;
	data _null_;
		call system("cd &fld ; zip &fname..zip &outfile" );
	run;
%end;
*/
%mend;

/*
-----Original Message-----
From: SAS Technical Support [mailto:support@sas.com] 
Sent: Thursday, October 27, 2005 3:54 PM
To: Romocea, Bogdan
Subject: [SAS us6301542] PROC EXPORT, CSV and quotes for character variables

::~:: NOTE: IF YOU REPLY, ALL TEXT FOLLOWING THIS LINE WILL BE IGNORED.

     Trknum: us6301542
   Customer: Bogdan Romocea
    Sitenum: 0047478001
    Company: Ohio Savings Bank
      Phone: +1 (216) 588-4552

         OS: win2000server    Product: base
 OS Release:                 Topic: proc
Product Rel: 9.1          Subtopic: export/csv
    TSLevel:           

<=== Page: 1 === SAS Consultant === emailed w/answer === 27Oct2005 15:42:10 ===>

Bogdan,
  You can't get the level of control that you want unless you go with a 
datastep. Proc Export and the Import Wizard just don't over you that level 
of detail. The External File Interface allows more, but you still can't 
get the exact output you want with it. Plus, it sounds like you're looking
for a coding solution rather than a point and click. 

Regards,
Kelly


-----Original Message-----
From: Romocea, Bogdan 
Sent: Thursday, October 27, 2005 3:33 PM
To: SAS Technical Support
Subject: RE: [SAS us6301542] PROC EXPORT, CSV and quotes for character variables

Kelly,

That's precisely what I want to avoid. I use PROC EXPORT because I want to automate the export, 
and I use PROC IMPORT because I want to automate the import. Manually tweaking data step code 
is not an option.

Can I get what I asked for in a completely automated fashion? (I can, I know how to code this 
from scratch, but would rather avoid that path.) I expected PROC EXPORT to be able to take care 
of this but couldn't find anything in the documentation.

Thanks,
Bogdan


-----Original Message-----
From: SAS Technical Support [mailto:support@sas.com] 
Sent: Thursday, October 27, 2005 3:19 PM
To: Romocea, Bogdan
Subject: [SAS us6301542] PROC EXPORT, CSV and quotes for character variables


::~:: NOTE: IF YOU REPLY, ALL TEXT FOLLOWING THIS LINE WILL BE IGNORED.

     Trknum: us6301542
   Customer: Bogdan Romocea
    Sitenum: 0047478001
    Company: Ohio Savings Bank
      Phone: +1 (216) 588-4552

         OS: win2000server    Product: base
 OS Release:                 Topic: proc
Product Rel: 9.1          Subtopic: export/csv
    TSLevel:           

<=== Page: 1 === SAS Consultant === emailed w/answer === 27Oct2005 15:15:20 ===>

Bogdan,
  You can take the datastep code that is generated by Proc Export and 
tweak it to enclose all your values in quotes. You will need DSD on your
FILE statement, then you can use the "~" modifier after the variable 
on the PUT statement to force it to be quoted: 

  put x ~date9.  y ~comma. z ~ ; 

   I will be happy to continue working with you should you have any
follow-up questions regarding this matter. The track will remain
open for an additional 7 days to allow you an opportunity to
request further assistance on this issue. 

Regards,
Kelly

-----Original Message-----
From: Romocea, Bogdan 
Sent: Thursday, October 27, 2005 3:02 PM
To: SAS Technical Support (support@sas.com)
Subject: PROC EXPORT, CSV and quotes for character variables

name=Bogdan Romocea 
phone=+1 216-588-4552 
site=0047478001 
company=Ohio Savings Bank 
product=Base SAS 
release=9.1 
os=Windows Server 2003

Hello,

How can I have PROC EXPORT enclose all/select character fields in double quotes? 
When I have to import the data back into SAS I want to avoid tweaking the code 
created by PROC IMPORT (which imports as numeric variables that were character prior 
to the export - 000123 in the text file becomes 123 in the SAS data set). I'm only 
interested in delimited text formats (CSV, TAB etc).

Thanks,
Bogdan
*/

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2008-04*/
/*
o This macro prints the first X rows from a dataset having the highest or lowest values
  of a given variable. I've been using PROC UNIVARIATE for this kind of stuff, however the output
  of this PROC is of limited usefulness because only the variable and the observation number are shown.
  This macro shows the complete observations and thus it is much more valuable for data exploration and 
  troubleshooting.
*/

%macro extremeobs(dset, var, rows=20);
proc sql outobs=&rows;
	title "The first &rows rows from [ &dset ] with the lowest values of [ &var ]";
	select *
	from &dset
	where &var is not null  /*required as missing is implemented as the smallest value in SAS...*/
	order by &var;
	title "The first &rows rows from [ &dset ] with the highest values of [ &var ]";
	select *
	from &dset
	order by &var desc;
quit;
title;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea aug04*/
/*
o This macro finds and prints the rows with the duplicate values of a given variable &var 
  from a dataset &table.
o Usage: %finddup(lib.dataset, column)
o May 2005: introduced a parameter to control the # of rows to be printed - to avoid 
  delays and/or crashes if there are a lot of duplicates.
o 2010-04: Implemented changes to make the printing of the examples more user-friendly.
  Instead of printing a given number of rows, print all the rows of a given number of
  non-distinct values, beginning with the most frequent ones.
o 2014-07: Added the capability to print just a short report, for the cases where getting
  the duplicate values is of most interest, with little interest for the rest of the info.
*/

%macro finddup(table, var, print=50, short=N);
proc sql noprint;	
	create table dupl_counts as
	select &var, count(&var) as TimesListed
	from &table
	group by &var
	order by TimesListed desc;
	select max(TimesListed) into :duplyn
	from dupl_counts;
quit;
%if &duplyn > 1 %then %do;
	proc sql;
		create table summ_dupl_rep as
		select TimesListed, count(*) as UniqueValues
		from dupl_counts
		group by TimesListed
		order by TimesListed;
	quit;
	%addpct(summ_dupl_rep, UniqueValues)
	proc print data=summ_dupl_rep noobs; run;
	data dupl_print;
		set dupl_counts;
		if TimesListed >= 2;
		if _N_ <= &print;
	run;
	proc sql;
		create table dupl_toprint as
		select distinct TimesListed, a.*
		from &table a inner join dupl_print b
			on a.&var = b.&var
		order by TimesListed desc, &var;
	quit;
	%if &short = N %then %do;
		proc print data=dupl_toprint noobs;
			by &var notsorted;
			title "The distinct rows from [ &table ] with the top &print most repeated [ &var ] values";
		run;
	%end;
	%else %do;
		proc print data=dupl_counts noobs;
			title "[ &var ] values that appear 5 times or more";
			where TimesListed >= 5;
		run;
	%end;
%end;
%else %do;
	data _null_;
		file print;
		put "There are no rows from [ &table ] with duplicate values in [ &var ].";
	run;
%end;
title;
/*
proc sql;
	drop table dupl_counts;
	%if &duplyn > 1 %then %do;
		drop table dupl_print;
		drop table dupl_toprint;
		drop table summ_dupl_rep;
	%end;
quit;
*/
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2005-04*/
/*
o Get frequency distributions and save some typing
o 2017-10: This is the older version, left here just in case
*/

%macro freq(ds, vars, ttl=N, order=);
title;
proc freq data=&ds
	%if &order ne %then %do; order=freq %end;;
	%if &ttl ne N %then %do; title "&ttl"; %end;
	tables &vars / missing nocol norow nocum
		%if %index(&vars, *) %then %do; nopercent %end;;
run;
%if &ttl ne N %then %do; title; %end;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2017-10*/
/*
o This is a customized frequency report using an SQL query to produce a GROUP BY table,
  belatedly replacing the SAS PROC FREQ output that I have been using so far.
o The advantages include a much easier representation of multidimensional frequencies,
  which take the form of additional colums in one table, rather than a (long) suite of 2x2
  tables, plus a (much) more compact output by default.
o vars = list with the variables of interest, separated by spaces
*/

%macro mfreq(dset, vars, label=Count, ttl=, order=, bal=, N=, where=, pct=Y);
%local i;
%lst(&vars, v)
proc sql;
	create table _freqtmp as
	select %do i = 1 %to %eval(&nw - 1); &&v&i , %end; &&v&nw ,
		count(*) format comma20.0 as &label
		%if "&bal" ne "" %then %do; , sum(&bal) as &bal format dollar20.0 %end;
	from &dset
	&where
	group by %do i = 1 %to %eval(&nw - 1); &&v&i , %end; &&v&nw
	%if %upcase(&order) = FREQ %then %do;
		%if "&bal" = "" %then %do; order by calculated &label desc %end;
		%if "&bal" ne "" %then %do; order by calculated &bal desc %end;
	%end; ;
quit;
%if &pct = Y %then %do;
	%addpct(_freqtmp, &label)
	%if "&bal" ne "" %then %do; %addpct(_freqtmp, &bal) %end;
%end;
%if "&ttl" ne "" %then %do; title "&ttl"; %end;
%if "&where" ne "" %then %do; title2 "Subset: &where"; %end;
proc print data=_freqtmp %if "&N" ne "" %then %do; (obs=&N) %end; noobs; 
	sum &label %if &pct = Y %then %do; Pct&label %end; 
		%if "&bal" ne "" %then %do; &bal %if &pct = Y %then %do; Pct&bal %end; %end; ;
run;
title;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2004-05*/
/*
o This macro contains the Great Circle Distance Formula which computes the distance between
  two points given their latitude and longitude. 
o Details:
  - The computed distance varies depending on the assumed radius of the Earth.
    In miles, R = 3949.9 (polar), 3963.1676 (equatorial), 3956.5 (average), 3960 (MapInfo).
  - To get the distance in kilometers or inches, enter R in km/inches.
  - The formula used is the Great Circle Distance Formula.
  - E long and N lat should be positive, and W long and S lat should be negative.
*/

%macro gcdf(lat1, lon1, lat2, lon2);
/*the first number in the formula below is R, the radius of the Earth in miles*/
3960 * arcos(sin(&lat1/(180/(4*atan(1)))) *  sin(&lat2/(180/(4*atan(1)))) + 
	cos(&lat1/(180/(4*atan(1)))) * cos(&lat2/(180/(4*atan(1)))) * 
	cos(&lon2/(180/(4*atan(1))) - &lon1/(180/(4*atan(1)))))
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2004-05*/
/*
o This macro computes the distance to the nearest branch for each geocoded address from an input
  dataset. HHs/addresses that were not properly geocoded are discarded (see below why).
o Arguments:
  - dset1 = data set with customers and their lat & long (lat1, lon1)
  - dset2 = data set with branches and their lat & long (lat2, lon2)
  - id1 = the field from dset1 to keep, apart from distance (why not keep everything:
    for efficiency, as the Cartesian join can easily produce massive data sets)
  - output = the name of the output data set
o Usage:
%geodist(customers, branches, latitude, longitude, latitude, longitude, SSN)
*/

%macro geodist(dset1,dset2,lat1,lon1,lat2,lon2,id1);
/*Cartesian join... not accidental this time*/
proc sql;
	create table alldist as
	select distinct a.&id1, br_num,
		/*Note: I have seen many cases of failed geocoding, where the lat and long are assigned a value
		of 0. If the formula is allowed to go through in those cases, the distances produced are 
		very large (5000 miles or so).*/
		case when abs(a.&lat1) + abs(a.&lon1) > 0 then %gcdf(a.&lat1, a.&lon1, b.&lat2, b.&lon2) else . end as Miles
	from &dset1 a, &dset2 b
	order by a.&id1, Miles;
quit;
/*get rid of HHs that weren't properly geocoded*/
data shortestdist;
	set alldist;
	if Miles = . then delete;
run;
/*keep just the closest branch and shortest distance*/
data shortestdist;
	set shortestdist;
	by &id1;
	if first.&id1;
run;
/*a report*/
proc means data=shortestdist;
	var Miles;
run;
%univ(shortestdist, miles)
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2012-05*/
/*
o This macro determines the name of the latest copy of a file, and puts it into a
  global macro variable.
o Because working directly with file names is not possible in our SAS installation
  (no X commands), an uglier approach (but which works) is implemented here. The
  idea is to go back from today, day by day, and check if a file from that date is
  found. If yes, the looping stops.
o 2013-03: Added a new argument, daysback, which specifies the max number of days
  the macro will go back. The idea is to define and enforce a threshold after which
  the looping will stop. This is meant to avoid situations where a file does not
  exist at all (either because the name or location is incorrect, or maybe the file
  was moved), in which case an infinite loop would occur.
o Arguments:
  - fld = the folder where the files are saved
  - prefix, suffix - the parts of the file names before and after the date
  - fmt = the format used for the dates in the file names
*/

%macro getLatestFile(fld, prefix, fmt, suffix, daysback=360);
%global targetFileName;
%local attempt;
%let filefound = 0;
%let attempt = 0;
%put ===Search for the most recent &prefix.*&suffix file in folder &fld..;
%let filedate = %sysfunc(inputn(&sysdate, date9.));  /*the start date - today*/
%do %while (&filefound = 0);
	%let fnamedate = %sysfunc(putn(&filedate, &fmt));
	%let filenameshort = %sysfunc(cat(&prefix, &fnamedate, &suffix));
	/*%put >>> Looking for (short) &filenameshort;*/
	/*2013-03-25: %sysfunc fails if the path name includes dashes. This came up when working
	with ERM BPO files. As a workaround, stop using %sysfunc to assemble file names.*/
	/*2013-03-25: Per SAS tech support, quoting is needed to mask special characters in the folder
	name. This is done with %superq and %qsysfunc.*/
	/* %let filename = %sysfunc(cat(&fld, \, &prefix, &fnamedate, &suffix)); */
	/*%let filename = %qsysfunc(cats(%superq(fld), \, &prefix, &fnamedate, &suffix));*/
	%let filename = &fld.\&prefix.&fnamedate.&suffix;
	/*%put >>> which is (long) &filename;*/
	%if %sysfunc(fileexist(&filename)) %then %do;
		%let filefound = 1;
		%let targetFileName = %sysfunc(cat(&prefix, &fnamedate, &suffix));
		%put ===File &filenameshort found, and put into the macro var 'targetFileName':;
	%end;
	%else %do;
		%put &filenameshort not found;
		%let filedate = %eval(&filedate - 1);  /*go back one day*/
	%end;
	%let attempt = %eval(&attempt + 1);
	/*%put This is attempt # &attempt..;*/
	%if %eval(&attempt > &daysback) %then %do;
		%put ===Hit the max number of file searches allocated (&daysback). Aborting.;
		%let filefound = 1;
		/*Enforce a blank value for the file name. Otherwise, if the file is not found, then the value
		defined previously elsewhere would still be present, since this is a global variable. This actually
		happened on 2013-04-25 and it took me a while to figure out what had happened.*/
		%let targetFileName = ;
	%end;
%end;
%put &targetFileName;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2012-10*/
/*
o I got tired of repeatedly copying frequency tables to Excel, for plotting.
o This macro uses a previous macro, which had been deprecated for quite some time, to
  plot frequency counts.
*/

%macro gfreq(dset, var);
proc sql;
	create table temp_freq as
	select &var, count(*) as freq
	from &dset
	group by &var;
quit;
%addpct(temp_freq, freq)
proc sort data=temp_freq; by descending freq; run;
%gplot(temp_freq, freq, &var)
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2013-03*/
/*
o This macro saves some typing when creating macro variable names with date values.
  Starting from a given date, it goes back a given number of periods (e.g. months),
  chooses the beginning or end of that interval, and puts it into a global macro variable.
o Arguments:
  - date = the reference date
  - N = the number of intervals to move; negative values to go back in the past, positive
    values to go forward in the future, 0 to stay in the current interval
  - period = the name of the interval of interest; can be month, week, year, plus several
    others as detailed in the SAS documentation for intnx()
  - place = the alignment; can be beginning, middle, end
  - var = the name of the output global macro var
  - adj = any adjustment to apply to the starting date (e.g. +2, -4)
o Usage:
  %goback(&sysdate, -3, month, beginning, from)
  %goback(&sysdate, -1, month, end, to)
*/

%macro goback(date, N, period, place, var, fmt=date9., adj=0);
%global &var &var.iso;
data _null_;
	call symputx("&var", trim(left(put(intnx("&period", "&date"d + &adj, &N, "&place"), &fmt))));
	call symputx("&var.iso", trim(left(put(intnx("&period", "&date"d + &adj, &N, "&place"), yymmddn8.))));
run;
/*report the results in the log*/
%put === The following macro variables were created: === ;
%put &var = &&&var , &var.iso = &&&var.iso;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2004-08*/
/*
o This macro plots several variables from a data set on the same graph.
o Arguments:
  - dset = the data set
  - by = the column with the X values
  - vars = the columns to be plotted (Y values)
  - syms = the symbols for each line graph
  - labels = add the Y-axis values of the plotted points or not? (Y if yes)
o USAGE: 
  %gplot(dset,Yvar,var1 var2 var3,N,syms=a b c)
  %gplot(dset,Yvar,var1 var2 var3,Y)
  %gplot(dset,Yvar,var1 var2 var3,Y,syms=1 2 3,clrs=pink magenta blue)
*/

%macro gplot(dset, by, vars, log=, labels=N, ylab=, range=Y, rangeLab=, join=Y,
	syms=dot triangle square circle diamond, 
	clrs=red green blue orange brown yellow);
%local i;
/*the variables*/
data _null_;
	nwords=%cwords(&vars);
	call symputx('nw',trim(left(nwords)));                      
run;
data _null_;
	%do i=1 %to &nw;
	w&i=scan("&vars",&i);
	call symputx("v&i",trim(left(w&i)));
	%end;
run;
/*the symbols*/
data _null_;
	%do i=1 %to &nw;
	w&i=scan("&syms",&i);
	call symputx("s&i",trim(left(w&i)));
	%end;
run;
/*the colors*/
data _null_;
	%do i=1 %to &nw;
	w&i=scan("&clrs",&i);
	call symputx("c&i",trim(left(w&i)));
	%end;
run;
/*the boundaries of the plot area*/
/*
proc sql;
	create table boundaries as
	select min(%do i=1 %to %eval(&nw-1); min(&&v&i), %end; min(&&v&nw)) as vmin,
		max(%do i=1 %to %eval(&nw-1); max(&&v&i), %end; max(&&v&nw)) as vmax,
		min(&by) as hmin,
		max(&by) as hmax
	from &dset;
quit;
*/
goptions dev=png gsfmode=replace xpixels=900 ypixels=600 vpos=46 hpos=99;
%do i=1 %to &nw;
	symbol&i color=&&c&i %if &join = Y %then %do; interpol=join %end; value=&&s&i 
		%if &labels = Y %then %do; pointlabel=("&&v&i") %end; 
		height=0.8 width=0.8;
%end;

*add the range to the X label if requested;
%if &range = Y %then %do;
	%varInfo(&dset, &by)
	proc sql noprint;
		select min(&by) %if &varFormat = DATE %then %do; format date9. %end; into :Min from &dset;
		select max(&by) %if &varFormat = DATE %then %do; format date9. %end; into :Max from &dset;
	quit;
	%let ax2lab = &by (&rangeLab %trim(&min) - %trim(&max));
%end;
%else %do;
	%let ax2lab = &by;
%end;

*if there is one variable, label the Y axis;
%if &nw = 1 %then %do;
	%let ylab = &vars;
%end;

axis1 label=(angle=90"&ylab") %if %index(%upcase(&log), Y) %then %do; logbase=2 logstyle=expand %end;;
axis2 label=("&ax2lab") %if %index(%upcase(&log), X) %then %do; logbase=2 logstyle=expand %end;;
proc gplot data=&dset;
	plot (&vars) * &by / overlay %if &nw ne 1 %then %do; legend %end; vaxis=axis1 grid haxis=axis2 grid;
run;
quit;
goptions reset=all;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2006-01*/
/*
o This macro is similar to the UNIX shell utility 'head' - it prints the first rows from 
  a SAS data set or Oracle table.
o PROC SQL with the inobs option makes this macro very fast on all kinds of datasets, 
  including very large Oracle tables (where a PROC PRINT with OBS=x would run forever).
o 2008-02: changed this macro to first create a table with PROC SQL, then print it with 
  PROC PRINT. The problem is that if a variable has a label, PROC SQL displays the label
  instead of the name of the variable, and I didn't find a way to avoid this irritating behavior.
o Usage:
%head(mytable)	*print the default number of rows;
%head(mytable,r=200)	*print a specified number of rows;
*/

%macro head(dset,r=50);
proc sql noprint;
	select count(*) format comma20. into :totalobs from &dset;
quit;
proc sql inobs = &r;
	create table forhead_temppp as
	select *
	from &dset;
quit;
proc print data=forhead_temppp noobs; 
	title "The first &r out of &totalobs total rows from [ &dset ]";
run;
/*
proc sql;
	drop table forhead_temppp;
quit;
*/
title;
%mend;


/*previous version
proc sql inobs = &r;
	title "The first &r rows from [ &dset ]";
	select *
	from &dset;
quit;
title;
*/

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2004-05*/
/*
o Given a year, this macro outputs the dates for several holidays into a data set.
o 2011-11: The year doesn't have to be provided any more, instead the holidays from
  the past and next couple of years are generated automatically. In addition the year
  is provided as a field, for ease of joins to other tables where this info is needed.
*/

%macro holidays;
%local i;
data loopyears;
	do i = -5 to 2;
		year = put(intnx('year', "&sysdate"d, i), year.);
		output;
	end;
run;
%dnull(loopyears, year, N)
%do i = 1 %to &N;
	data holtemp (drop=FDO_FEB FDO_MAY FDO_SEP FDO_OCT FDO_NOV);
		/*New Year's Day*/
		FDOY=MDY(1,1, &&year&i);
		/* Martin Luther King Day */
		Martin_Luther_King_Day=INTNX('WEEK.2',FDOY,(WEEKDAY(FDOY) NE 2)+2);
		/* President's Day */
		FDO_FEB=INTNX('MONTH',FDOY,1);
		Presidents_Day=INTNX('WEEK.2',FDO_FEB,(WEEKDAY(FDO_FEB) NE 2)+2);
		/* Memorial Day */
		FDO_MAY=INTNX('MONTH',FDOY,4);
		Memorial_Day=INTNX('WEEK.2',FDO_MAY,(WEEKDAY(FDO_MAY) IN (1,7))+4);
		/* Independence Day */
		Independence_Day=MDY(7,4, &&year&i);
		/* Labor Day */
		FDO_SEP=INTNX('MONTH',FDOY,8);
		Labor_Day=INTNX('WEEK.2',FDO_SEP,(WEEKDAY(FDO_SEP) NE 2));
		/* Columbus Day */
		FDO_OCT=INTNX('MONTH',FDOY,9);
		Columbus_Day=INTNX('WEEK.2',FDO_OCT,(WEEKDAY(FDO_OCT) NE 2)+1);
		/* Veteran's Day */
		Veterans_Day=MDY(11,11, &&year&i);
		/* Thanksgiving Day */
		FDO_NOV=INTNX('MONTH',FDOY,10);
		Thanksgiving=INTNX('WEEK.5',FDO_NOV,(WEEKDAY(FDO_NOV) NE 5)+3);
		/* Christmas Day */
		Christmas=MDY(12,25, &&year&i);
		format _all_ date9.;
		rename FDOY = New_Years_Day;
	run;
	proc transpose data=holtemp out=holidays&&year&i;run;
	data holidays&&year&i;
		set holidays&&year&i;
		rename _name_ = HolidayName col1=HolidayDate;
		Year = "&&year&i";
	run;
%end;
data holidays;
	set %do i = 1 %to &N; holidays&&year&i %end;;
run;
proc sql;
	drop table holtemp;
	drop table loopyears;
	%do i = 1 %to &N;
		drop table holidays&&year&i;
	%end;
quit;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea aug04
This macro imports CSV files. CAUTION: 
o PROC IMPORT scans a limited number of rows before generating the data step code. 
  If strings longer than 8 characters occur later in the CSV file, they will be truncated.
o Variables that you need to have as character may be read as numeric.
  In such cases, tweak the DATA STEP code generated by PROC IMPORT.
o 2010-09: Added the capability to import pipe-delimted files, which happens when csv=N.
*/

%macro import(file, dset, csv=Y, fld=\\NYCB\corp$\DBMarketing\Bogdan\data, guess=32767);
proc import datafile="&fld.\&file" out=&dset replace;
	guessingrows=&guess;
	getnames=yes;
	%if &csv = Y %then %do; delimiter=","; %end;
	%else %do; delimiter="|"; %end;
run;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2014-07
o An EXTREMELY annoying deficiency of SAS PROC IMPORT is its inability to properly 
  import text fields. Because only up to 32k rows are scanned during the import, longer 
  text values occurring later in the file will be truncated. To address this once and for all, 
  implement the following algorithm.
o (a) Import the file as usual. This allows identifying the character fields. (b) Generate an
  import syntax with widths for all character fields set at a reasonably high threshold, say 200
  or more. (c) Reimport the file with the tweaked syntax and compute the max observed length of 
  each character field. (d) Finalize the generated import syntax with character field widths set
  to be wide enough - as observed in the data - but also to minimize space. (e) Reimport the file 
  with proper widths for the character fields, as it should have been imported all along, except 
  that SAS is incapable of, or not even interested in, writing a PROC IMPORT algorithm that is
  robust and imports data sets which are identical to the input files.
*/

%macro importWideCharFields(file, dset, codeFld=\\NYCB\corp$\DBMarketing\Bogdan\out, csv=Y);
%local i;
*Set up the delimiter to use. This kind of trick is needed because the comma can not be
passed as an argument to the macro;
%if &csv = Y %then %do; 
	%let dlm = delimiter=',';
%end;
%else %do; 
	%let dlm = delimiter='|'; 
%end;
%put Using this delimiter: &dlm;
*---A. Import the file as usual;
proc import datafile="&file" out=&dset replace;
	guessingrows=32767;
	getnames=yes;
	&dlm ;
run;
*determine which variables are character;
proc contents data=&dset noprint out=tmp_check_fmt; run;
data tmp_check_fmt;
	length vtype $ 4;
	set tmp_check_fmt;
	if Type = 1 then vtype = 'NUM';
		else if Type = 2 then vtype = 'CHAR';
run;
proc sort data=tmp_check_fmt; by varnum; run;
*---B. generate the modified import syntax, with very wide character fields;
%dnull(tmp_check_fmt, Name vtype informat informl, Nvars)
data _null_;
	title;
	file "&codeFld.\import_with_wide_char_fields.sas";
	put "/* ===Generated import code with very wide character fields=== */";
	put " ";
	put "data &dset;";
	put "infile '&file' &dlm MISSOVER DSD lrecl=32767 firstobs=2;";
	%do i = 1 %to &Nvars;
		/*NB: while date variables are lumped into NUM, their separate informat must be kept*/
		%if &&vtype&i = NUM %then %do;
			%if &&informat&i ne BEST %then %do;
				put "informat &&name&i &&informat&i..&&informl&i... ;";
			%end;
			%else %do;
				put "informat &&name&i 32. ;";
			%end;
		%end;
		/*use an arbitrarily wide informat here*/
		%if &&vtype&i = CHAR %then %do;
			put "informat &&name&i $200. ;";
		%end;
	%end;
	put " ";
	put "input";
	%do i = 1 %to &Nvars;
		%if &&vtype&i = NUM %then %do;
			put "&&name&i ";
		%end;
		%if &&vtype&i = CHAR %then %do;
			put "&&name&i $ ";
		%end;
	%end;
	put "; ";
	put "run;"; 
run;

*---C. Reimport the file and compute the max width for each character field;
%include "&codeFld.\import_with_wide_char_fields.sas";
*get again the contents of the file;
proc contents data=&dset noprint out=tmp_check_fmt; run;
data tmp_check_fmt;
	length vtype $ 4;
	set tmp_check_fmt;
	if Type = 1 then vtype = 'NUM';
		else if Type = 2 then vtype = 'CHAR';
run;
proc sort data=tmp_check_fmt; by varnum; run;
%dnull(tmp_check_fmt, Name vtype, Nvars)
*retrieve the max lengths of each character field;
proc sql;
	create table maxlen as
	%do i = 1 %to &Nvars;
		%if &&vtype&i = NUM %then %do;
			select "&&name&i" as var, 8 as MaxLen
			from &dset
			union
		%end;
		%if &&vtype&i = CHAR %then %do;
			select "&&name&i" as var, max(lengthc(trim(left(&&name&i)))) as MaxLen
			from &dset
			union
		%end;
	%end;
	select " " as var, 0 as MaxLen
	from &dset;
quit;
data maxlen;
	set maxlen;
	if missing(var) then delete;
run;
*adjust the table of contents;
proc sql;
	create table tmp_check_fmt2 as
	select a.*, MaxLen
	from tmp_check_fmt a left join maxlen b
		on Name = var
	order by varnum;
quit;

*---D. Finally, generate the adjusted import syntax;
%dnull(tmp_check_fmt2, Name vtype informat informl maxlen, Nvars)
data _null_;
	title;
	file "&codeFld.\import_with_adj_char_fields.sas";
	put "/* ===Generated import code with data-driven character widths=== */";
	put " ";
	put "data &dset;";
	put "infile '&file' &dlm MISSOVER DSD lrecl=32767 firstobs=2;";
	%do i = 1 %to &Nvars;
		/*NB: while date variables are lumped into NUM, their separate informat must be kept*/
		%if &&vtype&i = NUM %then %do;
			%if &&informat&i ne BEST %then %do;
				put "informat &&name&i &&informat&i..&&informl&i... ;";
			%end;
			%else %do;
				put "informat &&name&i 32. ;";
			%end;
		%end;
		/*use an arbitrarily wide informat here*/
		%if &&vtype&i = CHAR %then %do;
			put "informat &&name&i $&&maxlen&i... ;";
		%end;
	%end;
	put " ";
	put "input";
	%do i = 1 %to &Nvars;
		%if &&vtype&i = NUM %then %do;
			put "&&name&i ";
		%end;
		%if &&vtype&i = CHAR %then %do;
			put "&&name&i $ ";
		%end;
	%end;
	put "; ";
	put "run;"; 
run;
*---E. THE END: re-import the file, as it should have been imported from the beginning;
%include "&codeFld.\import_with_adj_char_fields.sas";
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2005-10*/
/*this macro saves some typing for inner joins

tab1, tab2 = the tables to join
by1, by2 = the fields used to do the inner join ; if only by1 is provided,
	the join is done on the same field (&by1), otherwise the join is done 
	on &by1 = &by2
out = the name of the output data set, containing all fields from &tab1 and none from &tab2

USAGE: 
%innerjoin(dset1,dset2,SSN,,dset3)
%innerjoin(dset1,dset2,SSN,HSHLDMBR_TAX_ID,dset3)
*/

%macro innerjoin(tab1,tab2,by1,by2,out);
proc sql;
	create table &out as
	select distinct a.*
	from &tab1 a inner join &tab2 b
	%if &by2 = %then %do;
			on a.&by1 = b.&by1
	%end;
	%else %do;
			on a.&by1 = b.&by2
	%end;
	order by &by1;
quit;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2012-04*/
/*
o This macro displays in the log some info about the current SAS job.
o Can be useful for monitoring and troubleshooting batch jobs.
*/

%macro jobinfo;
%put ====== SAS=&sysvlong server=&SASSERVER user=&SYSUSERID PID=&SYSJOBID %sysfunc(Date(),date9.) %sysfunc(Time(),time.) ======;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2007-01*/
/*
o This macro compresses all/most uncompressed datasets from a SAS library. Compression for some
  datasets may be disabled, in case compression overhead would increase the size of the data set.
o Arguments:
  - lib = the library with the datasets to be compressed
  - compression = the compression method (one of CHAR or BINARY). BINARY is highly effective for 
  compressing medium to large (several hundred bytes or larger) blocks of binary data (numeric variables). 
  Because the compression function operates on a single record at a time, the record length needs to be several
  hundred bytes or larger for effective compression.
o Usage:
  %libcompress(mylib)
*/

%macro libcompress(lib,compression=CHAR);
%local i;
%libcont(&lib,out=temp_for_dset_compress,message=(Before Compression))
/*what to compress*/
data temp_for_dset_compress;
	set temp_for_dset_compress;
	where compress = 'NO';
run;
proc sql noprint;
	select trim(left(put(count(*),8.))) into :nfiles
	from temp_for_dset_compress;
	select memname into :file1-:file&nfiles
	from temp_for_dset_compress;
quit;
/*compress*/
%if &nfiles = 0 %then %do;
data _null_;
	file print;
	put "There are no uncompressed datasets in library %upcase(&lib). Aborting.";
run;
%end;
%else %do;
	%do i = 1 %to &nfiles;
		data &lib..&&file&i (compress=&compression);
			set &lib..&&file&i;
		run;
	%end;
%end;
%libcont(&lib,message=(After Compression))
proc sql;
	drop table temp_for_dset_compress;
quit;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2004-05*/
/*
o This macro exports the contents of a library to a dataset of your choice and/or prints a report.
o Usage:
  %libcont(mylib)  *view the report;
  %libcont(mylib,mydatasets)  *view the report and export the info to a dataset;
*/

%macro libcont(lib, out=, message=, like=);
proc sql;
	/*put all tables into a dataset*/
	create table temp_lib_cont as
	select *
	from dictionary.tables
	where libname=upcase("&lib")
		and memtype = 'DATA'
		%if &like ne %then %do; and upcase(memname) like "&like" %end; ;
	/*and print a report*/
	title "SAS datasets from library %upcase(&lib) &message";
	footnote 'Negative Percent Compression indicates cases where compression actually increased the file size.';
	select memname,
		/*user-friendly file size (reported value is bytes)*/
		filesize / (1024*1024) as filesize label 'File Size (MB)' format comma20.1,
		modate, nobs format comma20.0
	from temp_lib_cont
	order by memname;
quit;
title; footnote;
%if &out ne %then %do;
data &out;
	set temp_lib_cont;
run;
%end;
proc sql;
	drop table temp_lib_cont;
quit;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2008-04*/
/*
o This macro does LOCF (last observation carried forward) missing value replacements. Given a table
  properly sorted, any missing values of a given variable will be replaced with the last known value
  from a previous row above.
o 2019-06: Added a BY argument. If given, then LOCF imputation is only applied within each BY level.
o Arguments:
  - dset, var = the input dataset and the variable where the missing value replacements are to be done
  - out = output dataset
*/

%macro locf(dset, var, out, BY=);
data &out;
	retain prvxx&var;  /*prvxx chosen here to minimize the risk of variable name collisions*/
	set &dset;
	%if &BY ne %then %do;
		by &BY;
		if first.&BY then prvxx&var = . ;
	%end;
	if missing(&var) then &var = prvxx&var;
	prvxx&var = &var;
	*drop prvxx&var;
run;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2011-08*/
/*
o I got tired of searching for, copying and adapting the same PROC SQL syntax that's
  quite useful for creating macro variables for macro looping, and put together this
  rather ugly-looking macro syntax.
o Given a data set and a column name, the macro pulls all distinct values of that variable
  and assigns them to global macro variables.
o The information required for looping consists of N&var (the number of distinct values)
  and &var1, &var2, ... So if the variable of interest is named State, then the macro
  will create the global variables NState (= the number of states) and State1, State2, ...
o Subsequently, the macro looping can be done as follows:
  %do i=1 %to &NState;
    ..... &&State&i .....
   %end;
*/

%macro loopingvalues(dset, var);
%local i;
%global N&var;
proc sql noprint;
	select trim(left(put(count(distinct &var), 8.))) into :N&var
	from &dset
	where &var is not missing;
quit;
/*NB: the input dataset might be empty*/
%if &&N&var ne 0 %then %do;
	%do i = 1 %to &&N&var;
		%global &var.&i ;
	%end;
	proc sql noprint;
		select distinct &var into :&var.1-:&var.&&N&var
		from &dset
		where &var is not missing;
	quit;
%end;
/*report the results in the log*/
%put === The following macro variables were created: === ;
%put N&var = &&N&var;
%if &&N&var ne 0 %then %do;
	%do i = 1 %to &&N&var;
		%put &var.&i = &&&var.&i ;
	%end;
%end;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea Sep 2004*/
/*
o This macro takes a list of words and puts each word into a GLOBAL macro variable.
o Arguments:
  - list = the list of words
  - name = the root name for the global macro vars
o Usage: 
  %lst(one two 3,myvar)
  will create the global macro vars nw, myvar1, myvar2 and myvar3, where nw = the # of words in &list
o CAUTION: pay attention to macro variable collisions, which will occur if you have 
  a macro variable &nw or some other macro vars with names like &myvar3.
o 2017-03: Enforced blank as the default delimiter, as words/values with decimals are incorrectly
  split into words otherwise. Also removed converting to character and rounding the values.
*/

%macro lst(list, name, log=Y);
%local i;
data _null_;
	nwords=%cwords(&list);
	call execute('%global nw'||';');
	call symputx('nw',trim(left(put(nwords,10.))));
run;
data _null_;
	%do i=1 %to &nw;
	w&i=scan("&list", &i, " ");
	call execute('%global &name.'||trim(left(put(&i,10.)))||';');
	*call symput("&name&i",trim(left(put(w&i,50.))));
	call symputx("&name&i", w&i);
	%end;
run;
/*report the results in the log*/
%if &log = Y %then %do;
	%put === The following macro variables were created: === ;
	%put nw = &nw;
	%do i=1 %to &nw;
		%put &name.&i = "&&&name.&i" ;
	%end;
%end;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2010-02*/
/*
o This macro is intended to minimize a quite large dataset while preserving the missing
  values information. This is done by replacing all known numerics with 1, and all known
  character values with 'v'. While the former doesn't reduce the size of a SAS dataset
  unless less than 8 bytes are allocated for storage (4 would be fine for small numbers),
  it does reduce the size of the dataset exported in text format, which is what I'm after.
o The resulting dataset is meant to be exported to text and imported in R. The ultimate
  objective is to run a cluster analysis on all variables that depicts similarities in the
  ways in which observations are missing (naclus() from Hmisc).
o I already have a good macro that reports the missing values in SAS, but not for clustering
  the fraction of missings in common between any two variables, and it's easier to just move
  the data to R.
*/

%macro missinfo(dsname, out);
%local i;
%local lib dset;
%if %index(&dsname,.)=0 %then %do; 
	%let lib=WORK;
	%let dset=%upcase(&dsname);
%end;
%else %do; 
	%let lib=%upcase(%scan(&dsname,1,"."));
	%let dset=%upcase(%scan(&dsname,2,"."));
%end;
proc sql noprint;
	create table vars_miss as
	select libname, memname, memtype, name, type, varnum, label
	from dictionary.columns
	where libname="&lib" and memname="&dset";
quit;
/*set up the names for the counter variables - up to 32 bytes*/
data vars_miss;
	set vars_miss;
	len = length(name);
	if len <= 31 then CountVar = cats('c', trim(left(name)));
		else CountVar = cats('c', substr(trim(left(name)), 1, 31));
run;
proc sql noprint;
	select trim(left(put(count(*),8.))) into :nvars from vars_miss;
	select name into :varname1-:varname&nvars from vars_miss;
	select label into :varlab1-:varlab&nvars from vars_miss;
	select type into :vartype1-:vartype&nvars from vars_miss;
	select CountVar into :count1-:count&nvars from vars_miss;
	select count(*) into :TotalObs from &dsname;
quit;
/*flag the missings for each variable*/
data &out;
	set &dsname;
	/*remove all labels, formats and informats*/
	attrib _all_ label='';
	format _all_;
	informat _all_;
	%do i=1 %to &nvars;
		%if &&vartype&i = num %then %do;
			if missing(&&varname&i) then &&varname&i = .;
				else &&varname&i = 1;
		%end;
		%if &&vartype&i = char %then %do;
			if missing(&&varname&i) then &&varname&i = ' ';
				else &&varname&i = 'v';
		%end;
	%end;
run;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2004-06, upgraded 2010-02*/
/*
o This macro prints a missing value report for all variables in a data set.
o 2010-02: I completely overhauled the previous version to make it run faster on large
  SAS data sets. Instead of running one SQL query per column as before, the upgraded
  version reads the entire table only once with a data step, and counts the number of
  missing values along the way. The previous version, preserved under a different name
  (missingsql.sas), might be preferable when dealing with DBMS tables.
o 2017-06: Added a subtitle stating when no missing values are present. 
o Usage: %missing(lib.dataset)
*/

%macro missing(dsname, out=MissingCounts);
%local lib dset i;
%if %index(&dsname,.)=0 %then %do; 
	%let lib=WORK;
	%let dset=%upcase(&dsname);
%end;
%else %do; 
	%let lib=%upcase(%scan(&dsname,1,"."));
	%let dset=%upcase(%scan(&dsname,2,"."));
%end;
proc sql noprint;
	create table vars_miss as
	select libname, memname, memtype, name, type, varnum, label, format, length
	from dictionary.columns
	where libname="&lib" and memname="&dset";
quit;
/*set up the names for the counter variables - up to 32 bytes*/
data vars_miss;
	set vars_miss;
	len = length(name);
	if len <= 31 then CountVar = cats('c', trim(left(name)));
		else CountVar = cats('c', substr(trim(left(name)), 1, 31));
run;
proc sql noprint;
	select trim(left(put(count(*),8.))) into :nvars from vars_miss;
	select name into :varname1-:varname&nvars from vars_miss;
	select label into :varlab1-:varlab&nvars from vars_miss;
	select type into :vartype1-:vartype&nvars from vars_miss;
	select CountVar into :count1-:count&nvars from vars_miss;
	select count(*) into :TotalObs from &dsname;
quit;
/*count the missings for each variable*/
data count_missings;
	retain %do i=1 %to &nvars; &&count&i %end;;
	set &dsname end=lastobs;
	keep %do i=1 %to &nvars; &&count&i %end;;
	%do i=1 %to &nvars;
		if missing(&&varname&i) then &&count&i = sum(&&count&i, 1);
	%end;
	if lastobs then output;
run;
/*report*/
proc transpose data=count_missings out=count_missings2; run;
proc sql;
	create table &out as
	select Name, &TotalObs as N, max(Col1, 0) as Missing,
		&TotalObs - calculated Missing as Populated,
		calculated Missing / calculated N as PctMissing, type, varnum, format, length
	from count_missings2 a left join vars_miss b
		on _name_ = CountVar
	order by PctMissing desc, name;
quit;
/*how many vars have missing values?*/
proc sql noprint;
	select count(*) into :VM
	from &out
	where Missing > 0;
quit;
proc print data=&out noobs label;
	title "Missing Values Report for [ %upcase(&dsname) ]";
	%if &VM = 0 %then %do;
		title2 "===There are no variables with missing values.";
	%end;
	var name type length varnum N Populated Missing PctMissing;
	label Name='Field' N='Cases' type='Type' varnum='VarNum';
	format PctMissing percent10.2 N Missing Populated comma30.0;
run;
title;
proc sql;
	drop table vars_miss;
	drop table count_missings;
	drop table count_missings2;
quit;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2004-06*/
/*
o This macro prints a missing value report for all variables in a data set.
o 2010-02: I completely changed this macro to make it run faster on SAS data sets.
  The issue was the long time it took to examine large data sets on ADM. Instead of
  running N queries on the entire table, the new version reads the table only once,
  and adds the number of missing values along the way. The old version is preserved
  here under a different name because it could be still useful for DBMS tables that
  either can't or shouldn't be read with a data step.
o Usage: %missing(lib.dataset)
*/

%macro missingsql(dsname);
%local lib dset i;
%if %index(&dsname,.)=0 %then %do;
	%let lib=WORK;
	%let dset=%upcase(&dsname);
%end;
%else %do; 
	%let lib=%upcase(%scan(&dsname,1,"."));
	%let dset=%upcase(%scan(&dsname,2,"."));
%end;
proc sql noprint;
	/*the variables ...*/
	create table vars_miss as
	select libname,memname,memtype,name,type,varnum,label
	from dictionary.columns
	where libname="&lib" and memname="&dset";
	/*... into macro variables*/
	select trim(left(put(count(*),8.))) into :nvars from vars_miss;
	select name into :varname1-:varname&nvars from vars_miss;
	select label into :varlab1-:varlab&nvars from vars_miss;
	select type into :vartype1-:vartype&nvars from vars_miss;
quit;
/*missing counts for each variable*/
%do i=1 %to &nvars;
	proc sql noprint;
		select count(*) into :missing
		from &dsname
		where &&varname&i is null;
	quit;
	data temp_miss&i;
		length type $ 4 variable $ 32 label $ 80;
		Missing = &missing;
		Variable = "&&varname&i";
		Type = "&&vartype&i";
		Label = "&&varlab&i";
	run;
%end;
proc sql noprint;
	select count(*) into :obs
	from &dsname;
quit;
/*all together*/
data report_miss;
	set %do i=1 %to &nvars; temp_miss&i %end;;
run;
/*report*/
data report_miss;
	set report_miss;
	N = &obs;
	PctMissing = Missing / N;
	format N comma20.0 PctMissing percent10.2;
run;
proc sort data=report_miss;
	by descending type descending PctMissing;
run;
proc print data=report_miss noobs label;
	title "Missing Values Report for [ %upcase(&dsname) ]";
	by type notsorted;
	var variable N PctMissing;
	label PctMissing = 'Missing' type='Variable Type';
run;
title;
/*cleanup*/
proc sql;
	%do i=1 %to &nvars; drop table temp_miss&i; %end;
quit;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2010-02*/
/*
o This macro computes and adds a variable counting the total number of missing
  values per observation for a given data set. The intention is to group appended
  data (e.g. Acxiom) at HH level by keeping the info for the member with the
  lowest number of missing values.
*/

%macro missperobs(dsname);
%local lib dset i j;
%if %index(&dsname, .)=0 %then %do; 
	%let lib=WORK;
	%let dset=%upcase(&dsname);
%end;
%else %do;
	%let lib=%upcase(%scan(&dsname,1,"."));
	%let dset=%upcase(%scan(&dsname,2,"."));
%end;
/*---the numeric and character vars into macro vars*/
proc sql noprint;
	create table varstoscan as
	select libname, memname, memtype, name, type, varnum, label
	from dictionary.columns
	where libname = "&lib" and memname = "&dset" and type = "num";
	select trim(left(put(count(*),8.))) into :numvars from varstoscan;
	select name into :numeric1-:numeric&numvars from varstoscan;
	create table varstoscan as
	select libname, memname, memtype, name, type, varnum, label
	from dictionary.columns
	where libname = "&lib" and memname = "&dset" and type = "char";
	select trim(left(put(count(*),8.))) into :charvars from varstoscan;
	select name into :character1-:character&charvars from varstoscan;
	drop table varstoscan;
quit;
/*---count the number of missing values per observation*/
data &dsname;
	set &dsname;
	MissPerObs = sum(
	%if &numvars ne 0 %then %do;
		%do i = 1 %to %eval(&numvars - 1); (&&numeric&i = .) , %end; (&&numeric&numvars = .) ,
	%end;
	%if &charvars ne 0 %then %do;
		%do j = 1 %to %eval(&charvars - 1); (&&character&j = " ") , %end; (&&character&charvars = " ")
	%end;
	%if &charvars eq 0 %then %do;
		0
	%end;
	);
run;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2007-04*/
/*
o This macro computes the Exponential or Simple Moving Average of a variable for a given lag
  for each account/HH/etc identified through the variable &by.
o R syntax for EMA validation:
  srs <- read.delim(file='clipboard', na.strings=c(".","","NA"), header=FALSE)
  data.frame(Rema=ema(srs[,1], 10))
o Arguments:
  - dset/out = input/output datasets
  - by = the variable indicating the subjects (accounts, HHs etc) for which repeated
    measures were taken at regular intervals
  - var = the quantity measured at several points in time, for which the EMA will be computed
  - lag = the EMA lag
  - ema = the name of the EMA variable
  - type = whether the computed average should be SIMple or EXPonential
o The input dataset is obviously assumed to be sorted by &by, and in proper chronological order.
*/

%macro movingavg(dset, out, by, var, lag, ema, type=SIM);
%local i;
data &out;
	retain &ema;
	set &dset;
	k = 2/(1 + &lag);  /*the EMA smoothing factor*/
	/*Higher lags lead to lower smoothing factors = lower weights to apply to the last value,
		and therefore produce a greater degree of smoothing.*/
	by &by;
	if first.&by then counter = 1;
		else counter + 1;
	/*store the lags needed for the EMA start = the regular average of the first &lag values*/
	%do i = 1 %to %eval(&lag - 1);
		lg&i = lag&i(&var);
		if counter <= &i then lg&i = .;
	%end;
	/*compute MA, simple or exponential*/
	if counter < &lag then &ema = .;
	%if &type = EXP %then %do;
		/*EMA start: the regular average of the last &lag values*/
		else if counter = &lag then &ema = mean(%do i = 1 %to %eval(&lag - 1); lg&i, %end; &var);
		/*EMA formula*/
		/*note: ema after '=' is the previous ema value, due to the retain statement*/
		else &ema = &ema + k * (&var - &ema);
	%end;
	%if &type = SIM %then %do;
		else &ema = mean(%do i = 1 %to %eval(&lag - 1); lg&i, %end; &var);
	%end;
	drop k counter %do i = 1 %to %eval(&lag - 1); lg&i %end;;
run;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2012-05*/
/*
o This macro creates empty files whose names follow a certain naming convention.
  It is meant to be called at the end of a scheduled job, with the intention of
  using these files to track jobs which were scheduled but did not finish running
  as expected.
o Creating such files to monitor job execution is not the most elegant approach, but
  after asking SAS tech support it emerged that the alternative (inserting rows in
  a table) cannot be made concurrent in SAS 9.1 without SAS/Share (a product that
  offers concurrent access and likely costs $$$). So to make sure dataset collisions
  between SAS jobs are avoided, create such files to confirm program execution.
o 2019-10: The SAS server is now specified through a macro variable. This is defined
  in the batch_job.sas file.
*/

%macro newRunFile(jobID, fld=\\&SASserver.\BI\0.CommonFiles\BI_code\recurring_tasks\job_monitor);
data _null_;
	file "&fld.\job_&jobID._%sysfunc(Date(),yymmddn8.)_%sysfunc(compress(%sysfunc(Time(),hhmm.),:))";
run;
*2016-06: Log the time at which the job finished running.;
%timenowlog(This job finished running at)
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2010-02*/
/*Delete all variable labels from a data set.*/

%macro nolabels(dset);
data &dset;
	set &dset; 
	attrib _all_ label='';
run;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2004-09
o This macro gets the date/time as of the execution of this macro - and not as of 
when the SAS session was started. Purpose: display time in the log.
o 2020-08: Finally added support for displaying the message in the output window as
  well.
*/

%macro now(message, out=N);
%put ========= &message: %sysfunc(Date(),date9.)_%sysfunc(Time(),time.) =========;
%if &out ne N %then %do;
	data _null_;
		file print;
		put "========= &message: %sysfunc(Date(),date9.)_%sysfunc(Time(),time.) =========";
	run;
%end;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2020-07
o This macro is meant to support other macros that run pass-through queries against Oracle. 
  The connection info for those "connect to oracle" statements is defined based on the schema names.
o As a prerequisite, the connection info for each schema must exist already as macro variables with
  the names connDNA, connIVUE, etc.
*/

%macro OracleConn(tb);
%global connInfo;
*DNA and Open Data;
%if %index(%lowcase(&tb), osibank) or %index(%lowcase(&tb), eis_dw) %then %do;
	%let connInfo = &connDNA;
%end;
*iVue;
%if %index(%lowcase(&tb), informent) %then %do;
	%let connInfo = &connIVUE;
%end;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2020-02
o This macro runs some native Oracle queries that report all existing tables and views 
  in a given schema, along with the number of records (but only for tables).
o If the schema is blank
*/

%macro OracleTOC(schema, conn=);
%sqlString(&schema, vname=schm, quotes=Y)
*Determine the connection info to use for Oracle pass-through queries, based on the schema name;
%OracleConn(&schema)

proc sql;
	connect to oracle (&connInfo);
	/*all schemas in there*/
	/*
	create table schemas as
	select * from connection to oracle
	(select USERNAME 
	from SYS.ALL_USERS 
	order by USERNAME) ;  */

	/*get all tables and views*/
	create table tbvw as
	select * from connection to oracle
	(select TABLE_NAME "Table", OWNER, 'Table' "Type", NUM_ROWS
	from SYS.ALL_TABLES 
	where upper(OWNER) = upper(&schm)
	union all
	select VIEW_NAME "Table", OWNER, 'View' "Type", NULL "NUM_ROWS"
	from SYS.ALL_VIEWS 
	where upper(OWNER) = upper(&schm)
	order by "Table") ;
	disconnect from oracle;
quit;
proc print data=tbvw;
	title "All tables and views in &schema as of &sysdate9";
	format NUM_ROWS comma20.0;
run;
title;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2014-07
o This is meant to just save some typing.
o 2020-05: Added a confirmation for the cases when a dataset is empty. This is far
  better than saying nothing: in many cases it really is not clear whether something
  broke or was overlooked, or no records/results are present.
*/

%macro pr(dset, r=, ttl=);
proc sql noprint;
	select trim(left(put(count(*), 8.))) into :N from &dset;
quit;
%if &N ne 0 %then %do;
	title "&ttl";
	proc print data=&dset %if &r ne %then %do; (obs=&r) %end; noobs; run;
%end;
%if &N = 0 %then %do;
	data _null_;
		file print;
		put "&ttl : NO RECORDS";
	run;
%end;
title;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2012-10*/
/*
o Compute and plot the percentiles for a given distribution.
o While SAS/Graph is 100% inferior to R, this kind of code makes possible
  some quick checks without having to export the data to R every time.
*/

%macro qpl(dset, var, log=);
%local i;
proc univariate data=&dset noprint;
	var &var;
	output out=pctls pctlpts = %do i = 0 %to 100; &i %end;
	pctlpre = p pctlname = %do i = 0 %to 100; c&i %end;;
run;
proc transpose data=pctls out=pctls2; run;
data pctls3;
	set pctls2;
	rename col1 = &var;
	percentile = input(scan(_label_, 2), 10.);
run;
*customize the title: show the sample size and the number of missing values;
proc sql noprint;
	select count(*) format comma20.0 into :sample from &dset where &var is not missing;
	select count(*) format comma20.0 into :missing from &dset where &var is missing;
quit;
title2 "Percentiles for &var (&sample values, &missing missing values)";
%gplot(pctls3, percentile, &var, log=&log)
title;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2013-03*/
/*
o This macro adds a variable Quarter with the format YYYYQN, where N = 1 to 4.
*/

%macro quarterDate(dset, date);
data &dset;
	set &dset;
	if &date ne . then do;
		temp_mth = month(&date);
		if temp_mth >= 1 and temp_mth <= 3 then temp_quart = 1;
			else if temp_mth >= 4 and temp_mth <= 6 then temp_quart = 2;
			else if temp_mth >= 7 and temp_mth <= 9 then temp_quart = 3;
			else if temp_mth >= 10 and temp_mth <= 12 then temp_quart = 4;
		Quarter = cats(year(&date), 'Q', temp_quart);
	end;
	drop temp_mth temp_quart;
run;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2014-07
o This macro saves some typing when pulling random samples.
*/

%macro randomSample(dset, N, out);
proc surveyselect data=&dset out=&out method=srs n=&N;
	/*strata tel_nbr;*/
run;  
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea sep05*/
/*get the range of a variable from a table*/
/*if fmt=D, the values are formatted as dates*/

/*USAGE:
%range(data,variable)
%range(data,date_variable,fmt=D)
*/

%macro range(dset, var, ttl=);
title "Range of variable [ &var ] from table [ &dset ] &ttl";
%varInfo(&dset, &var)
proc sql;
	select min(&var) %if &varFormat = DATE %then %do; format date9. %end; as Min,
		max(&var) %if &varFormat = DATE %then %do; format date9. %end; as Max
	from &dset;
quit;
title;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2010-10*/
/*
o This macro helps translate range variables (such as income, age) to scale variables by
  generating code that replaces a given range with its middle point. The issue is that
  range variables, quite common in purchased data, can't be used unless they're translated
  first to values that indicate the rank as well as the size of the differences between them.
o Arguments:
  - dset, vars = the data set and the variables of interest
  - sep = the character that separates the start and the end of the intervals
o Output:
  - SAS code that may need to be adjusted
*/

%macro rangerecode(dset, vars, sep, del);
%local i;
options ls=max;
%lst(&vars, vrbl)
%do i=1 %to &nw;
	/*get the intervals and compute the mid points*/
	proc sql;
		create table values as
		select distinct &&vrbl&i as range
		from &dset;
	quit;
	/*generate and print the code*/
	data values2;
		length code $ 300;
		set values;
		from = compress(scan(range, 1, "&sep"), "$,");
		to = compress(scan(range, 2, "&sep"), "$,");
		mid = round((input(from, 20.) + input(to, 20.)) / 2);
		code = "if trim(left(&&vrbl&i)) = '" || trim(left(range)) || "' then N&&vrbl&i = " || trim(left(mid)) || ';';
	run;
	proc sort data=values2; by mid; run;
	data _null_;
		set values2 end=last;
		file print;
		put code;
		if last then do;
			put "drop &&vrbl&i ;";
			put "rename N&&vrbl&i = &&vrbl&i ;";
		end;
	run;
%end;
options ls=80;
%mend;

/*----------------------------------------------------------------------------------*/


/*bogdan romocea 2007-03, updated 2010-02*/
/*
o This macro replaces the missing values of all numeric variables from a dataset
  with a given value (say 0).
o 2010-02: Added the capability to replace any value (including missing) with any
  other value, for all numeric or character variables. This was motivated by the
  need to replace _ with missing for a bunch of character variables.
o Arguments:
  - dsname = the name of the input dataset
  - out = the name of the output dataset
  - type = the type of the variables to be processed; must be num or char
  - replace = the value to replace
  - with = the value that will replace the given value
  - except = variables where the replacement should not occur
o Usage: 
%replacevalue(mydata, mydata2, num, ., 0) *replace all missing numerics with 0;
%replacevalue(mydata, mydata2, char, _, ) *replace all '_' with missing values;
*/

%macro replacevalue(dsname, out, type, replace, with, except=(''));
%local lib dset i;
%if %index(&dsname, .)=0 %then %do; 
	%let lib=WORK;
	%let dset=%upcase(&dsname);
%end;
%else %do;
	%let lib=%upcase(%scan(&dsname,1,"."));
	%let dset=%upcase(%scan(&dsname,2,"."));
%end;
proc sql noprint;
	/*the variables ...*/
	create table varstoscan as
	select libname, memname, memtype, name, type, varnum, label
	from dictionary.columns
	where libname = "&lib" 
		and memname = "&dset" 
		and type = "&type"
		and name not in &except;
	/*... into macro variables*/
	select trim(left(put(count(*),8.))) into :nvars from varstoscan;
	select name into :varname1-:varname&nvars from varstoscan;
quit;
/*replace missing values*/
%put The following statements were generated: ;
data &out;
	set &dsname;
	%do i=1 %to &nvars;
		%if &type = num %then %do;
			if &&varname&i = &replace then &&varname&i = &with;
			%put if &&varname&i = &replace then &&varname&i = &with;
		%end;
		%if &type = char %then %do;
			if trim(left(&&varname&i)) = "&replace" then &&varname&i = "&with";
			%put if trim(left(&&varname&i)) = "&replace" then &&varname&i = "&with";
		%end;
	%end;
run;
/*cleanup*/
proc sql;
	drop table varstoscan;
quit;
%mend;

/*----------------------------------------------------------------------------------*/

/*BR 2019-08*/
/*
o Prepare a summary report showing the row counts from an Oracle table, for a given list of fields.
  The &connInfo macro variable (user + password + path) is assumed to exist.
o Before running this, specify the list of fields separated by commas (%let fields = A, B, C). 
  This cannot be passed as an argument because of the commas (yet another brilliant SAS feature).
*/

%macro rowCounts(tb, columns, out=rep, order=, where=);
*Wow. Oracle is VERY, VERY picky when it comes to quotes. It simply does not accept
the double ones. Single ones are fine, but then the SAS macro vars do not resolve. To
make both work, assemble the precise string required in advance.;
data _null_;
	oraLab = cats("'", "&tb", "'");
	call symputx('oraLab', oraLab);
run;
%sqlstring(&columns, vname=fields, quotes=N)
%OracleConn(&tb)  /*choose the proper connInfo*/
proc sql;
	connect to oracle (&connInfo 
	/*path="(description = (address = (protocol= tcp)(host=172.16.30.92)(Port= 41443))(connect_data = (SID = TCS1443V)))"*/);

	create table &out as select * from connection to oracle
	(select &oraLab "Table", &fields, count(*) "Rows" 
		from &tb
		&where 
		group by &fields
		order by &fields) ;

	disconnect from oracle;
quit;
data &out;
	set &out;
	format Rows comma20.;
run;
%if %upcase(&order) = FREQ %then %do;
	proc sort data=&out;
		by descending Rows;
	run;
%end;
proc print data=&out noobs; run;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2006-11*/
/*
o This macro performs a fully automated sample imputation of missing values: all missing values 
  from all columns of a dataset are replaced with values from random samples (with replacement),
  pulled from the populations of non-missing values.
o Why random sample imputation: because for one model (so far) I've looked at more closely,
  sample imputations outperformed mean/median/mode imputations.
o All missing values are imputed by default, even those where this wouldn't make any sense (say
  missing values in NAME, ADDRESS, SSN etc). To avoid such behavior, pay attention to the dataset
  that you feed to this macro.
o For performance reasons the data set is modified in place; if you need a backup copy create it 
  before running the macro.
o Usage:
%sampleimpute(mytable)
o Value: the same dataset with all missing values imputed via random sampling.
*/

%macro sampleimpute(dset);
%local lib dsname i;
/*---determine the names of the library and dataset*/
%if %index(&dset,.)=0 %then %do; 
  %let lib=WORK;
  %let dsname=%upcase(&dset);
%end;
%else %do; 
  %let lib=%upcase(%scan(&dset,1,"."));
  %let dsname=%upcase(%scan(&dset,2,"."));
%end;
/*---put the variable names into macro variables*/
proc sql noprint;
  create table vars_miss as
  select libname,memname,memtype,name,type,varnum,label
  from dictionary.columns
  where libname="&lib" and memname="&dsname";
  select trim(left(put(count(*),8.))) into :nvars from vars_miss;
  select name into :varname1-:varname&nvars from vars_miss;
  select label into :varlab1-:varlab&nvars from vars_miss;
  select type into :vartype1-:vartype&nvars from vars_miss;
quit;
/*---replace any missing values in a loop*/
%do i=1 %to &nvars;
  proc sql noprint;
    select nmiss(&&varname&i) into: todraw  /*number of missing values for this variable*/
    from &dset;
  quit;
  %if &todraw ne 0 %then %do;
    /*the full distribution, before NA replacement*/
    data dtb;
      set &dset;
      Obs = _N_;
      keep Obs &&varname&i;
    run;
    proc sql;
      /*the non-missing values*/
      create table forsamp as
      select &&varname&i
      from dtb
      where &&varname&i is not null;
      /*the missing values*/
      create table missing as
      select *
      from dtb
      where &&varname&i is null;
    quit;
    /*random sample with replacement, similar to R's sample(x, N, replace=TRUE)*/
    proc surveyselect data=forsamp out=filling method=urs n=&todraw outhits; run;
    data filling2;
      merge missing filling;
      drop NumberHits;
      rename &&varname&i = replacement;
    run;
    /*replace the missing values*/
    /*chosen method for NA replacement: MODIFY with the POINT option (see the documentation for details)*/
    data &dset;
      set filling2;
      modify &dset point=obs;
      &&varname&i = replacement;
    run;
    /*a report*/
    data _null_;
      title;
      file print;
      put "- imputed &todraw missing values for &&varname&i";
    run;
  %end;
%end;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2006-08*/
/*
o This macro imports and parses a given SAS log file to create a report
  showing the running times of all DATA steps and PROCs.
o The log file can be created with the macro redirect.sas.
o Objectives:
  - Quickly assess the time consumed by SAS programs with many PROCs and DATA steps.
  - Identify bottlenecks.
o Arguments:
  - logfile = the log file
  - out = if none, print the report to screen; if a file, print the report to a file
o Usage:
%saslogtime(\\pplzapp05\BI\Bogdan\SASlog.txt) - print the report
%saslogtime(\\pplzapp05\BI\Bogdan\SASlog.txt,out=\\pplzapp05\BI\Bogdan\SAStime.txt) - redirect the report
*/

%macro saslogtime(logfile,out=);
/*import the log*/
data realtime cputime;
	infile "&logfile" truncover end=last;
	input logrows $char300.;
	logrows = compbl(logrows);
	if index(upcase(logrows),"REAL TIME") > 0 then output realtime;
	if index(upcase(logrows),"CPU TIME") > 0 then output cputime;
run;
/*extract the times, in seconds*/
data realtime;
	set realtime;
	RealTime = compress(tranwrd(tranwrd(logrows,'real time',''),'seconds',''));
	RealSeconds = input(RealTime,stimer20.);
	rename logrows=RealTimeLog;
run;
data cputime;
	set cputime;
	CPUTime = compress(tranwrd(tranwrd(logrows,'cpu time',''),'seconds',''));
	CPUSeconds = input(CPUTime,stimer20.);
	rename logrows=CPUTimeLog;
run;
data bothtimes;
	merge realtime cputime;
run;
proc sort data=bothtimes;
	by descending RealSeconds;
run;
proc sql;
	create table logtimesumm as
	select round(sum(RealSeconds)/60,0.01) as RealMinutes,
		round(sum(CPUSeconds)/60,0.01) as CPUMinutes
	from bothtimes;
quit;
/*report*/
%if &out ne %then %do;
	proc printto new print="&out"; run;
%end;
proc print data=logtimesumm noobs;
	title '======Total Real/CPU running times======';
run;
proc print data=bothtimes noobs;
	title '======Time used by each data step/procedure======';
	var realtimelog cputimelog;
run;
%if &out ne %then %do;
	proc printto; run;
%end;
title;
proc sql;
	drop table realtime;
	drop table cputime;
	drop table bothtimes;
	drop table logtimesumm;
quit;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2006-05*/
/*
o This macro sends emails with attachments via the SMTP interface. In addition,
  the macro can parse a given log file and report if possible errors occurred.
o 2007-06: decided to delete some offending lines from the log generated by PROC EXPORT.
  These lines containing the word 'error' are left in the log file, but not in the dataset
  built by importing the log (and used to determine possible errors).
o To enable SMTP mail in SAS, insert these lines in the SAS configuration file (SASV9.CFG):
  -EMAILSYS SMTP
  -EMAILHOST pplzexclb
  -EMAILPORT 25
  (emailhost and emailport are provided by our email administrator).
o If a redirected log file is specified, the log is imported and searched for errors.
  This SHOULD happen for SAS jobs scheduled to run in batch mode. I do NOT recommend anyone
  to run SAS jobs in batch mode without redirecting the log: big errors can remain unnoticed.
o The list of words indicating trouble, currently ERROR WARNING TERMINATED DAMAGED ABNORMALLY UNINITIALIZED
  can be easily updated.
o Usage:
  - Send an email with an attachment:
%sendemail("bromocea@ohiosavings.com","addr1@ohiosavings.com" "addr2@ohiosavings.com",
  "email from SAS job","This is a test.","\\Pplzapp05\BI\Bogdan\loanperf_files.txt",)
  - Send an email with more than one attachment:
%sendemail("bromocea@ohiosavings.com","addr1@ohiosavings.com" "addr2@ohiosavings.com",
  "email from SAS job","This is a test.","\\..\file1.txt" "\\..\file2.txt",)
  - As above, but also parse the log for errors:
%sendemail("bromocea@ohiosavings.com","addr1@ohiosavings.com" "addr2@ohiosavings.com",
  "email from SAS job","This is a test.","\\..\file1.txt" "\\..\file2.txt","\\...\SASlog.txt")
*/

%macro sendemail(from, to, subject, body, attach, logfile);
%local logwords i;
/*---if a log file is given, parse it to look for errors---*/
%if &logfile ne %then %do;
	%let logwords = ERROR WARNING TERMINATED DAMAGED ABNORMALLY UNINITIALIZED;
	%lst(%upcase(&logwords),logword)
	data saslog;
		infile &logfile truncover end=last;
		input logrows $char300.;
		/*br 2007-06: decided to remove some inappropriate LOG lines generated by PROC EXPORT,
		containing the word ERROR. I talked to SAS tech support but the solution they suggested 
		(proc printto log=_null; run) is not satisfactory.*/
		if indexw(logrows, 'set the ERROR detection macro variable') + indexw(logrows, 'if _ERROR_ then call') > 0 then delete;
		%do i=1 %to &nw;
			f&i=index(upcase(logrows),"&&logword&i");
		%end;
		found=sum(of %do i=1 %to &nw; f&i %end;);
	run;
	proc sql noprint;
		select sum(found) into :errors
		from saslog;
		select distinct 'POSSIBLE ERROR * '||&subject into :errsubject
		from saslog;
	quit;
	/*---the addresses and subject for the email---*/
	filename mymail email 
		from = &from
		to = (&to)
		%if %eval(&errors) = 0 %then %do;
			subject = &subject
		%end;
		%if %eval(&errors) ne 0 %then %do;
			subject = "&errsubject"
		%end;
		content_type = "text/plain"
		%if &attach ne %then %do;
			attach = (&attach)
		%end;;
	data _null_;
		file mymail;
		put &body;
		put /'The log file was searched for these words:';
		put @3 %do i=1 %to &nw; "&&logword&i  " %end;;
		if %eval(&errors) = 0 then put 'None was found. (Other words indicating trouble can and perhaps should be added.)';
		else put 'At least one word appears at least once in the log.'//'!!!!!!!!!!!!!'/
			'!!! ALARM !!!'/'!!!!!!!!!!!!!';
	run;quit;
%end;
%if &logfile eq %then %do;
	filename mymail email 
		from = &from
		to = (&to)
		subject = &subject
		content_type = "text/plain"
		%if &attach ne %then %do;
			attach = (&attach)
		%end;;
	data _null_;
		file mymail;
		put &body;
	run;quit;
%end;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2004-06*/
/*
o This macro standardizes the export of SAS output to HTML files.
o The formatting is implemented through a CSS file named 'blue.css', which needs to be 
  saved in the parent folder of &folder.
o Why use ODS MARKUP with the tagsets.htmlcss tagset to create HTML output:
  - The formatting is centralized in one location (the CSS file), and the looks of an 
    unlimited number of reports can be changed by editing only one file. The same would
    be much more difficult to achieve in the case of regular HTML files (where the 
    formatting is done through HTML tags), as each HTML file would have to be edited.
  - The file sizes are significantly reduced because many of the formatting HTML tags are 
    eliminated. These savings can easily add up to quite a lot of disk space. (I've seen
    cases where the HTML-CSS files were 50-80% smaller than the regular HTML files.)
o Arguments:
  - file = the file name; to export in "Excel" format, use the extension .xls
  - fld = the folder where the HTML file will be saved
  - 2011-04 style: whether to add formating through a CSS file, or produce minimal HTML. CSS
    leads to very significant space savings compared to regular HTML formatting, and minimal
    HTML through the CHTML tagset leads to another 50% space savings on top of that.
o 2011-03: renamed macro from odsb.sas to sink.sas, to make it similar to sink() from R.
*/

%macro sink(file, fld=\\NYCB\corp$\DBMarketing\Bogdan\out, style=Y, stylename=normal);
%if &file ne %then %do;
	%if &style eq Y %then %do;
		/*CSS style sheets are good for dashboards (smaller file sizes, formatting is defined
		and controlled in one place). But this doesn't work well for emailing the reports as
		the CSS file may become inaccessible, so instead i settled on HTML with the formatting
		defined in each file.*/
		/*ods markup body="&fld.\&file" stylesheet=(URL="../blue.css") tagset=tagsets.htmlcss;*/
		ods html body="&fld.\&file" style=&stylename;
	%end;
	%if &style ne Y %then %do;
		ods markup body="&fld.\&file" tagset=CHTML;  /*minimal file size*/
	%end;
%end;
%else %do;
	%if &style eq Y %then %do; ods html close; %end;
	%if &style ne Y %then %do; ods markup close; %end;
%end;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2006-05*/
/*
o This macro suspends the execution of a program for a specified number of minutes.
o Usage:
%sleep(0.1)
*/

%macro sleep(minutes);
%local rc;
%let rc = %sysfunc(sleep(&minutes*60));
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2013-02*/
/*
o This macro puts a program to sleep until a predefined time (as opposed to a number
  of minutes). The advantage is that the time when a certain even should happen (such
  as a report being emailed) can be controlled precisely in advance, without having 
  to keep track of the job start time, actual running time, possible bottlenecks and so on.
o Usage:
  %sleepUntil(14:45)
  %sleepUntil(08:45)
*/

%macro sleepUntil(time);
*compute the number of minutes to sleep;
data _null_;
	time = input("&time", time.);
	now = input("%sysfunc(Time(),time.)", time.);
	min2sleep = max(0, ceil((time - now) / 60));
	call symputx('min2sleep', trim(left(min2sleep)));
run;
%put Sleep requested until &time.. It is now %sysfunc(Time(),time.). Will sleep for &min2sleep minutes.;
%if &min2sleep ne 0 %then %do;
	%sleep(&min2sleep)
%end;
%put Wake up. It is now %sysfunc(Time(),time.).;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2019-08*/
/*
o As a workaround for SAS macros not accepting arguments that include commas (which is however
  required in some cases), convert a list of words separated by spaces into a string that can 
  work in SQL.
*/

%macro sqlstring(words, vname=sqlstring, quotes=Y);
%global &vname;
%local i;
data _null_;
	nwords = %cwords(&words);
	call symputx('nw',trim(left(put(nwords,5.))));
run;
data tmp_w2csv;
	%do i=1 %to &nw;
		%if &quotes = Y %then %do;
			word = cats("'", scan("&words",&i), "'");
		%end;
		%else %do;
			word = cats(scan("&words",&i));
		%end;
		output;
	%end;
run;
proc sql noprint;
	select word into: &vname separated by ", "
	from tmp_w2csv;
quit;
%put String for SQL (macro var name = &vname):  &&&vname;
proc sql;
	drop table tmp_w2csv;
quit;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2019-06*/
/*
o As I knew for a long time, stacking datasets with the SAS data step WILL lead to truncation
  of data if the datasets added later have wider columns. This is a VERY STUPID default behavior,
  by the way, courtesy of SAS Institute.
o This SAS bug is usually no big deal - however can be very serious if NO truncation should occur.
  As a workaround, use PROC SQL to stack datasets.
o HOWEVER... I also saw this fail BIG TIME....... when the tables did not have the same columns.
  Some fields were inserted into other fields!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
o So this is safe to use ONLY IF all tables have 100% identical columns. Probably in the same order too.
o Choose your poison.......
*/

%macro stack(ds, out);
%lst(&ds, dataset)
proc sql;
	create table &out as
	select * from &dataset1
	%do i = 2 %to &nw;
		union all
		select * from &&dataset&i
	%end;
	;
quit;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2011-07*/
/*
o This macro produces summary statistics for a given variable.
o 2012-03: Fixed an issue with date variables which used to show their summary
  statistics as numbers rather than date values.
o 2012-10: Finally got around to adding some looping that is able to run several
  variables at the same time. This reduces typing and also allows variables to
  be more easily compared side by side, for example %stat(dset, var1 var2 var3).
*/

%macro stats(dset, var, by=, print=Y, tall=Y, ttl=, fmt=, subset=, wait=0.03);
%local v c;
%lst(&var, statvar)
/*2014-03: To be able to format the statistics as Dates if applicable, reformat the
BY variable if provided so its values represent valid SAS names. In turn it will be
possible to loop over those variable names after PROC TRANSPOSE.*/
data tmp_stats;
	set &dset;
	%if "&subset" ne "" %then %do;
		if &subset ;
	%end;
	%if &by ne %then %do;
		tempBY = cats('_', translate(trim(compbl(compress(&by, '+-()/'))), '_', ' '));
	%end;
run;

%do v = 1 %to &nw;  /*start of loop by var*/

/*2021-02: Introduce some sleeping due to persistent file access issues (bad/slow network share)*/
%sleep(&wait)

proc means data=tmp_stats noprint fw=100;
	var &&statvar&v;
	%if &by ne %then %do; class tempBY / missing; %end;
	output out=summstats(drop=_type_ _freq_) N=N Nmiss=Nmiss Sum=Sum Min=Min P1=P1 P5=P5
		P25=P25 Median=Median Mean=Mean P75=P75 P95=P95 P99=P99 Max=Max;
run;
%nolabels(summstats)
%if &fmt ne %then %do;
	data summstats;
		set summstats;
		format Sum Min P1 P5 P25 Median Mean P75 P95 P99 Max &fmt ;
	run;
%end;

%if &by ne %then %do;
	data summstats;
		set summstats;
		/*
		new&by = put(&by, 20.);  *in case &by only includes numeric values;
		if trim(left(new&by)) in (' ','.') then new&by = 'ALL';
		drop &by;
		rename new&by = &by;
		*/
		if tempBY = ' ' then tempBY = 'ALL';
	run;
%end;
proc transpose data=summstats out=summstats2;
	%if &by ne %then %do; id tempBY; %end;
run;
data summstats2;
	set summstats2;
	rename _name_=Statistic %if &by = %then %do; col1 = Value %end;;
run;
/*If &var is a date, PROC TRANSPOSE eliminates the format and the values show up as
numbers, which is not helpful at all. To correct this, check the format, and if it's
DATE then apply the required formats.*/
%varInfo(summstats, MEAN)
%let finfmt = ;
%if &varFormat = DATE %then %do; %let finfmt = date9.; %end;
%if &fmt ne %then %do; %let finfmt = &fmt; %end;
%if &finfmt ne and &by = %then %do;
	data summstats2;
		set summstats2;
		if upcase(statistic) in ('N','NMISS') then NewValue = put(Value, comma20.0);
			else NewValue = put(Value, &finfmt);
		drop value;
		rename NewValue = Value;
	run;
%end;
%if &finfmt ne and &by ne %then %do;
	proc sql noprint;
		/*the character column names ...*/
		create table temp_vars as
		select name, type
		from dictionary.columns
		where libname = "WORK" and memname = "%upcase(summstats2)" and name not in ('Statistic');
		/*... into macro variables*/
		select trim(left(put(count(*),8.))) into :nvars from temp_vars;
		select name into :v1-:v&nvars from temp_vars;
	quit;
	data summstats2;
		set summstats2;
		%do c = 1 %to &nvars;
			if upcase(statistic) in ('N','NMISS') then n&&v&c = put(&&v&c, comma20.0);
				else n&&v&c = put(&&v&c, &finfmt);
			drop &&v&c;
			rename n&&v&c = &&v&c;
		%end;
	run;
%end;

data summstats2_&v;
	set summstats2;
	%if &by = %then %do; rename Value = &&statvar&v; %end;
run;
%end;  /*end of loop by var*/

data allstat;
	merge %do v = 1 %to &nw; summstats2_&v %end;;
	/*by Statistic notsorted;*/
run;

*print the results;
%if &ttl = %then %do;
	%if &by ne %then %do; title "Summary statistics for &var by &by"; %end;
	%else %do; title "Summary statistics for &var"; %end;
%end;
%else %do; title "&ttl"; %end;
%if &print = Y %then %do;
	%if &tall = Y %then %do;
		proc print data=allstat noobs; run;
	%end;
	%else %do;
		proc print data=summstats noobs; run;
	%end;
%end;
title;
/*
proc sql;
	drop table summstats;
	drop table summstats2;
	drop table check_date;
	drop table allstat;
	%do v = 1 %to &nw; drop table summstats2_&v ; %end;
quit;
*/
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea jan06*/
/*macro similar to the UNIX shell utility 'tail'*/
/*it prints the last rows from a SAS data set*/

/*USAGE:
%tail(mytable)	*print the default number of rows;
%tail(mytable,r=100)	*print a specified number of rows;
*/

%macro tail(dset,r=20, ttl=);
proc sql noprint;
	select trim(left(put(count(*),8.))) into :nrows
	from &dset;
quit;
%if &ttl = %then %do;
	title "The last &r rows from [ &dset ]";
%end;
%else %do;
	title "&ttl";
%end;
proc print data=&dset (firstobs=%eval(&nrows-&r+1) obs=&nrows);
run;
title;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea sep04*/

/*This macro gets the date/time as of the execution of this macro - and not as of 
when the SAS session was started. Purpose: display time in the log.*/

%macro timenowlog(message);
%put ========= &message ;
%put %sysfunc(Date(),date9.)_%sysfunc(Time(),time.) =========;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2019-10
o This macro saves some typing when redirecting output to Excel files.
o At the end, you must close the Excel file with 
    ods excel close;
*/

%macro toExcel(filename, sheet=Report, opt=);
goptions device=png;  /*this is only meant to supress a stupid WARNING in the log - courtesy of SAS*/
%if %index(&filename, \\) %then %do;
	ods excel file= "&filename" 
%end;
%else %do;
	ods excel file= "\\NYCB\corp$\DBMarketing\Bogdan\out\&filename" 
%end;
	options( embedded_titles='yes' embedded_footnotes='yes' frozen_headers='yes' &opt)
	style=normal;
ods excel options(sheet_name="&sheet");
%mend;

/*----------------------------------------------------------------------------------*/

/*BR 2019-08*/
/*
o Select the top X rows from an Oracle table. Use pass-through for performance.
  The &connInfo macro variable is assumed to exist.
o The table name must include the schema (e.g. EIS_DW.HISTORY).
o Arguments:
  N = the number of rows to return
  cols = an optional list with column names separated by blanks. If provided, only pull those.
*/

%macro top(tb, N=200, cols=);
*Wow. Oracle is VERY, VERY picky when it comes to quotes. It simply does not accept
the double ones. Single ones are fine, but then the SAS macro vars do not resolve. To
make both work, assemble to precise string required in advance.;
data _null_;
	oraLab = cats("'", "&tb", "'");
	call symputx('oraLab', oraLab);
run;
%if &cols = %then %do;
	%let slct = *;
%end;
%else %do;
	%sqlString(&cols, vname=slct, quotes=N)
%end;

*Determine the connection info to use for Oracle pass-through queries, based on the schema name;
%OracleConn(&tb)
title; footnote;
proc sql;
	connect to oracle (&connInfo);

	select * from connection to oracle
	(select &oraLab "Table", to_char(count(*), '999,999,999,999') "Rows" from &tb) ;
	/*create a table - for sending to Excel when needed*/
	create table top_tmp as
	select * from connection to oracle
	(select &slct from &tb
	where ROWNUM <= &N ) ;

	disconnect from oracle;
quit;
proc print data=top_tmp noobs; run;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2012-07*/
/*
o I finally got tired of repeatedly typing the same PROC TRANSPOSE syntax over and over and over again.
*/

%macro transp(dset, by, id, vars, out, reverse=N, label=Y);
%lst(&vars, trvar)
%local i;
data trtemp;
	set &dset;
	%do i = 1 %to &nw;
		/*2021-03: Sometimes, simple variable names are preferable*/
		%if &label = Y %then %do;
			%if &reverse = N %then %do; TrFlag&i = catx('_', &id, "&&trvar&i"); %end;
			%else %do; TrFlag&i = catx('_', "&&trvar&i", &id); %end;
		%end;
		%else %do;
			TrFlag&i = &id;
		%end;
	%end;
run;
proc sort data=trtemp; by &by &id; run;
%do i = 1 %to &nw;
	proc transpose data=trtemp out=trtemp&i;
		by &by;
		id TrFlag&i;
		var &&trvar&i;
	run;
%end;
data &out;
	merge %do i = 1 %to &nw; trtemp&i (drop = _name_ ) %end;;  /*_label_*/
	by &by;
run;
proc sql;
	/*drop table trtemp;*/
	%do i = 1 %to &nw; drop table trtemp&i; %end;
quit;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2012-11*/
/*
o Do a quick time series plot for a given date field. This is meant to replace
  PROC FREQ followed by manual copy, paste and chart in Excel.
o Note that the v= argument allows counting all rows (by default), or distinct
  values in a certain field (e.g. distinct ACCT_NBR).
o Examples:
  %ts(mytable, ACCOUNT_OPEN_DATE)
  %ts(myttrans, date, v=distinct acct_nbr)
o 2017-09: 
  - Tweaked the v argument so that other summary functions (such as sum of balances)
  can be used in addition to row counts. 
  - Added a date period argument (for intnx), plus a few required syntax changes, so 
  that the data can be summarized by week or month.
  - Added a format for the variable being plotted - dollar amounts in the Millions can
  be especially hard to read without a format.
o 2020-10: A new argument allows specifying whether a given field is a datetime, to be
  plotted as a date.
o Example: 
  %ts(pmma, ACCOUNT_OPEN_DATE, period=week.2, v=sum(DAILY_END_BALANCE), vlab=Balance,
    ttl=New PMMA balances (weekly), fmt=dollar20.0)
*/

%macro ts(dset, date, dtime=N, period=, v=count(*), vlab=freq, ttl=, plot=Y, fmt=);
%if "&period" = "" %then %do;
	%if &dtime = N %then %do;
		%let dt = &date;
		%let gby = &date;
		%let RL = ;
	%end;
	%else %do;
		%let dt = datepart(&date) format date9.;
		%let gby = calculated &date;
		%let RL = ;
	%end;
%end;

/*While I first used put() with a weekly format here, the results were not satisfactory 
because of rather cryptic date labels such as 17W27. Better use the intnx function so that 
actual dates are displayed on the charts.*/
%else %do;
	%let dt = intnx("&period", &date, 0, "beginning") format date9.;
	%let gby = calculated &date;
%end;
*Properly label the date axis when weekly or monthly summaries are requested.;
%if %index(%upcase(&period), WEEK) %then %do;
	%let RL = Week Starting;
%end;
%if %index(%upcase(&period), MONTH) %then %do;
	%let RL = Month Starting;
%end;
proc sql;
	create table tempfreq as
	select &dt as &date, &v %if "&fmt" ne "" %then %do; format &fmt %end; as &vlab
	from &dset
	where &date is not null
	group by &gby
	order by &gby;
quit;
data tempfreq;
	set tempfreq;
	Cumul_&vlab + &vlab;
	%if "&fmt" ne "" %then %do;
		format Cumul_&vlab &fmt;
	%end;
run;
%if "&ttl" ne "" %then %do; title "&ttl"; %end;
%if &plot = Y %then %do;
	%gplot(tempfreq, &date, &vlab, rangeLab=&RL)
	%if "&ttl" ne "" %then %do; title "&ttl (Cumulative)"; %end;
	%gplot(tempfreq, &date, Cumul_&vlab, rangeLab=&RL)
%end;
%else %do;
	proc print data=tempfreq noobs; run;
%end;
title;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea nov05*/
/*this macro saves some typing*/

%macro univ(dset,vars);
proc univariate data=&dset nextrobs=20 nextrval=20;
	var &vars;
	histogram &vars;
run;
%mend;

/*----------------------------------------------------------------------------------*/

/*bogdan romocea 2007-12*/
/*
o This macro combines and automates the steps associated with clustering variables. In a 
  departure from what I did in the past, instead of selecting 1-3 vars from each cluster, 
  the macro creates a dataset with all cluster scores which are meant to be used later 
  in place of the original variables.
o Arguments:
  - dset = the dataset containing the vars to be clustered
  - vars = the list of variables for clustering
  - prefix = a prefix to be added to each cluster name (so Clus1, Clus2 ... become prefix_Clus1, 
    prefix_Clus2 ... which is good for keeping track of who's who when the cluster scores from 
    several variable clustering analyses are meant to be combined into one dataset)
  - coef = the name of the dataset with the cluster coefficients, to be saved for later use
  - scored = the name of the dataset with the cluster scores
  - folder = where to save the report
*/

%macro varclus(dset, vars, prefix, coef, scored, folder);
%local ncl_dset;
/*---run varclus*/
ods output clusterquality=summary rsquare(match_all)=clusters;
proc varclus data=&dset short hi outstat=score;
	var &vars;
run;

/*---get the number of clusters in the final solution*/
proc sql noprint;
	/*with -2 here, due to the weird naming convention of the CLUSTERS datasets*/
	select trim(left(put(max(NumberOfClusters)-2, 10.))) into :ncl_dset
	from summary;
	select trim(left(put(max(NumberOfClusters), 10.))) into :ncl
	from summary;
quit;
/*however &ncl_dset can't be 0*/
%if &ncl_dset = 0 %then %do;
	%let ncl_dset = ;
%end;
/*---produce cluster scores*/
/*get the coefficients (NB: the coefficients are standardized and the MEAN and STD must be retained, via _NCL_ = .)*/
data &coef._&prefix;
	set score;
	if _NCL_ in (&ncl, .);
	if index(_NAME_, 'Clus') = 1 then _NAME_ = cats("&prefix", '_', _NAME_);
	if _NCL_ = . and _TYPE_ = 'CORR' then delete;
	drop _NCL_;
run;
/*PROC SCORE standardizes the new data by subtracting the original variable means that are stored in the _TYPE_='MEAN' 
observations, and dividing by the original variable standard deviations from the _TYPE_='STD' observations. Then PROC SCORE 
multiplies the standardized variables by the coefficients from the _TYPE_='SCORE' observations to get the cluster scores. */
proc score data=&dset out=&scored (drop=&vars) score=&coef._&prefix; run;

/*---report*/
ods markup body = "&folder.\varclus_&prefix..htm"
	stylesheet=(url="\\Pplzapp05\BI\0.CommonFiles\misc\forEG.css")
	tagset=tagsets.htmlcss;
proc print data=summary;
	title 'Varclus iterations';
run;
proc print data=clusters&ncl_dset;
	title 'Varclus - Selected Solution';
	title2 '1 - R^2 ratio = (1 - R^2 own) / (1 - R^2 next closest)';
	title3 'Choose the 1-3 variable(s) with the lowest 1 - R^2 ratio from each cluster, or use the cluster scores.';
run;
/*this can be huge, so better don't print it*/
/*
proc print data=&coef._&prefix;
	title 'Dataset used to produce cluster scores';
run;
*/
title;
ods markup close;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2012-04*/
/*
o This macro checks whether a given variable is character or numeric.
o The output is a global variable, named variableclass, with the values CHAR or NUM.
o Note this does not check the contents of a column.
*/

%macro variableclass(dset, var);
%global variableclass;
proc contents data=&dset noprint out=temp_contents; run;
data temp_contents;
	length ttype $ 4;
	set temp_contents;
	if type = 1 then ttype = 'NUM';
		else if type = 2 then ttype = 'CHAR';
run;
proc sql noprint;
	select ttype into :variableclass
	from temp_contents
	where upcase(name) = upcase("&var");
	drop table temp_contents;
quit;
%put The following macro variable has been created for &var: variableclass = &variableclass;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2012-11*/
/*
o This macro retrieves various info, currently type and format, for a variable from a given
  table. It's helpful for determining, for example, if a given variable is formatted as a 
  date, which in turns allows labeling it as a date in PROC MEANS or when creating macro variables.
o That SAS should automatically keep track of the DATE formats and apply them everywhere
  automatically is a clear bug, and an issue that could be discussed elsewhere.
*/

%macro varInfo(dset, v);
%put ===Now checking the format of variable [ &v ] from dataset [ &dset ];  /*for troubleshooting*/
%global varFormat varType;
proc contents data=&dset noprint out=tmp_check_fmt; run;
data tmp_check_fmt;
	length vtype $ 4;
	set tmp_check_fmt;
	if Type = 1 then vtype = 'NUM';
		else if Type = 2 then vtype = 'CHAR';
run;
proc sql noprint;
	select trim(left(format)) into :varFormat from tmp_check_fmt where upcase(name) = upcase("&v");
	select trim(left(vtype)) into :varType from tmp_check_fmt where upcase(name) = upcase("&v");
	drop table tmp_check_fmt;
quit;
%if %index(YYMMDD MMDDYY, &varFormat) %then %do;
	%let varFormat = DATE;
%end;
%let varFormat = %trim(&varFormat);
%let varType = %trim(&varType);
%put === The following global vars were created: varType = &varType , varFormat = &varFormat .;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2019-04
o Import a text delimited file but with all fields as character - and without any truncation.
o Importing all fields as character: This is occasionally required or mandatory, or at least very
 important for robustness. Importing all fields as character stops SAS from trying to infer the data
 types, and then potentially applying some inappropriate conversions. Instead of messing around with
 informats, better treat EVERYTHING as character.
o This macro is not recommended for importing multiple files in a loop - because scanning with guess=max
 takes a very long time (SAS bug/deficiency). Instead, one use can this macro to import single (large)
 files only - as long as they are representative (meaning all fields/values there are as wide as they
 are going to be in the future) - then copy the import syntax into a separate data step. This speeds up
 the import by 10X or more.
*/
%macro importDlmAsChar(file, fld, out, names=yes, guess=max, dlm=",");
/*First import as usual to get the field names and lengths*/
proc import datafile="&fld.\&file" dbms=DLM out=tmp replace;
   guessingrows = &guess ;  /*use max to read all rows: (much) slower, but more secure*/
   getnames = &names;
   delimiter = &dlm;
run;
/*field names and lengths*/
/*NB: THE LENGTH ONLY WORKS FOR CHARACTERS, AS MEANT HERE!! All numerics have a length of 8 bytes,
which will obviously fail for fields (such as SSN and phone number) that have more than 8 digits.
As a shortcut, replace all numeric lengths with 50. In other words, assume that all numeric
values, when converted to character, will not exceed 50 characters.*/
proc  contents data=tmp out=tmpcont noprint; run;
data tmpcont;
   set tmpcont;
   if type = 1 then Length = 50;
   /*Make sure the field names are OK. Otherwise the import will fail, as observed with the RDC
   extracts from Fiserv in Dec 2021.*/
   Name = tranwrd(strip(Name), " ", "_");

run;
PROC SQL NOPRINT;
   SELECT CATT(name,' : $',length,'.') INTO :vars SEPARATED BY ' ' FROM tmpcont ORDER BY varnum;
QUIT;
%put &vars;
/*the final table with all fields as character*/
DATA &out;
   INFILE "&fld.\&file" DELIMITER = &dlm MISSOVER DSD FIRSTOBS=2 LRECL=32767;
   INPUT &vars;
RUN;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2023-01
o This macro pulls over all schemas, all tables and all column names from a given Oracle connection.
 This is meant to greatly speed up certain kinds of searches, where the analyst needs to know all of
 the tables where a given field is present.
*/

%macro OracleAllColumns(out);
%if %sysfunc(exist(&out)) %then %do;
       %put =====Table &out is present. Not querying it again. ;
%end;
%else %do;
   proc sql;
       connect to oracle (&connDNA);
       /*Retrieve all column names in all tables*/
       create table tmp_DNA_fields as
       select * from connection to oracle
       (select *
       from ALL_TAB_COLUMNS
       /*where ROWNUM <= 100*/
       /*where upper(OWNER) = 'EIS_DW'*/
       order by TABLE_NAME, COLUMN_ID );

       /*also get the # of records in each table*/
       create table numrows as
       select * from connection to oracle
       (select TABLE_NAME "Table", OWNER, 'Table' "Type", NUM_ROWS
       from SYS.ALL_TABLES
       union all
       select VIEW_NAME "Table", OWNER, 'View' "Type", NULL "NUM_ROWS"
       from SYS.ALL_VIEWS ) ;

       disconnect from oracle;
   
       create table &out as
       select a.*, NUM_ROWS
       from tmp_DNA_fields a left join numrows b
           on a.OWNER = b.OWNER and a.TABLE_NAME = b.TABLE;
   quit;
%end;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2020-02
o This macro runs some native Oracle queries that report all existing tables and views
 in a given schema, along with the number of records (but only for tables).
o If the schema is blank
*/

%macro OracleTOC(schema, conn=);
%sqlString(&schema, vname=schm, quotes=Y)
*Determine the connection info to use for Oracle pass-through queries, based on the schema name;
%OracleConn(&schema)

proc sql;
   connect to oracle (&connInfo);
   /*all schemas in there*/
   /*
   create table schemas as
   select * from connection to oracle
   (select USERNAME
   from SYS.ALL_USERS
   order by USERNAME) ;  */

   /*get all tables and views*/
   create table tbvw as
   select * from connection to oracle
   (select TABLE_NAME "Table", OWNER, 'Table' "Type", NUM_ROWS
   from SYS.ALL_TABLES
   where upper(OWNER) = upper(&schm)
   union all
   select VIEW_NAME "Table", OWNER, 'View' "Type", NULL "NUM_ROWS"
   from SYS.ALL_VIEWS
   where upper(OWNER) = upper(&schm)
   order by "Table") ;
   disconnect from oracle;
quit;
proc print data=tbvw;
   title "All tables and views in &schema as of &sysdate9";
   format NUM_ROWS comma20.0;
run;
title;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2022-06
o Finally put the code for sending emails in a macro. I did not do this for the longest time, because I thought the gains would be too
 small. However, this macro is still helpful for cleaning up the scripts and it also simplifies this task.
o NB: Because all senders and recipients are internal, exclude the @mynycb.com part from all email addresses. That string is added
 automatically.
*/

%macro sendEmail(from, sender, to, subj, body, cc=, att=, append=@flagstar.com, send=Y);
%sqlstring(&to, vname=emlrecipients, sep=" ", append=&append)
%if &att ne %then %do;
   %sqlstring(&att, vname=emlfiles, sep=" ")
%end;
%if &cc ne %then %do;
   %sqlstring(&cc, vname=copyto, sep=" ", append=&append)
%end;
filename mymail email from="&from.@flagstar.com" sender="&sender.@flagstar.com" to=(&emlrecipients)
   %if &cc ne %then %do; cc=(&copyto) %end;
   subject="&subj" content_type="text/html"
%if &att ne %then %do;
   attach=( &emlfiles content_type="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" )
%end; ;
/*2022-07: It turns out that sending the email is to be avoided if there is a need for formatted HTML emails, which
can however still use the mymail definition above. */
%if &send = Y %then %do;
   data _null_;
       file mymail;
       put "&body";
   run; quit;
   filename mymail clear;
%end;
%mend;

/*----------------------------------------------------------------------------------*/

/*br 2023-07
o Finally assembled a macro to send emails with a table embedded in the body of the email, and not as an attachment.
 This functionality has been in use for years, but was not made available through a macro.
o dset = the name of the report/table to put in the body of the email
o 2023-09: Added the ability to print an additional report in the email.;
*/

%macro sendHTMLemail(from, sender, to, subj, body, dset, body2=, dset2=, cc=, att=, append=@flagstar.com);
%sqlstring(&to, vname=emlrecipients, sep=" ", append=&append)
%if &att ne %then %do;
   %sqlstring(&att, vname=emlfiles, sep=" ")
%end;
%if &cc ne %then %do;
   %sqlstring(&cc, vname=copyto, sep=" ", append=&append)
%end;
filename mymail email from="&from.@flagstar.com" sender="&sender.@flagstar.com" to=(&emlrecipients)
   %if &cc ne %then %do; cc=(&copyto) %end;
   subject="&subj" content_type="text/html"
%if &att ne %then %do;
   attach=( &emlfiles content_type="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" )
%end; ;

*This is meant to format the HTML email in a way that is more reasonable;
proc template;
   define style styles.test;
   parent=styles.htmlblue;
   *get rid of black border around the titles;
   class systitleandfootercontainer / htmlstyle="border:none";
   *reduce the font size in the table;
   class fonts from fonts /
       'docFont' = ("Calibri",10pt);
   *make the output table more compact;
   class Table from Output /
       rules = ALL
       cellpadding = 2pt
       cellspacing = 0.25pt
       borderwidth = 0.5pt;
  end;
run;

*HTML email with PROC REPORT output included;
ods _all_ close;
ods html file=mymail style=styles.test;
title font=calibri height=11pt color=navy "&body";
*2023-11: To avoid sum _numeric_ add up the dates, which does not look very smart, adjust the names of the fields
being summed up;
%columns(&dset, out=temp_tb_cont)
proc sql noprint;
   select name into :addup separated by ' '
   from temp_tb_cont
   where lowcase(type) = 'num' and index(lowcase(format), 'date') = 0;
quit;
proc print data=&dset noobs;
   * sum _numeric_;
   sum &addup ;
run;
%if &dset2 ne %then %do;
   %if &body2 ne %then %do;
       title font=calibri height=11pt color=navy "&body2";
   %end;
   %else %do;
       title;
   %end;
   proc print data=&dset2 noobs; run;
%end;
ods html close;
ods listing;
filename mymail clear;
%mend;

/*----------------------------------------------------------------------------------*/

/*
br 2022-05
o This macro produces a more user friendly version of a given report, by shading records with alternate
 values in the &BY field in grey and white.
o Arguments:
 - excl = any fields that should be excluded (not printed). For example something used to calculated the
   FlagForShading, but which should then be excluded.
 - columns = a list of the fields to print. Also used to control the order
 - hC1-6 = individual column names, to be colored differently (just the header, not the entire column)
 - rowShade: look into the field named rowShadeField. If that field includes the string rowShadeValue,
   then color the entire row.
*/

%macro shadedRep(tb, BY, excl=, columns=, hC1=, hC2=, hC3=, hC4=, hC5=, hC6=, hC7=, rowShadeField=, rowShadeValue=, rowBoldField=, rowBoldValue=);
data tmp_for_print;
   set &tb;
   by &BY notsorted;
   if first.&BY then FlagForShading + 1;
   FlagForBold = 0;
   FlagForRowShade = 0;
   %if &rowBoldField ne %then %do;
       if index(&rowBoldField, "&rowBoldValue") > 0 then FlagForBold = 1;
   %end;
   %if &rowShadeField ne %then %do;
       if index(&rowShadeField, "&rowShadeValue") > 0 then FlagForRowShade = 1;
   %end;

run;
%if &excl ne %then %do;
   data tmp_for_print;
       set tmp_for_print;
       drop &excl ;
   run;
%end;
proc report data=tmp_for_print nowd;
   define FlagForShading / display noprint ;
   define FlagForBold / display noprint ;
   define FlagForRowShade / display noprint ;
   /*row colors*/
   compute FlagForRowShade;
       if FlagForRowShade = 1 and FlagForBold = 0 then call define(_row_,"style","style={background=DarkKhaki}");
       if FlagForRowShade = 1 and FlagForBold = 1 then call define(_row_,"style","style={font_weight=bold background=DarkKhaki}");
   endcomp;
   /*bold rows*/
   compute FlagForBold;
       if FlagForBold = 1 and mod(FlagForShading, 2) = 0 then call define(_row_,"style","style={font_weight=bold}");
       if FlagForBold = 1 and mod(FlagForShading, 2) = 1 then call define(_row_,"style","style={font_weight=bold background=GWH}");
   endcomp;
   /*alternate row shading*/
   compute FlagForShading;
       if mod(FlagForShading, 2) = 1 /*and FlagForBold = 0 and FlagForRowShade = 0*/ then call define(_row_,"style","style={background=GWH}");
       *if mod(FlagForShading, 2) = 1 and FlagForBold = 1 and FlagForRowShade = 0 then call define(_row_,"style","style={font_weight=bold background=GWH}");
   endcomp;
   %if &columns ne %then %do;
       column FlagForShading &columns ;
   %end;
   /*header colors*/
   %if &hC1 ne %then %do; define &hC1 / style(header)={background= verylightgreen tagattr='wrap:No' asis=on}; %end;
   %if &hC2 ne %then %do; define &hC2 / style(header)={background= lightorange tagattr='wrap:No' asis=on}; %end;
   %if &hC3 ne %then %do; define &hC3 / style(header)={background= lightblue }; %end;
   %if &hC4 ne %then %do; define &hC4 / style(header)={background= Plum }; %end;
   %if &hC5 ne %then %do; define &hC5 / style(header)={background= Gold }; %end;
   %if &hC6 ne %then %do; define &hC6 / style(header)={background= DarkTurquoise }; %end;
   %if &hC7 ne %then %do; define &hC7 / style(header)={background= LightPink }; %end;

run;
%mend;

/*----------------------------------------------------------------------------------*/

