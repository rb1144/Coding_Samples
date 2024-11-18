### BR 2019-03 and later
### Python functions for various utility tasks and Pandas


from bs4 import BeautifulSoup
import datetime
import difflib 
import email
from email.mime.application import MIMEApplication
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
import fnmatch
# import getpass  #to retrieve owner full names using subprocesses
import io
from itertools import (islice, takewhile, repeat)
import math
import os
import pandas as pd
import pathlib
# import pickle
import re
import requests
import shutil
import smtplib
import subprocess
import textwrap
# import win32api  #needed to get folder/file ownership on Windows
# import win32con
# import win32security


'''
br 2020-04
Finally wrote a script to do file comparisons in Python. After trying a few versions, 
I settled for HTML output as the easiest to read and understand.
    fldo, oldf, oldlab = the old/previous folder + file + a descriptive label
    fldn, newf, nlab = the new/updated folder + file + label
    fldr, repf = the folder + file name for the difference report
'''
def compareFiles(fldo, oldf, olab, fldn, newf, nlab, fldr, repf='diff_report.html', wrap=70):
    #Read the files given into lists. Must read line by line, for the comparison to work
    prev = os.path.join(fldo, oldf)
    upd = os.path.join(fldn, newf)
    with open(prev, 'r') as file:
        oldl = file.readlines()
    with open(upd, 'r') as file:
        newl = file.readlines()
    #custom title
    now = datetime.datetime.now().strftime('%Y-%m-%d %H:%M')
    ttl = '<b>Automated Version Comparison</b> between &nbsp;&nbsp;<i>' + prev + '</i> &nbsp;&nbsp; and &nbsp;&nbsp;<i>' + upd + '</i> &nbsp;&nbsp;'
    ttl = ttl + '<br>Produced with the Python difflib module. &nbsp;&nbsp;[' + now + ']'
    #diff = difflib.Differ().compare(oldl, newl)
    #diff = difflib.ndiff(oldl, newl)
    #diff = difflib.unified_diff(oldl, newl)
    #for l in diff:
    #    print(l.strip())

    #HTML output. Wrap the lines - otherwise the table will be quite wide and difficult to
    #   read without zooming out.
    #TO DO: move the legend at the top. Currently it's at a bottom, which is not the best way.
    diff = difflib.HtmlDiff(wrapcolumn=wrap).make_file(oldl, newl, oldf + olab, newf + nlab)

    #Fix the legend issue: copy it from the bottom to the top. Having it only at the end of
    #   a long output is not user-friendly.
    #Retrieve the legend
    legend_beg = diff.find('<table class="diff" summary="Legends">')
    legend_end = diff.find('</body>', legend_beg)
    legend = diff[legend_beg : (legend_end - 1)]    
    #2024-09: Reformat the legend, to make it very clear that those 2 tables are different
    legend = legend.replace('<th colspan="2"> Legends', '<th colspan="3" bgcolor="orange"> Legends')
    legend = legend.replace('summary="Colors"', 'summary="Colors" style="border:3px solid blue;border-collapse:collapse;"')
    legend = legend.replace('summary="Links"', 'summary="Links" style="border:3px solid green;border-collapse:collapse;"')
    legend = legend.replace('</table></td>\n', '</table></td>\n<td>' + '&nbsp'*10 + '</td>\n')
    #Where to insert it?
    new = diff.find('<table class="diff"')
    #the updated report
    newdiff = diff[0:new] + ttl + legend + diff[new:]
    with open(os.path.join(fldr, repf), 'w') as f:
        f.writelines(newdiff) 

#-------------------------------------------------------------------------------

'''
2024-07: Compare two files with functions and determine the changes function by function.
   bupFN, prodFN = the older backup and current version (full paths to files)
   diffile, updfile = the diff report (for the common functions) and the update file (new + modified functions)
   fundef, fsep = strings that identify and separate the functions
'''
def compareFunc(bupFN, prodFN, diffile, updfile, fundef='<- function', fsep='#' + 79*'-'):
   prod = parseFunc(prodFN, fundef, fsep)  #current version
   bup = parseFunc(bupFN, fundef, fsep)
   #Brand new functions. This is easy
   new = [x for x in [*prod] if x not in [*bup]]  #Wow. This is nice
   if len(new) > 0:
       print('New functions:')
       print(*new, sep='\n')
   #Changes. This is harder
   common = [x for x in [*prod] if x in [*bup]]
   rep = []
   mod = []
   for fn in common:
       bu = bup[fn].split('\n')
       bu = [i for i in bu if i != '']
       pr = prod[fn].split('\n')
       pr = [i for i in pr if i != '']
       # diff = difflib.context_diff(bu, pr, n=0)  #lineterm='\n'
       diff = difflib.unified_diff(bu, pr, lineterm='')
       rep.append('='*20 + fn + '='*20 + '\n')
       modified = 0
       for d in diff:
           if not d.startswith(' '):
               rep.append(d + '\n')
               modified = modified + 1
       if modified > 0:
           mod.append(fn)

   #The difference report
   with open(diffile, 'w') as file:
       for r in rep:
           file.write(r)
   #The update file = new + modified
   wanted = new + mod
   chosen = []
   for w in sorted(wanted, key=str.lower):
       chosen.append(prod[w])
   with open(updfile, 'w') as f:
       for ch in chosen:
           f.writelines(ch)
           f.writelines(fsep)

#-------------------------------------------------------------------------------

#br 2020-07
#Send text emails with attachments. Adapted from an example found on Stack Overflow

def eml(send_from, send_to, subject, text, attach=None, server="0235smtprelay.nycb.com"):
    assert isinstance(send_to, list)

    msg = MIMEMultipart()
    msg['From'] = send_from
    msg['To'] = email.utils.COMMASPACE.join(send_to)
    msg['Date'] = email.utils.formatdate(localtime=True)
    msg['Subject'] = subject

    msg.attach(MIMEText(text))

    for f in attach or []:
        with open(f, "rb") as fil:
            part = MIMEApplication(fil.read(), Name=os.path.basename(f))
        # After the file is closed
        part['Content-Disposition'] = 'attachment; filename="%s"' % os.path.basename(f)
        msg.attach(part)

    with smtplib.SMTP(server) as smtp:
        smtp.sendmail(send_from, send_to, msg.as_string())

#-------------------------------------------------------------------------------

#br 2021-11
#Given a pandas dataframe, print a quick summary
def explore(pan, n=3):
    if isinstance(pan, pd.DataFrame):
        print(type(pan))
        print('Rows and columns: ', end='')
        print(pan.shape)
        print(pan.head(n=n))

#-------------------------------------------------------------------------------

#2021-12: Explore a list of Pandas data frames, of the kind created by pd.read_html()
def explorePdHtml(rep, n=5):
    for l in rep:
#        if l.shape[1] != 1:  #skip the titles and footnotes (tables with 1 column)
        print('=' * 80)
        explore(l, n=n)

#-------------------------------------------------------------------------------

#br 2019-06
#This function produces an exploratory report for an existing pandas dataframe DF
#N = how many distinct values to print (in descending order of frequency)
#2019-07: If a column is numeric, then show the summary statistics (nicely formatted), instead
#   of the frequency distribution
def exploreDF(DF, N=10):
    #While my initial version had a lot of print() statements, it is better to store the
    #   output in a list. Because that simplifies the printing later on - especially the 
    #   redirection to external files.
    myrep = []
    #---Part A: a summary of Missing and Unique values
    nm = DF.count().rename_axis('Column').reset_index(name='Populated')  #Non-Missing values
    nm.insert(1, 'Col #', nm.index + 1)  #add the field number (varnum in SAS)
    nm.insert(2, 'Cases', DF.shape[0])  #add the number of cases/rows
    nm['Missing'] = nm['Cases'] - nm['Populated']
    nm['PctMissing'] = nm['Missing'] / nm['Cases']
    #value_counts.rename_axis('unique_values').to_frame('counts')
    unique = DF.nunique().rename_axis('Column').reset_index(name='UniqueValues')
    both = pd.merge(nm, unique, on='Column')
    both = both.sort_values(['PctMissing', 'Col #'], ascending=[False, True])
    rep = reportFormat(both)  #apply the formats ($, %, comma)
#    print('====== A. Summary ======\n', '\n')
#    print(rep, '\n\n')
    myrep.append('====== A. Summary ======')
    myrep.append('There are ' + f'{DF.shape[0]:,d}' + ' rows and ' + f'{DF.shape[1]:,d}' + ' columns.\n')
    myrep.append(rep)
    myrep.append('\n\n')
    #---Part B: frequency distributions
    #These fields are 100% missing. Say so, and exclude them from the frequency distributions.
    allmiss = nm.loc[lambda nm: nm.Populated == 0, ['Column', 'Col #']]
    amc = []
    if allmiss.shape[0] > 0:
        amc = allmiss['Column'].tolist()  #columns with 100% missing
        allmiss = ', '.join(amc)  #join into a string
        myrep.append('======WARNING====== The following fields are 100% Missing:')
        myrep.append(textwrap.fill(allmiss, 100))
        myrep.append('\n\n')
    #DF.columns.difference(amc)]
    myrep.append('====== B. Frequency Distributions ======\n')
    for v in DF.columns:
        if v in amc:  #ignore Columns with 100% missing
            continue
        #Very long syntax for the title, better split the line in 2
        ttl1 = '====== Col #' + rep['Col #'][rep['Column'] == v].tolist()[0] + ', ' + v + ': ' + f'{DF[v].size:,d}'
        ttl2 = ' values (' + f'{DF[v].count():,d}' + ' populated, ' + f'{DF[v].isna().sum():,d}' + ' missing)'
        myrep.append(ttl1 + ttl2)
        nuniq = f'{DF[v].nunique():,d}'  #NB: the distinct counts exclude NAs
        datakind = DF[v].dtype.kind  #A character code (one of ‘biufcmMOSUV’) identifying the general kind of data.
        if datakind in 'biufc':  #These are the numeric data types (from numpy)
            sst = DF[v].describe().rename_axis('Stat').reset_index(name='Value')
            if datakind in 'iu':
                sst['Value'] = sst['Value'].map("{:,.0f}".format)
            if datakind in 'fc':
                sst['Value'] = sst['Value'].map("{:,.2f}".format)
            myrep.append('Distinct values: ' + nuniq)
            myrep.append(sst)
        else:
            fd = freq(DF, v)  #the frequency distribution
            myrep.append('Distinct values: ' + nuniq + '. Showing ' + ('all' if N >= fd.shape[0] else 'the top ' + str(N)) + ':')
            myrep.append(fd.head(N))
        myrep.append('\n')
    return myrep

#-------------------------------------------------------------------------------

#br 2020-07: Given a string, find all matches for a certain pattern and return their position.

def findAll(string, patt):
    matches = [m.start() for m in re.finditer(patt, string)]
    return matches

#-------------------------------------------------------------------------------

#br 2019-06
#Replace all strings in a file with another string. The file is assumed to be small enough
#   to fit in memory. This is based on some code I found on Stack Overflow, but I enhanced 
#   it with a summary report.
#fld, f, fo = the folder, input file and output file. If an output file is not provided,
#   the input file is overwritten
#find, repl = the string to Find and its Replacement
def findReplace(fld, f, find, repl, fo=None):
    if fo is None:  #if an output file is not provided, overwrite the existing file
        fo = f
    now('Processing [ ' + f + ' ]')
    with open(os.path.join(fld, f), 'r') as file:  #read the file
        filedata = file.read()
    #Determine if there are any strings to replace, AND provide confirmation for the changes
    #   made, if any. Simply replacing the strings without providing a summary of the changes
    #   IS NOT a good idea - because unexpected and/or unintended outcomes could be missed.
    N = filedata.count(find)
    msg = ' matches. Nothing to do.' if N == 0 else ' match. Replacing' if N == 1  else ' matches. Replacing'
    print('\tSearching for "' + find + '". Found ' + str(N) + msg, end='')
    if N > 0:
        print(' with "' + repl + '":')
        filedata = filedata.replace(find, repl)  #replace  
        with open(os.path.join(fld, fo), 'w') as file:  #save
            file.write(filedata)
#        print('\tDone.')
    else:
        print('')

#-------------------------------------------------------------------------------

#br 2019-06
#Customize the default frequency report from pandas to include a header and also the 
#   Pct and Cumulative Pct (nicely formatted too).
#DF = pandas dataframe
#v = column name
#alpha = whether to sort alphabetically. By default, pandas value_counts() sorts by
#    frequency, descending. The sorting must be finalized before the Cumul % is calculated. 
def freq(DF, v, alpha=False):
    fd = DF[v].value_counts(dropna=False).rename_axis(v).reset_index(name='Freq')
    fd['Pct'] = fd['Freq'] / sum(fd['Freq'])  #calculate the %
    if alpha:
        fd = fd.sort_values(v)
    fd['CumulPct'] = fd['Pct'].cumsum()
    return reportFormat(fd)  #format to commas and %

#-------------------------------------------------------------------------------

'''
br 2023-09
This function calculates the distance between two points (lat & long) using the Great Circle Distance Formula.
The computed distance varies depending on the assumed radius of the Earth. In Miles, R = 3949.9 (polar), 
3963.1676 (equatorial), 3956.5 (average), 3960 (MapInfo). 
p1/2 = tuples with lat and long, in this assumed order
'''

def gcdf(p1, p2, R=3960):
   return 3960 * math.acos(math.sin(p1[0] / (180 / (4 * math.atan(1)))) *  math.sin(p2[0] / (180 / (4 * math.atan(1)))) + 
           math.cos(p1[0] / (180 / (4 * math.atan(1)))) * math.cos(p2[0] / (180 / (4 * math.atan(1)))) * 
           math.cos(p2[1] / (180 / (4 * math.atan(1))) - p1[1]/(180 / (4 * math.atan(1)))))
        
#-------------------------------------------------------------------------------

'''
br 2023-07: Produce file difference reports in two ways, with or without context. This
 is similar to the compareFiles() function, except it is meant for text reports (not HTML)
 consisting only of the differences + some context if desired.
The HTML reports are unsuitable for the large R and SAS function files, where out of thousands
 of lines of code, only a few (or just 1!) change at a time, usually. Thus practically
 the only option for record keeping is to show only the differences. The output is
 a list of lines of code, ready for printing.
'''

def getFileChanges(prodfld, prodfname, migfld, migfname, context=True):
   prev = os.path.join(prodfld, prodfname)  #the existing version
   upd = os.path.join(migfld, migfname)  #the new version
   with open(prev, 'r') as file:
       oldl = file.readlines()
   with open(upd, 'r') as file:
       newl = file.readlines()
   rep = []
   if context:
       diff = difflib.context_diff(oldl, newl, n=1)
       # diff = difflib.ndiff(oldl, newl)

       # if len(diff) == 0:
       #     rep.append('\n    No changes found for ' + migfname + '\n')
       # else:
       rep.append('='*20 + migfname + '='*20 + '\n')
       for d in diff:
           rep.append(d)
   else:
       diff = list(difflib.Differ().compare(oldl, newl))
       changes = [l for l in diff if l.startswith('+ ') or l.startswith('- ')]
       if len(changes) == 0:
           rep.append('\n    No changes found for ' + migfname + '\n')
       else:
           rep.append('='*20 + migfname + '='*20 + '\n')
           for c in changes:
               rep.append(c)
   return(rep)

#-------------------------------------------------------------------------------

#br 2019-06
#This function groups the functionality for retrieving files of interest from a folder (tree).
#   Return a list rather than a generator, as lists are more user-friendly.
#2020-11: Added the option to return file creation and modification times, and to exclude
#    files with certain extensions. Note that excl must be a tuple
#pattern = only file names matching this pattern will be returned
#recursive = if False, only the provided folder is scanned. If True, the entire tree is scanned.
#times = if True, the Creation and Modification times are included
#excl = a tuple with the file extensions to be excluded

def getFileList(fld, pattern='*', recursive=False, times=False, excl=None):
    if recursive:
        fileList = []
        #os.walk() is recursive
        for dirpath, dirnames, filenames in os.walk(fld):
            for f in filenames:
                if fnmatch.fnmatch(f, pattern):
                    fileList.append(os.path.join(dirpath, f))
    else:
        #os.scandir() looks only in the given folder and ignores subfolders
        fileList = [os.path.join(fld, entry.name) 
            for entry in os.scandir(fld)
                if entry.is_file() and fnmatch.fnmatch(entry.name, pattern)]
    before = len(fileList)
    print('The pattern ' + pattern + ' matched ' + str(before) + ' files.')
    
    #Exclude certain extensions. Endswith() is OK with mutiple values IF they are in a tuple
    if excl is not None:
        fileList = [f for f in fileList if not (f.endswith(excl))]
        after = len(fileList)
        print('    ' + str(before - after) + ' files were excluded.')
        
    #Get the creation and modification times if requested
    if times:
        fLst = []
        for f in fileList:
            tm = os.stat(f)
            fLst.append({'file': f, 'created': datetime.datetime.fromtimestamp(tm.st_ctime), 
                         'modified': datetime.datetime.fromtimestamp(tm.st_mtime)})
        return fLst
    else:
        return fileList

#-------------------------------------------------------------------------------

#2019-03: This function returns the total size in bytes of a given folder and all of its
#   contents. This os.walk() version was measured to run about 30% faster than a previous 
#   version based on the os.listdir() approach.
#2019-04: Added a "count" argument. If True, then file counts are printed for large subfolders
#   prior to scanning. This is helpful when walking through massive hierarchies (e.g. many
#   subfolders with thousands of files each), when in the absence of a progress indicator, it
#   looks as if the script is stuck for good and will not finish.
#   Also, return the number of files as well, in addition to the number of bytes.
def getFolderSize(folder, count=False):
    size = 0
    files = 0
    for dirpath, dirnames, filenames in os.walk(folder):
        files += len(filenames)
        #If file counts are requested, print only the large subfolders. 
        if count:
            N = len(filenames)
            if N >= 500:
                now('    large subfolder: ' + str(N) + ' files in ' + dirpath)
        for f in filenames:
            fp = os.path.join(dirpath, f)
            try:  #this can fail due to odd names, just-deleted files and so on
                size += os.path.getsize(fp)
            except:
                print('Scanning error: file\n' + fp + '\nwas ignored.')
    return (size, files)

#-------------------------------------------------------------------------------

#2019-03: Return the owner of a folder (or file) on Windows. The os.stat().st_uid approach
#   does not work - all IDs returned are 0. The Windows Security API must be used instead.
def getOwner(folder):
    try:
        sd = win32security.GetFileSecurity(folder, win32security.OWNER_SECURITY_INFORMATION)
        owner_sid = sd.GetSecurityDescriptorOwner()
        owner, domain, type = win32security.LookupAccountSid(None, owner_sid)
    except:
        owner = 'NotFound'
    return owner

#-------------------------------------------------------------------------------

#2023-03: Download a given url to disk
def getPage(link, fname,
            headers={'User-Agent': 'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:80.0) Gecko/20100101 Firefox/80.0'}):
    page = requests.get(link, stream=False, verify=False, headers=headers)
    with open(fname, "wb") as html: 
        for chunk in page.iter_content(chunk_size=1024): 
            if chunk: 
                 html.write(chunk) 
                 
#-------------------------------------------------------------------------------

#2023-03: This function downloads a file from the Internet (or loads it from disk) and
#   returns a dictionary with the links + the parsed BeautifulSoup object
#TO DO: replace the file below with a temporary one
def getHtml(url, download=True, 
            headers={'User-Agent': 'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:80.0) Gecko/20100101 Firefox/80.0'}):
    if download:
        tmp = "/Users/br/1data/economist/tmp.html"
        getPage(url, tmp, headers=headers)
    else:
        tmp = url
    #read the file and parse the HTML
    with open(tmp, "r", encoding="UTF-8") as fp:   
        soup = BeautifulSoup(fp, "html.parser")
    links = []
    for lk in soup.find_all('a', href=True):
        links.append(lk['href'])
    return {'links': links, 'soup': soup}                 

#-------------------------------------------------------------------------------

#2019-03: Retrieve the full user name on Windows, given the user ID (u00...)
#   In the absence of the pywin32 or ldap libraries, this is done by running
#   a shell subprocess, whose output is then parsed for the content desired.
#   For efficiency, it is best to store this data in a file somewhere, so that
#   the user info is queried only if not known.
def getUserName(user):
    p = subprocess.Popen(
        'net user %s /domain' % user,
        stdout=subprocess.PIPE, stderr=subprocess.PIPE
    )
    info, err = p.stdout.read(), p.stderr.read()
    info = info.decode("utf-8")  #convert from byte to string
    full_name = re.findall(r'Full Name\s+(.*\S)', info)
    title = re.findall(r'Comment\s+(.*\S)', info)
    if len(full_name) > 0:
        return (full_name[0], title[0])
    else:
        return('', '')
##getUserName('u0030286')

#-------------------------------------------------------------------------------

#br 2019-05
#This function is similar to HEAD from Linux/R, and also reports the total number of lines in a file
#filename = the full name including the path
#L = how many lines to print
#lc = whether to count the lines. This significantly increases the processing time for large files,
#   as the entire file needs to be read.
def head(filename, L=6, lc=False):
    if lc:
        N = lineCount(filename)
        print('====== ' + os.path.basename(filename) + ': the First [', L, ' out of', f'{N:,d}', '] lines ======')
    else:
        print('====== ' + os.path.basename(filename) + ': the First [', L, '] lines ======')
    with open(filename) as f:
        for line in islice(f, L):
            print(line.rstrip('\n'))  #remove blank lines between lines

#-------------------------------------------------------------------------------

#br 2019-05: Count the number of lines in a given file. This is based on a solution that
#   I found on StackOverflow, and which was timed there to be very efficient.        
#filename = the full name including the path
def lineCount(filename):
    f = open(filename, 'rb')
    bufgen = takewhile(lambda x: x, (f.raw.read(1024*1024) for _ in repeat(None)))
    return sum( buf.count(b'\n') for buf in bufgen )

#-------------------------------------------------------------------------------

#br 2019-03: Print a given message with the current time in a given format
def now(message = '', fmt="%H:%M:%S"):
    ctime = datetime.datetime.now().strftime(fmt)
    print(message + ' ' + ctime)

#-------------------------------------------------------------------------------

#br 2019-07
#Convert a path from Windows Explorer to the format expected by Python
def path(x=None):
    fld = input('Enter the path: ')
    fld = fld.replace(os.sep, '/')
    print(fld)

#-------------------------------------------------------------------------------

#br 2024-06: Parse a given syntax file and build a dictionary of functions and their names
def parseFunc(fileName, fsep='#' + 79*'-'):
   lines = readFile(fileName, lines=False)
   #Divide into individual functions. Filter() is meant to remove the empty strings that may result after splitting
   func = list(filter(None, lines.split(fsep)))
   out = {}
   for fun in func:
       tmp = list(filter(None, fun.split('\n')))
       fline = which(tmp, '<- function', index=False)
       if len(fline) > 0:
            FN = fline[0].split('<-')[0].strip()
            out[FN] = fun
   return out

#-------------------------------------------------------------------------------

#br 2020-07: Import/read a file either line by line, or as a string

def readFile(myfile, lines=True):
    with open(myfile, 'r', encoding='utf8') as file:
        if lines:
            fl = file.readlines()
        else:
            fl = file.read()
    return fl

#-------------------------------------------------------------------------------

#br 2019-06
#Given a pandas dataframe, apply formats to its columns in a programatic fashion. This is
#   only meant to be used on smaller dataframes or reports, because when the formats
#   are applied the columns are converted to text. Strange but true: I did not find another 
#   easy way to apply custom formatting to pandas columns while keeping them numeric.
#DF = a pandas dataframe
def reportFormat(DF):
    #Build a dictionary with the formats to apply. Columns with 'Pct' and 'Bal' in their
    #   names are formatted as % and $. Integer columns are formatted with , .
    fmtd = {}
    for v in DF.select_dtypes(exclude=['object']).columns.tolist():  #the numeric columns
#        print(DF[v].dtype)
        if 'Pct' in v:  #percent
            fmtd[v] = "{:,.1%}"
        elif 'Bal' in v:  #currency
            fmtd[v] = "${:,.2f}"
        elif 'int' in str(DF[v].dtype):  #integer
            fmtd[v] = "{:,.0f}"
    #Apply the formats. NB: This converts to text, so is only meant for final reports
    for key, value in fmtd.items():
        DF[key] = DF[key].apply(value.format)
    return DF

#-------------------------------------------------------------------------------

#br 2020-11
#This function is meant to quickly report the size and structure of a given object
def see(x):
    if type(x) is dict:
        print('This dictionary has ' + str(len(x)) + ' pairs. In no particular order, 3 pairs are:')
        #This was observed to fail with datetimes: Object of type datetime is not JSON serializable
#        print(json.dumps(dict(list(x.items())[:3]), indent = 2))
        subset = {k: v for i, (k, v) in enumerate(x.items()) if i < 3}
#        pprint.pprint(subset)
        print("\n".join("{}:\t{}".format(k, v) for k, v in subset.items()))
    if type(x) is list:
        print('This list has ' + str(len(x)) + ' elements. The first 3 are:')
        print(*x[:3], sep='\n')

#-------------------------------------------------------------------------------

#br 2020-11
#Take a given list and split into N pieces of roughly equal size
def split(lst, N):
    each = math.ceil(len(lst) / N)
    for i in range(0, len(lst), each):
        yield lst[i:i + each]

#-------------------------------------------------------------------------------

#br 2019-06: Adapted a function found on StackOverflow to return the last L lines of a file.
#   This is similar to the Linux TAIL command.
def tail(filename, L=5, buffer=io.DEFAULT_BUFFER_SIZE, lc=False):
    if lc:
        N = lineCount(filename)
        print('====== ' + os.path.basename(filename) + ': the Last [', L, ' out of', f'{N:,d}', '] lines ======')
    else:
        print('====== ' + os.path.basename(filename) + ': the Last [', L, '] lines ======')
    last_lines = []  #place holder
    block_counter = -1  #go backwards 1 buffer from the end
    #Use seek() to move the pointer at 1 buffer before end of the file (os.SEEK_END = 2).
    #   If readlines() returns fewer than L lines, then increase the buffer counter and try again.
    f = open(filename)
    while len(last_lines) < L:
        try:
            f.seek(block_counter * buffer, os.SEEK_END)
        except IOError:  #small files, not enough lines found
            f.seek(0)  #go to beginning of file
            last_lines = f.readlines()
            break
        last_lines = f.readlines()
        block_counter -= 1  #If not enough lines were found, read one more buffer
    for ll in last_lines[-L:]:
        print(ll.rstrip('\n'))  #remove blank lines between lines

#-------------------------------------------------------------------------------

#br 2023-08
#Save some typing when converting strings to date. Optionally, format those dates as needed
def todate(dateString, infmt, outfmt=None):
   asDate = datetime.datetime.strptime(dateString, infmt)
   if outfmt == None:
       return asDate
   else:
       return datetime.datetime.strftime(asDate, outfmt)

#-------------------------------------------------------------------------------

#2024-07: Merge a file with function updates into an existing master file. This is done by (1) adding the
#   brand-new functions, and (2) replacing the existing functions with copies from the Update file.
# currFN = file name (full path) of the current master file
# updFN = file name of the update file
def updFuncFile(currFN, updFN, backup='/Users/br/docs/prior_versions', funcsep = '#' + 79*'-'):
    current = parseFunc(currFN)
    updates = parseFunc(updFN)
    
    nochanges = [f for f in [*current] if f not in [*updates]]  #to keep as-is
    modnew = [*updates]  #to add or replace
    print('Found the following updates and/or additions:')
    print([*modnew], sep=', ')

    new = {}
    for f in nochanges:
        new[f] = current[f]
    for f in modnew:
        new[f] = updates[f]
    # new = sorted(new, key=str.lower)
    
    #save a copy of the existing master file, just in case
    today = datetime.datetime.now().strftime('_%Y%m%d')
    bup = os.path.join(backup, pathlib.Path(currFN).stem + today + '.txt')
    shutil.copy2(currFN, bup)  #use copy2() to preserve file attributes
    print('Copied the existing file as ' + bup + ', for backup')
    
    #overwrite the existing file with the new version
    keys = sorted([*new], key=str.lower)
    with open(currFN, 'w') as f:
       for k in keys:
           f.writelines(new[k])
           f.writelines(funcsep)
    print('Overwrote the existing file. All done')

#-------------------------------------------------------------------------------

#br 2019-11
#Search a list of strings for a certain pattern, and return either the indices or the matches
#   first = whether to return the first match only
#   index = whether to return the indices, or the matched strings

def which(lst, pattern, first=False, index=True):
    out = []
    for i, s in enumerate(lst):
        if s.find(pattern) != -1:
            if index:
                out.append(i)
            else:
                out.append(s)
            if first:
                break
    return out

#-------------------------------------------------------------------------------

