#!/usr/bin/perl

# Documentation follows at the __END__, and can be read with `perldoc Ptml'.

(undef)='

    $Id: Ptml.pm,v 1.5.1.8 1999/10/06 04:23:25 malcolm Exp malcolm $

    Ptml.pm is a Perl module to merge/format/render a PTML template file.
    Copyright (C) 1999 Malcolm Dew-Jones.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

    Malcolm Dew-Jones can be reached at 73312.2317@compuserve.com 
    or yf110@freenet.victoria.bc.ca, or via snail mail 
       C/O 63 Dock Street, Victoria B.C. Canada V8V 1X6
';

my $_PtmlModule;
($_PtmlModule=(caller(0))[6])=~s/[:\/\\]/::/g;
$_PtmlModule=~s/\.[pP][mM]$//;

my $_Ptmlshowing        =1;
my $_Ptmldefining_subroutine=0;
my $_Ptmlsubroutines    ={};
my $_Ptmlsections       ={};
my $_Ptmlembedding      =1;
my $_PtmlAllowComments  =1;
my @_Ptmltells          =();
my @_PtmlIFs            =();
my %_PtmlLineEdits      =();
my $_Ptml_set_verify    =0;
my $_Ptmlskip_loop      =0;

my $_PtmlInDent         ='(?:<BR>){0,1}\s*';
my $_PtmlSetVerTag      ='<BR>::';

sub Text::Template::Ptml::scanthruPtml #(INFILE,OUTFILE,position)
{  
   my ($_PtmlINFILE) = shift;
   my ($_PtmlOUTFILE) = shift || \*STDOUT;
   my ($_Ptmlposition)= shift || 0;

   seek $_PtmlINFILE , $_Ptmlposition , 0;

   print $_PtmlOUTFILE 
   "${_PtmlSetVerTag}scanthruPtml at $_Ptmlposition\n" 
       if $_Ptml_set_verify;

   while (<$_PtmlINFILE>)
   {   
       print $_PtmlOUTFILE "${_PtmlSetVerTag}",$_  if $_Ptml_set_verify;

       if ((not m/^$_PtmlInDent\./io) and ($_Ptmldefining_subroutine <= 0) 
           and $_Ptmlshowing and @_PtmlIFs <= 0)
       {   # automatically show anything that can't be a command line 
           # unless not showing, or defining subroutine, 
           # or in the depths of an IF
           goto NORMAL_OUTPUT;
       }

       next if $_PtmlAllowComments and m/^$_PtmlInDent\.#/io ;    # skip comments

       if (m/^$_PtmlInDent\.SECTION\s+(\S.*)/io)
       {   # start of section, is it one we want?
           my (@_Ptmlnames) = split(' ',$1);
           print $_PtmlOUTFILE 
               "${_PtmlSetVerTag} sections ",join(',',@_Ptmlnames),"\n"  
               if $_Ptml_set_verify;
           my ($_Ptmlname);
           foreach $_Ptmlname (@_Ptmlnames)
           {   last if $_Ptmlshowing = $_Ptmlsections->{uc $_Ptmlname};
           }
           next;
       }elsif
           (m/^$_PtmlInDent\.END_SECTION/io)
       {   # end of section, display rest of file by default
           $_Ptmlshowing = 1;
           next;
       }
       next if not $_Ptmlshowing;
       
       if (m/^$_PtmlInDent\.SUBROUTINE\s+(\S+)\s*(.*)/io)
       {   $_Ptmldefining_subroutine++;
           my ($_Ptmlsubroutine_name) = $1;
           my ($_Ptmlsubroutine_params) = $2 || '@argv';
           my ($_Ptmlsubroutine_tell) = tell $_PtmlINFILE;
           my ($_Ptmlsubroutine) = 
           "\$_Ptmlsubroutines{$_Ptmlsubroutine_name} = \n".
           "sub 
           {
               if (\$_Ptml_set_verify ) 
               { my \$_Ptmli=1; 
                 foreach (\@_) { print \$_PtmlOUTFILE \"arg[\@{[\$_Ptmli++]}]=\$_\n\";}
               }
               local ($_Ptmlsubroutine_params) = \@_;
               my (\$_Ptmlcaller_tell) = tell \$_PtmlINFILE;
               Text::Template::Ptml::scanthruPtml(\$_PtmlINFILE,\$_PtmlOUTFILE,$_Ptmlsubroutine_tell);
               seek \$_PtmlINFILE , \$_Ptmlcaller_tell , 0;
           }\n";
           print $_PtmlOUTFILE "${_PtmlSetVerTag}",$_Ptmlsubroutine if $_Ptml_set_verify;
           eval "$_Ptmlsubroutine";
           print $_PtmlOUTFILE $_,$@ if $@;
       }elsif
          ($_Ptmldefining_subroutine >0 and 
           m/^$_PtmlInDent\.END_SUBROUTINE/io)
       {   $_Ptmldefining_subroutine --;
           next;
       }
       
       next if $_Ptmldefining_subroutine > 0;

       if (@_PtmlIFs >0)                    # if we're in an IF (may fall thru)
       {   if (m/^$_PtmlInDent\.ELSE/io)
           {
               $_PtmlIFs[$#_PtmlIFs] = -1 * $_PtmlIFs[$#_PtmlIFs]; # toggle showing/notshowing
               next;
           }elsif
               (m/^$_PtmlInDent\.ELSIF\s+(\S.*)/io)
           {   my ($_Ptmlexpr) = $1;
               next if $_PtmlIFs[$#_PtmlIFs] == 0;       # not showing 
               next if --$_PtmlIFs[$#_PtmlIFs] == 0;     # no longer showing 
               $_PtmlIFs[$#_PtmlIFs] = eval($_Ptmlexpr)?1:-1; # true=positive, false=negative
               print $_PtmlOUTFILE $_,$@ if $@;
               next;
           }elsif
               (m/^$_PtmlInDent\.END_IF/io)
           {   pop @_PtmlIFs;
               next;
           }elsif
               ($_PtmlIFs[$#_PtmlIFs] <= 0)              # in IF, are we IF-not-showing?
           {   # recognize IF levels even if not displaying
               push @_PtmlIFs , 0 if (m/^$_PtmlInDent\.IF\s+\S.*/io);
               next;
           }
           # ELSE FALL THRU
       }

       if (m/^$_PtmlInDent\.IF\s+(\S.*)/io)
       {   # IF (expression)
           my ($_Ptmlexpr) = $1;
           push @_PtmlIFs , eval($_Ptmlexpr)?1:-1; # true=positive, false=negative
           print $_PtmlOUTFILE $_,$@ if $@;
       }elsif
           (m/^$_PtmlInDent\.EMBEDDING/io)
       {   # looking for embedding commands
           $_Ptmlembedding = 1;
       }elsif
           (m/^$_PtmlInDent\.END_EMBEDDING/io)
       {   # stop looking for embedding commands
           $_Ptmlembedding = 0;
       }elsif
           (m/^$_PtmlInDent\.EDIT_LINES\s+(\S.*)/io)
       {   # various line editing keywords are available
           my (@_Ptmlnames) = split(' ',$1);
           my ($_Ptmlname);
           foreach $_Ptmlname (@_Ptmlnames)
           {   $_PtmlLineEdits{uc $_Ptmlname}=1;
           }
       }elsif
           (m/^$_PtmlInDent\.END_EDIT_LINES/io)
       {   # stop doing any line editing
           %_PtmlLineEdits = ();
       }elsif
           (m/^$_PtmlInDent\.END_LOOP/io and @_Ptmltells > 0)
       {   # exit foreach or while loop if in a loop
           last ;   
       }elsif
           (m/^$_PtmlInDent\.(FOREACH|WHILE|FOR)\s+(\S.*)/io)
       {   my ($_Ptmlforwhile_clause) = $2;
           my ($_Ptmlforwhile) = lc $1;
           my (@_PtmlsaveIFs) = @_PtmlIFs;
           my $_Ptmlat_least_once = 0;
           push @_Ptmltells , tell $_PtmlINFILE;
           eval
           "   $_Ptmlforwhile $_Ptmlforwhile_clause 
               {   \$_Ptmlat_least_once = 1;
                   \@_PtmlIFs = \@_PtmlsaveIFs;  # restore state of IFs each time
                   Text::Template::Ptml::scanthruPtml(\$_PtmlINFILE,\$_PtmlOUTFILE,$_Ptmltells[$#_Ptmltells]);
               }
           " if $_Ptmlskip_loop <=0;
           print $_PtmlOUTFILE $_,$@ if $@;
           if (not $_Ptmlat_least_once)
           {   $_Ptmlskip_loop ++;
               Text::Template::Ptml::scanthruPtml($_PtmlINFILE,$_PtmlOUTFILE,$_Ptmltells[$#_Ptmltells]);
               $_Ptmlskip_loop --;
           }
           pop @_Ptmltells;
       }elsif
           (m/^$_PtmlInDent\.INCLUDE\s+(\S.*)/io)
       {   my ($_Ptmlfilename) = $1;
           my (@_PtmlsaveIFs) = @_PtmlIFs;
           my (@_Ptmlsave_tells) = @_Ptmltells;
           local *_PtmlFileSym ;
           eval
           "   open( _PtmlFileSym , \"\<$_Ptmlfilename\" ) or die \$!; 
               Text::Template::Ptml::scanthruPtml(*_PtmlFileSym,\$_PtmlOUTFILE);
               # no close in case of defined SUBS
               # close FILE;
           ";
           print $_PtmlOUTFILE $_,$@ if $@;
           @_PtmlIFs    = @_PtmlsaveIFs;
           @_Ptmltells  = @_Ptmlsave_tells;
       }elsif
           (m/^$_PtmlInDent\.(?:END_SUBROUTINE|EXIT_SUBROUTINE)/io)
       {   last;
       }elsif
           (m/^$_PtmlInDent\.EVAL\s+(.*)/io)
       {   my ($_Ptmlstatements)= $1;
           eval $_Ptmlstatements;
           print $_PtmlOUTFILE $_,$@ if $@;
       }elsif
           (m/^$_PtmlInDent\.CALL\s+(\S+)(.*)/io)
       {   my ($_Ptmlsubroutine_name) = $1;
           my ($_Ptmlsubroutine_params) = $2;
           my (@_PtmlsaveIFs) = @_PtmlIFs;
           my (@_Ptmlsave_tells) = @_Ptmltells;
##+
print $_PtmlOUTFILE 
   "${_PtmlSetVerTag}\&{\$_Ptmlsubroutines{$_Ptmlsubroutine_name}}($_Ptmlsubroutine_params);\n"
   if $_Ptml_set_verify;
##+
           eval   "&{\$_Ptmlsubroutines{$_Ptmlsubroutine_name}}($_Ptmlsubroutine_params);";
           print $_PtmlOUTFILE $_,$@ if $@;
           @_PtmlIFs    = @_PtmlsaveIFs;
           @_Ptmltells  = @_Ptmlsave_tells;
       }else
       {   
NORMAL_OUTPUT:
           next if ($_Ptmlskip_loop > 0);   # inside a non-showing loop
           # various possible edits to the lines are available
           s/^[ \t]+\|?//o if $_PtmlLineEdits{TRIM};
           chomp           if $_PtmlLineEdits{JOIN};
           s/\\\n\Z//o     if $_PtmlLineEdits{JOINSOME};
           # look for {+ $var +} 

           print $_PtmlOUTFILE $_Ptml__
               if $_Ptml_set_verify and $_Ptmlembedding and 
                  ($_Ptml__=$_) =~ s/{\+\s*(.*?)\+}/$1/eg ;

           s/{\+\s*(.*?)\+}/$1/eeg if $_Ptmlembedding;
           print $_PtmlOUTFILE $@ if $@ and $_Ptml_set_verify;
           print $_PtmlOUTFILE $_;
       }
   }
}

my $_Ptml_import = sub 
{
   shift;
   my $callpkg = caller(1);
   @_ = qw{ PtmlPrint PtmlPrintMe PtmlMerge } if @_ == 0;
   eval "*${callpkg}::$_=*Text::Template::Ptml::$_" while $_ = shift @_;
};

*Text::Template::Ptml::import = $_Ptml_import;
*{"${_PtmlModule}::import"} = $_Ptml_import;


package Text::Template::Ptml;

$VERSION=(q{$Revision: 1.5.1.8 $}=~m/(\d+\.\d+)/)[0];

sub set_verify { $_Ptml_set_verify = @_ || 1; }
sub set_noverify { $_Ptml_set_verify = 0; }
sub are_verifying { $_Ptml_set_verify; }

sub PtmlMerge
{  local *INFILE = shift;
   local *OUTFILE = shift;
   my $section_list = shift || [];
   $_Ptmlshowing         = shift || 1;
   $_Ptmlembedding       = shift || 1;

   foreach (@{$section_list}) { $_Ptmlsections->{uc $_}=1; }

   scanthruPtml(\*INFILE,\*OUTFILE, tell INFILE);
}

sub PtmlPrint
{  my ($filename)   = shift;
   my $section_list = shift || [];
   $_Ptmlshowing         = shift || 1;
   $_Ptmlembedding       = shift || 1;

   foreach (@{$section_list}) { $_Ptmlsections->{uc $_}=1; }

   local *FILE;
   open( FILE , "<$filename" ) or die $!;
   scanthruPtml(\*FILE,\*STDOUT);
   close FILE;
}

sub PtmlPrintMe
{
   my $section_list      = shift || [];
   $_Ptmlshowing         = shift || 1;
   $_Ptmlembedding       = shift || 1;
   my $callpkg           = caller(1);

   foreach (@{$section_list}) { $_Ptmlsections->{uc $_}=1; }

   scanthruPtml(\*{"${callpkg}::DATA"},\*STDOUT,
                tell "${callpkg}::DATA");
}


# the '1' here returns TRUE to the 'require'r of this module
1;

__END__

This file Copyright (C) 1999 Malcolm Dew-Jones.

=head1 NAME

Ptml - Perl Template Merge Language (aka `HTML with a P')

=head1 SYNOPSIS

   Declaring the functions.
   
use Text::Template::Ptml;     OR

use Ptml;      (...depending on where Ptml.pm is installed.)


   Merging data into a template.

C<PtmlPrint( "templatefilename" [,\@section_names ] );>

C<PtmlPrintMe( [ \@section_names ] );>

C<PtmlMerge( *TEMPLATE , *OUTPUT [,\@section_names ] );>


   Debugging a template.

C<Text::Template::Ptml::set_verify();>

C<Text::Template::Ptml::set_noverify();>

C<Text::Template::Ptml::are_verifying();>


=head1 DESCRIPTION

PTML is a simple but full featured "language" used to merge Perl data
into templates.  Ptml.pm is the module that implements the merge.  

A template is some text which is stored in one or more files, or embedded in
the __DATA__ segment of your Perl program.  The template must be stored in a
file (as opposed, perhaps, to a pipe or a Perl variable).

The template includes text that is displayed verbatim, macros that are
replaced by values during the merge, and PTML language statements that
control the merge process.

PTML language statements look vaguely similar to RUNOFF commands, and macros
are snippets of Perl code bracketed on a single line by C<{+> and C<+}>.

The merge is tightly integrated with your Perl code - it accesses all your
Perl elements, data, subs, file handles, etc., exactly as if it were defined
as a regular subroutine within your program.  

Data for the merge is not explicitly passed to the template, but instead
consists of what ever Perl variables (or subroutine results) the template
decides to use.  It is the responsibility of the calling Perl code to have
prepared what ever data the template requires. (Though missing data will not
normally cause any problems.)

It is convenient to provide well structured data via one or more aptly named
hash arrays, but this is not required.

The merge procedure runs within the package of the I<first> module that
I<use>'s Ptml.  If Ptml is C<use>'d near the top of a typical Perl program
then the merge takes place in the C<main::> package.

=over 5

=head1 MODULE FUNCTIONS

=item PtmlPrint( "TemplateFilename" [,\@SECTION_NAMES ] )

Merge a file named I<TemplateFilename>.  I<SECTION_NAMES> is a reference to an
array of strings, each of which is the name of a section to be shown.
(Sections, as discussed below, are indicated with the .SECTION statement.)


=item PtmlPrintMe( [ \@SECTION_NAMES ] )

This function reads the template from the __DATA__ section of a Perl
program.  To be specific, its reads the __DATA__ section of the code
that invokes PtmlPrintMe.

I<SECTION_NAMES> is as documented above.

=item PtmlMerge( *TEMPLATE , *OUTPUT [,\@SECTION_NAMES ] )

I<TEMPLATE> is the file handle of an open template file.
I<OUTPUT> is the handle to which the data is `print'ed.  It can be any type of
handle that can be used by `print', such as a pipe to sendmail (hint hint).

I<SECTION_NAMES> is as documented above.

=item Text::Template::Ptml::set_verify()

=item Text::Template::Ptml::set_noverify()

=item Text::Template::Ptml::are_verifying()

Turn the set verify mode on or off, or test whether it's on or off.

=back

=head1 QUICK EXAMPLE

The following text is a short, introductory example to what a simple
template file might look like, if cat'd or TYPE'd to the screen.

   .SECTION NEW_USER
   Welcome to our BBS system.  There are {+$number_of_accounts+} 
   other users that have accounts here.  If you would like to join 
   then select the NEW USER option when it appears below.

   .SECTION RETURNING_USER
   Welcome back, {+$the_user+}, to our BBS system.

   .END_SECTION

   Right now there are {+$number_of_users+} other users logged on,
   and you can chat with them if you wish.
   
   They are...
   .FOREACH $user (@current_users)
       {+$user+}.  {+he_she_of($user)+} likes {+likes_of($user)+}, but 
                   hates {+dislikes_of($user)+}.
   .END_LOOP
   Press Enter to continue...


=head1 PTML MACROS

The text of a template can contain snippets of text that should be replaced
when the template is merged.  These are called I<macros>.

Each macro is bracketed by I<{+> and I<+}> .  This bracketing was chosen
because it will rarely clash with any text or macro contents.  The entire
macro must fit on a single line of the template.  

The contents of each macro is actually a snippet of Perl code that should
return the value to be embedded.  The simplest macro is a scalar string
variable.  However any amount of Perl code that will fit on a single line
can be used.

I find it useful to use Perl subs as macros to perform common formatting
tasks.  The file F<PtmlMisc.pl> contains various subs which I have found
useful on a variety of projects.  They are useful in their own
right, and also illustrate certain useful techniques.

The following 3 lines contain some simple examples of macros...

   1. The Perl variable $something has the value {+$something+}
   
   2. The current time as returned by localtime() is {+localtime()+}

   3. More complex Perl code can be used if desired... such as the 
      following code : {+$x==5?"X was 5":"X was not five"+}


=head1 PTML LANGUAGE STATEMENTS

=over 12

=item .SECTION name [name ...]

Indicates the start of a named section of the template.  A section can have
more than one name, in which case it will be formatted whenever any of the
indicated sections are being merged.  

Portions of the template that lie outside of any .SECTION or .END_SECTION
are always formatted.  Each new .SECTION ends any previous .SECTION.

=item .END_SECTION

Indicates the end of any previous .SECTION.

=item .SUBROUTINE SubroutineName  [arg1 , arg2 , ...]

Marks the beginning of a template subroutine.  The template text upto the
corresponding .END_SUBROUTINE is not printed when first encountered, but
instead is saved for later and formatted when a I<.CALL SubName> statement is
encountered with this subroutine name.  The arguments are made available as
perl variables for use as Ptml macros within the text of the subroutine.

Subroutine definitions may be nested, though there is no particular use
for this fact in this version of Ptml.

=item .EXIT_SUBROUTINE

Any currently formatting subroutine is exited.

=item .END_SUBROUTINE

This statement marks the end of the defintion of a subroutine.

=item .CALL SubroutineName parametre_expr

The merge process I<calls> the indicated subroutine.  This actually means
that the merge temporarily takes place starting at the text immediately
following the corresponding .SUBROUTINE statement.  The I<parametre_expr>
becomes the list of arguments in a Perl function call, so its syntax is
anything that Perl accepts.

=item .IF expr

=item .ELSIF expr

=item .ELSE

=item .END_IF

Standard .IF logic.  I<expr> is passed directly to Perl so its syntax is
anything that Perl accepts.  The .IF nesting is independent of the loop
nesting, so that statements such as the following are possible...

   .IF $usingDynamicList
       .WHILE ($next=GetNext())
   .ELSE
       .FOREACH $next (@staticList)
   .END_IF
           the next item is {+$next+}
       .END_LOOP

though I'm not suggesting you will want to use this capability very often.

=item .EMBEDDING

Turns on the embedding of macros.  Macro embedding is turned on by default,
but can be turned off with the .END_EMBEDING statement.

=item .END_EMBEDDING

Turns off the embedding of macros.  Macro embedding is turned on by default,
but can be turned off with the .END_EMBEDING statement.

=item        .EDIT_LINES [ TRIM | JOIN | JOINSOME ]

C<EDIT_LINES> provides several simple options to edit each line of template
input before it is used in the merge.

I<TRIM> removes any white space from the beginning of each line, plus if
the line begins with I<|> then removes that character as well. This is useful
to allow indenting of the text in a template to show the logic of the
template even though the final document is not to have the indenting.  It
also provides a technique to escape the initial . of Ptml commands, thus
allowing Ptml to generate Ptml.

I<JOIN> removes the trailing LF from each template line
before it is displayed, (and before macros on that line
are expanded).  This `joins' the output lines into a
single line. The input is still read one line at a time.

Note: You may wish to include one extra blank line I<after> the
joined lines to force a new line before further output.

   .EDIT_LINES JOIN
   These lines 
   Will be joined together.
   .END_EDIT_LINES
   --This line should be left blank!

I<JOINSOME> is simlar to I<JOIN>, but is only applied to lines with
a I<\> at the end.  The slash is removed as well as the LF.



=item .END_EDIT_LINES 

Turns off any .EDIT_LINES options previously set.  There is no way in this
version of Ptml to nest the use of EDIT_LINES options.

=item .EVAL expr

Passes I<expr> to the Perl C<eval> command.  It is the literal text of
I<expr> that is eval'd.  Variables embedded within the expression are not
expanded unless the expression includes code to force that expansion.

=item .INCLUDE filename

Merges the indicated file.  The filename can include Perl variables.  E.g.
statements such as the following are possible

   .INCLUDE $TemplateDir/Header.html

where $TemplateDir would be a Perl variable containing a directory name.

=item .FOREACH expr

=item .FOR expr

=item .WHILE expr

=item .END_LOOP 

Each loop keyword is translated directly into its obvious Perl equivalent.
I<expr> is passed directly to Perl so its syntax is anything that Perl
accepts.  The loop continues until the corresponding .END_LOOP is encountered. 

   e.g. .FOREACH $i (@SomeValues)
           ... text to repeatedly display goes here ...
        .END_LOOP 

   e.g. .FOR ($i=0;$i<4; $i++)
           ... text to repeatedly display goes here ...
        .END_LOOP 

   e.g. .WHILE ($i<4)
           ... text to repeatedly display goes here ...
        .END_LOOP 

Due to the independance of IF logic and the LOOP logic there is a semantic
bug.  The bug does not alter the text displayed by the merge, but care must
be taken if an IF statement inside a loop has side effects and depends on
the loop's logic.  Examples and solutions are shown in the BUGS section.


=back

=head1 INSTALLATION

Ptml.pm can be installed in any directory by simply copying it to that
directory.  The path specified in the C<use> statement must then refer to
the install directory.

   E.g. if Ptml.pm is installed in the same directory as your script

      use Ptml;
   
   E.g. if Ptml.pm is installed in a Utils directory

      use Utils::Ptml;

   E.g. if Ptml.pm is installed in the Text/Template directory of 
        the Perl installation
        
      use Text::Template::Ptml;

However, no matter where Ptml.pm is installed, it always uses the
Text::Template::Ptml name space.  

By default, all three merge subroutines are exported into your name space.
Alternatively, you can list which subs should be exported, as discussed in
the standard Perl documentation, except that the numerous capabilities of
the standard Exporter module are not available.  Ptml.pm allows you
just to list the names of the subs to be imported.

   E.g. use Ptml qw{PtmlPrint set_verify are_verifying};

   E.g. to prevent importing any of the Ptml routines
      
      use Ptml ();



=head1 SECURITY

The PTML file is as great a security risk as the Perl code which merges it.
While I do not believe that user data being merged _into_ a template poses an
_inherent_ security risk, it is certainly possible to write templates where
that would be the case.  (Examples shown below.)

ONE RULE IS ABSOLUTE - do not allow any untrusted data to be used as the
_source_ of the _template_.

Data is merged using evals, but the eval's actually act upon the literal
text of the Perl variable names that your program uses as `macros', not on
the data contained within the macros.

The following template snippet shows some dos and don'ts.  You can run this
snippet using `doPtml' if you wish by extracting the lines into a file.

=over 5 

=item -- Template snippet start --

Let's define two variables with potentially dangerous contents, just as if a
user was trying to weasel some nastiness into your template.  (These first
`.EVAL's are simply to make this self contained example work. In "real life"
the contents of the variables would come from a user, perhaps as an entry in
an HTML form.)

 .EVAL $ls_command1='`ls -l >> ls_output1`'
 .EVAL $ls_command2='`ls -l >> ls_output2`'

Note the evaluation quotes embedded in the strings.  Also note that
neither of the above .EVALS cause the commands to run because the string
uses ' quotes.

Now lets embed the value of the string in the template output.

$ls_command1 is the string {+$ls_command1+}. After this template is
merged you should look for the file `ls_output1'.  However you won't find
it. Embedding the string in this template does not cause the command to
run, even though the string contained `eval' quotes.

I wouldn't normally want to .EVAL any user input, but lets show that its
not inherently unsafe to do so!
 
 .EVAL $ls_command1
 
The ls command still didn't run.  The PTML EVAL is still acting on the
variable itself, not on its contents!

WHEREAS the following two template lines ARE DANGEROUS.  Note the
EXPLICIT DOUBLE EVALS.

     This embedding is dangerous, ouch! {+eval $ls_command2+}
     The following .EVAL is dangerous
     .EVAL eval $ls_command2

Afterwards you will find a file has been created - ls_output2, which
indicates that the second I<ls> comand, used in the double evals, ran.

=item -- Template snippet end --

=back

Counter examples are surely welcome.



=head1 BUGS

IF statement expressions contained within a loop being merged are always
evaluated even if the loop iterates zero times.  The documentation
below shows one possible work around.  (There are undoubtedly others.) If
an IF expression has no side effects then this issue can normally be
ignored as the result of the expression does not change the logic flow of
the template merging.

1. The following shows a benign occurence of the bug.

   e.g. .FOR ($i=1;$i<=$somevalue; $i++)
           .IF $i==1       (*) this is "always" evaluated at least once
               Start of list
           .END_IF
        .END_LOOP 

2. The following shows a dangerous occurence of the bug.  

   e.g. .FOR ($i=1;$i<=$number_of_files; $i++)
           .IF remove_file("File_Number_".$i)==$Err    (*) side effect
               Error removing File_Number_{+$i+}
           .END_IF
        .END_LOOP 


The line at (*) is evaluated at least once if that part of the
template is currently being merged.

If `$somevalue' is 0 then the loop iterates zero times and the text in the
loop is not displayed - *BUT* the line at (*) is evaluated once anyways. In
example 1 this makes no difference. However in example 2 the remove_file()
function would be called once to delete a file called "File_Number_".

The following example shows one way to protect against this bug if it's an
issue.

   e.g. #2
        .IF $somevalue>0
        .FOR ($i=0;$i<$somevalue; $i++)
           .IF $i==1       (*) this is NOT evaluated when $somevalue<=0
               Here we go
           .END_IF
        .END_LOOP 
        .END_IF

The outer IF/END_IF turns off the merge when the loop is going to iterate
zero times.  This prevents the IF at (*) from being evaluated.


=head1 AUTHOR

(C) Copyright 1999 Malcolm Dew-Jones.

Malcolm Dew-Jones, E<lt>F<73312.2317@compuserve.com>E<gt>.

=head1 LICENSE

Ptml is distributed under the terms of the GNU general license.

=head1 FILES

 Ptml.pm       module to be `require'd, and the pod documentation
 README        introduction and more documentation
 example.ptml  full examples of each statement
 example.data  used by examples.ptml
 doPtml        working example of routine to merge templates
 PtmlMisc.pl   useful `macro' functions
 
