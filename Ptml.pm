#!/usr/bin/perl

# Documentation follows at the __END__, and can be read with `perldoc Ptml'.

(undef)='

    $Id: Ptml.pm,v 1.4 1999/07/09 05:53:24 malcolm Exp malcolm $

    Ptml.pm is a PERL module to merge/format/render a PTML template file.
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


my $showing        =1;
my $defining_subroutine=0;
my $subroutines    ={};
my $sections       ={};
my $embedding      =1;
my $AllowComments  =1;
my @tells          =();
my @IFs            =();
my %LineEdits      =();
my $_set_verify    =0;

my $InDent         ='(?:<BR>){0,1}\s*';
my $SetVerTag      ='<BR>::';

sub Ptml::set_verify { $_set_verify = @_ || 1; }
sub Ptml::set_noverify { $_set_verify = 0; }
sub Ptml::are_verifying { $_set_verify; }

sub Ptml::scanthruPtml #(INFILE,OUTFILE,position)
{  my ($INFILE)  = shift;
   my ($OUTFILE) = shift || STDOUT;
   my ($position)= shift || 0;

   seek $INFILE , $position , 0;

   print $OUTFILE 
       "${SetVerTag}scanthruPtml([$INFILE],[$OUTFILE],[$position])\n"
       if $_set_verify;

   while (<$INFILE>)
   {   
       print $OUTFILE "${SetVerTag}",$_  if $_set_verify;

       if ((not m/^$InDent\./io) and ($defining_subroutine <= 0) 
           and $showing and @IFs <= 0)
       {   # automatically show anything that can't be a command line 
           # unless not showing, or defining subroutine, 
           # or in the depths of an IF
           goto NORMAL_OUTPUT;
       }

       next if $AllowComments and m/^$InDent\.#/io ;    # skip comments

       if (m/^$InDent\.SECTION\s+(\S.*)/io)
       {   # start of section, is it one we want?
           my (@names) = split(' ',$1);
           print $OUTFILE 
               "${SetVerTag} sections ",join(',',@names),"\n"  
               if $_set_verify;
           my ($name);
           foreach $name (@names)
           {   last if $showing = $sections->{uc $name};
           }
           next;
       }elsif
           (m/^$InDent\.END_SECTION/io)
       {   # end of section, display rest of file by default
           $showing = 1;
           next;
       }
       next if not $showing;
       
       if (m/^$InDent\.SUBROUTINE\s+(\S+)\s*(.*)/io)
       {   $defining_subroutine++;
           my ($subroutine_name) = $1;
           my ($subroutine_params) = $2 || '@argv';
           my ($subroutine_tell) = tell $INFILE;
           my ($subroutine) = 
           "\$subroutines{$subroutine_name} = \n".
           "sub 
           {
               if (\$_set_verify ) 
               { my \$i=1; 
                 foreach (\@_) { print \$OUTFILE \"arg[\@{[\$i++]}]=\$_\n\";}
               }
               local ($subroutine_params) = \@_;
               my (\$caller_tell) = tell \$INFILE;
               Ptml::scanthruPtml($INFILE,\$OUTFILE,$subroutine_tell);
               seek \$INFILE , \$caller_tell , 0;
           }\n";
           print $OUTFILE "${SetVerTag}",$subroutine if $_set_verify;
           eval "$subroutine";
           print $OUTFILE $_,$@ if $@;
       }elsif
          ($defining_subroutine >0 and 
           m/^$InDent\.END_SUBROUTINE/io)
       {   $defining_subroutine --;
           next;
       }
       
       next if $defining_subroutine > 0;

       if (@IFs >0)                    # if we're in an IF (may fall thru)
       {   if (m/^$InDent\.ELSE/io)
           {
               $IFs[$#IFs] = -1 * $IFs[$#IFs]; # toggle showing/notshowing
               next;
           }elsif
               (m/^$InDent\.ELSIF\s+(\S.*)/io)
           {   my ($expr) = $1;
               next if $IFs[$#IFs] == 0;       # not showing 
               next if --$IFs[$#IFs] == 0;     # no longer showing 
               $IFs[$#IFs] = eval($expr)?1:-1; # true=positive, false=negative
               print $OUTFILE $_,$@ if $@;
               next;
           }elsif
               (m/^$InDent\.END_IF/io)
           {   pop @IFs;
               next;
           }elsif
               ($IFs[$#IFs] <= 0)              # in IF, are we IF-not-showing?
           {   # recognize IF levels even if not displaying
               push @IFs , 0 if (m/^$InDent\.IF\s+\S.*/io);
               next;
           }
           # ELSE FALL THRU
       }

       if (m/^$InDent\.IF\s+(\S.*)/io)
       {   # IF (expression)
           my ($expr) = $1;
           push @IFs , eval($expr)?1:-1; # true=positive, false=negative
           print $OUTFILE $_,$@ if $@;
       }elsif
           (m/^$InDent\.EMBEDDING/io)
       {   # looking for embedding commands
           $embedding = 1;
       }elsif
           (m/^$InDent\.END_EMBEDDING/io)
       {   # stop looking for embedding commands
           $embedding = 0;
       }elsif
           (m/^$InDent\.EDIT_LINES\s+(\S.*)/io)
       {   # various line editing keywords are available
           my (@names) = split(' ',$1);
           my ($name);
           foreach $name (@names)
           {   $LineEdits{uc $name}=1;
           }
       }elsif
           (m/^$InDent\.END_EDIT_LINES/io)
       {   # stop doing any line editing
           %LineEdits = ();
       }elsif
           (m/^$InDent\.END_LOOP/io and @tells > 0)
       {   # exit foreach or while loop if in a loop
           last ;   
       }elsif
           (m/^$InDent\.(FOREACH|WHILE|FOR)\s+(\S.*)/io)
       {   my ($forwhile_clause) = $2;
           my ($forwhile) = lc $1;
           my (@saveIFs) = @IFs;
           push @tells , tell $INFILE;
           eval
           "   $forwhile $forwhile_clause 
               {   \@IFs = \@saveIFs;  # restore state of IFs each time
                   Ptml::scanthruPtml($INFILE,$OUTFILE,$tells[$#tells]);
               }
           ";
           print $OUTFILE $_,$@ if $@;
           pop @tells;
       }elsif
           (m/^$InDent\.INCLUDE\s+(\S.*)/io)
       {   my ($filename) = $1;
           my (@saveIFs) = @IFs;
           my (@save_tells) = @tells;
           my ($FileSym)= $INFILE.'_'; # we have to generate a file symbol
           eval
           "   open( $FileSym , \"\<$filename\" ) or die \$!; 
               Ptml::scanthruPtml($FileSym,\$OUTFILE);
               # no close in case of defined SUBS
               # close FILE;
           ";
           print $OUTFILE $_,$@ if $@;
           @IFs    = @saveIFs;
           @tells  = @save_tells;
       }elsif
           (m/^$InDent\.(?:END_SUBROUTINE|EXIT_SUBROUTINE)/io)
       {   last;
       }elsif
           (m/^$InDent\.EVAL\s+(.*)/io)
       {   my ($statements)= $1;
           eval $statements;
           print $OUTFILE $_,$@ if $@;
       }elsif
           (m/^$InDent\.CALL\s+(\S+)(.*)/io)
       {   my ($subroutine_name) = $1;
           my ($subroutine_params) = $2;
           my (@saveIFs) = @IFs;
           my (@save_tells) = @tells;
##+
print $OUTFILE 
   "${SetVerTag}\&{\$subroutines{$subroutine_name}}($subroutine_params);\n"
   if $_set_verify;
##+
           eval   "&{\$subroutines{$subroutine_name}}($subroutine_params);";
           print $OUTFILE $_,$@ if $@;
           @IFs    = @saveIFs;
           @tells  = @save_tells;
       }else
       {   
NORMAL_OUTPUT:
           # various possible edits to the lines are available
           s/^[ \t]+\|?//o if $LineEdits{TRIM};
           chomp           if $LineEdits{JOIN};
           s/\\\n\Z//o     if $LineEdits{JOINSOME};
           # look for {+ $var +} 
           if ($_set_verify)
           {   if ($embedding)
               {   undef $__;
                   ($__ = $_) =~ s/{\+\s*(.*?)\+}/$1/eg;
                   print $OUTFILE $__;
                   s/{\+\s*(.*?)\+}/$1/eeg;
               }
           }else
           {   s/{\+\s*(.*?)\+}/$1/eeg if $embedding;
           }
           print $OUTFILE $_;
       }
   }
}

sub Ptml::Print
{  my ($filename)   = shift;
   my $section_list = shift || [];
   $showing         = shift || 1;
   $embedding       = shift || 1;

   foreach (@{$section_list}) { $sections->{uc $_}=1; }

   local *Ptml::FILE;
   open( Ptml::FILE , "<$filename" ) or die $!;

   Ptml::scanthruPtml(Ptml::FILE,STDOUT);
   close Ptml::FILE;
}

sub Ptml::PrintMe
{
   my $section_list = shift || [];
   $showing         = shift || 1;
   $embedding       = shift || 1;

   foreach (@{$section_list}) { $sections->{uc $_}=1; }

   Ptml::scanthruPtml(DATA,STDOUT,tell DATA);
}

sub Ptml::Merge
{  my ($INFILE)  = shift;
   my ($OUTFILE) = shift;
   my $section_list = shift || [];
   $showing         = shift || 1;
   $embedding       = shift || 1;

   foreach (@{$section_list}) { $sections->{uc $_}=1; }

   Ptml::scanthruPtml($INFILE,$OUTFILE, tell $INFILE);
}
package Ptml;
$VERSION=(qw($Revision: 1.4 $))[1];

# the '1' here returns TRUE to the 'require'r of this module
1;

__END__

This file Copyright (C) 1999 Malcolm Dew-Jones.

=head1 NAME

Ptml - Perl Template Merge Language (aka `HTML with a P')

=head1 SYNOPSIS

require Ptml;

   Routines used to merge data into a template.

C<Ptml::Print( "templatefilename" [,\@section_names ] );>

C<Ptml::PrintMe( [ \@section_names ] );>

C<Ptml::Merge( TEMPLATE , OUTPUT [,\@section_names ] );>

   Routines useful when debugging templates.

C<Ptml::set_verify();>

C<Ptml::set_noverify();>

=head1 DESCRIPTION

Ptml is both a module and the simple language that it understands.  
You load
the module to be able to use the language.  The
language is used to control the merging of data into a template.  
PTML language statements are
embedded in the template, which must be a file (as opposed perhaps to a pipe
or a perl variable).  The template can be placed in its own file (or files),
or included as part of your main PERL module by being embedded in the
 __DATA__ segment of your code.

The Ptml module loads into your current package.  The template merging uses
PERL to provide all of its logic (surprise, surprise), and is tightly
integrated with your PERL code, having full access to most of your variables
and subroutines.

PTML uses RUNOFF like commands to control the merge proces, and allows the
embedding of `macros', which are really tiny PERL programs that equate to
the values to embed.

PTML is _not_ an extension of HTML.  PTML was designed this way.  In my
experience it is difficult to use standard HTML design tools to layout
templates that use extensions to HTML because the non-HTML tags disappear
from the display, leaving you to guess what the finished document will really
look like.  In contrast PTML `macros' often give a close approximation of
the final appearance of a document. Besides, I use PTML for
non-HTML tasks.

=over 5

=head1 MODULE FUNCTIONS

=item Ptml::Print( "TemplateFilename" [,\@SECTION_NAMES ] )

Merge a file named I<TemplateFilename>.  I<SECTION_NAMES> is a reference to an
array of strings, each of which is the name of a section to be shown.
(Sections in the template are indicated with the .SECTION statement.)

The data for the merge is not explicitly passed to the template, but instead
consists of what ever PERL variables (or subroutine results) the template
decides to use.  It is the responsibility of the calling PERL code to have
prepared what ever data the template requires. (Though missing data will not
normally cause any problems.)

It is convenient to provide well structured data via one or more aptly named
hash arrays, but this is not required.

=item Ptml::PrintMe( [ \@SECTION_NAMES ] )

This function reads the template from the __DATA__ section.  Otherwise it is
identical to Ptml::Print.

=item Ptml::Merge( TEMPLATE , OUTPUT [,\@SECTION_NAMES ] )

I<TEMPLATE> should be the file handle of the already opened template file.
I<OUTPUT> is the handle to which the data is `print'ed.  It can be any type of
handle that can be used by `print', such as a pipe to sendmail (hint hint).

=back

=head1 PTML MACROS

The text of a template can contain snippets of text that should be replaced
when the template is merged.  I call these I<macros>.

Each macro is bracketed by I<{+> and I<+}> .  This bracketing was chosen
because it will rarely clash with any text or macro contents.  The entire
macro must fit on a single line of the template.

The contents of each macro is actually a snippet of PERL code that should
return the value to be embedded.  The simplest macro is a scalar string
variable.  However any amount of PERL code that will fit on a single line
can be used.

I find it useful to use PERL subs as macros to perform common formatting
tasks.  The file F<PtmlMisc.pl> contains various subs which I have found
useful on a variety of projects.  They are useful in their own
right, and also illustrate certain useful techniques.

The following 3 lines contain some simple examples of macros...

   1. The PERL variable $something has the value {+$something+}
   
   2. The current time as returned by localtime() is {+localtime()+}

   3. More complex PERL code can be used if desired... such as the 
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
becomes the list of arguments in a PERL function call, so its syntax is
anything that PERL accepts.

=item .IF expr

=item .ELSIF expr

=item .ELSE

=item .END_IF

Standard .IF logic.  I<expr> is passed directly to PERL so its syntax is
anything that PERL accepts.  The .IF nesting is independent of the loop
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

Passes I<expr> to the PERL C<eval> command.  It is the literal text of
I<expr> that is eval'd.  Variables embedded within the expression are not
expanded unless the expression includes code to force that expansion.

=item .INCLUDE filename

Merges the indicated file.  The filename can include PERL variables.  E.g.
statements such as the following are possible

   .INCLUDE $TemplateDir/Header.html

where $TemplateDir would be a PERL variable containing a directory name.

=item .FOREACH expr

=item .FOR expr

=item .WHILE expr

=item .END_LOOP 

Each loop keyword is translated directly into its obvious PERL equivalent.
I<expr> is passed directly to PERL so its syntax is anything that PERL
accepts.  The loop continues until the corresponding .END_LOOP is encountered. 

   e.g. .FOREACH $i (@SomeValues)
           ... statements to merge go here ...
        .END_LOOP 

   e.g. .FOR ($i=0;$i<4; $i++)
           ... statements to merge go here ...
        .END_LOOP 

   e.g. .WHILE ($i<4)
           ... statements to merge go here ...
        .END_LOOP 

This version of Ptml has a bug such that a loop should always be made to
iterate at least once.  Otherwise the body of the loop will be formatted
anyways but outside of the loop. The BUGS section mentions some workarounds
to this. It has not normally been a problem for me, so I haven't fixed it
(yet).

=back


=head1 SECURITY

The PTML file is as great a security risk as the PERL code which merges it.
While I do not believe that user data being merged _into_ a template poses an
_inherent_ security risk, it is certainly possible to write templates where
that would be the case.  (Examples shown below.)

ONE RULE IS ABSOLUTE - do not allow any untrusted data to be used as the
_source_ of the _template_.

Data is merged using evals, but the eval's actually act upon the literal
text of the PERL variable names that your program uses as `macros', not on
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

A loop should always iterate at least once.  If not then the body of the
loop gets merged once anyway but no loop variables are set.  You will know
this is the bug if you see an .END_LOOP printed for no obvious reason.

There are several workarounds.  I normally structure my templates so that I
never try to format empty loops.  In fact I didn't even notice this bug for
almost a year because all my projects had checked for empty lists and used
SECTIONs to display nice messages explaining any lack of data.

An alternative work around is to wrap the loop in an .IF .END_IF block.

E.g.

   .IF @users == 0
      <H2>There are no users to list!</H2>
   .ELSE
      <TABLE>
      .FOREACH $user (@users)
         <TR><TD>{+$user+}</TD>
             <TD>is a user.</TD>
         </TR>
      .END_LOOP
      </TABLE>
   .END_IF

The third alternative is to ignore the problem.  Nothing much goes wrong
with the rest of the formatting, it's just that the output might be slightly
confusing when one non-existent item is formatted without data. In a quick
and dirty project that might be satisfactory.


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
 PtmlMsc.pl    useful `macro' functions
 
